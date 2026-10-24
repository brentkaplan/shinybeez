document.addEventListener('DOMContentLoaded', () => {
  // Get current hash
  const currentHash = window.location.hash;

  // If there's a hash, find and highlight the corresponding link
  if (currentHash) {
    const activeLink = document.querySelector(`.btn-toggle-nav a[href="${currentHash}"]`);
    if (activeLink) {
      // Add active class
      activeLink.classList.add('active');

      // Open the parent collapse
      const collapseElement = activeLink.closest('.collapse');
      if (collapseElement) {
        // eslint-disable-next-line no-undef
        const bootstrapCollapse = new bootstrap.Collapse(collapseElement, {
          toggle: false,
        });
        bootstrapCollapse.show();

        // Update aria-expanded on the button
        const toggleButton = document.querySelector(`[data-bs-target="#${collapseElement.id}"]`);
        if (toggleButton) {
          toggleButton.setAttribute('aria-expanded', 'true');
          toggleButton.classList.remove('collapsed');
        }
      }
    }
  }

  // Update active link when hash changes
  window.addEventListener('hashchange', () => {
    // Remove active class from all links
    document.querySelectorAll('.btn-toggle-nav a').forEach((link) => {
      link.classList.remove('active');
    });

    // Add active class to current link
    const currentLink = document.querySelector(`.btn-toggle-nav a[href="${window.location.hash}"]`);
    if (currentLink) {
      currentLink.classList.add('active');
    }
  });
});

// --- Consent: stored choice, platform default, and the Google Analytics loader ---
//
// R emits window.SHINYBEEZ = {gaId, platform, policy} in <head> (app/main.R). The choice is
// stored in localStorage (per browser, and per app in the Electron build). On connect the
// browser sends the stored choice, or the platform default when there is none, to the
// consent module (app/view/consent.R) as the root input `consent_level`. GA loads only at
// the `usage` level and only from here; R never emits the tag.

const CONSENT_KEY = 'shinybeez.consent';
const LEVELS = ['none', 'diagnostic', 'usage'];
const EU_ATLANTIC_ZONES = [
  'Atlantic/Reykjavik', 'Atlantic/Canary', 'Atlantic/Madeira', 'Atlantic/Azores', 'Atlantic/Faroe',
];

function config() {
  const cfg = window.SHINYBEEZ || {};
  return {
    gaId: typeof cfg.gaId === 'string' ? cfg.gaId : '',
    platform: cfg.platform === 'desktop' ? 'desktop' : 'web',
    policy: Number.isInteger(cfg.policy) ? cfg.policy : 1,
  };
}

// {level, policy, ts} or null. Storage may be blocked or throw (private modes); a throw,
// garbage, an unknown level or an older policy version all count as "no choice yet".
function readStoredConsent() {
  try {
    const raw = window.localStorage.getItem(CONSENT_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw);
    if (!parsed || !LEVELS.includes(parsed.level)) return null;
    if (!Number.isFinite(parsed.policy) || parsed.policy < config().policy) return null;
    return { level: parsed.level, policy: parsed.policy, ts: Number(parsed.ts) || 0 };
  } catch (e) {
    return null;
  }
}

function writeStoredConsent(level) {
  if (!LEVELS.includes(level)) return;
  try {
    window.localStorage.setItem(
      CONSENT_KEY,
      JSON.stringify({ level, policy: config().policy, ts: Date.now() }),
    );
  } catch (e) {
    // Best effort: a blocked store means the banner returns next visit.
  }
}

// EU/EEA/UK/CH visitors get the diagnostic default. Judged from the browser's IANA time
// zone: every Europe/* zone plus the EU's Atlantic islands; unknown counts as EU.
function isEuRegion() {
  let tz = '';
  try {
    tz = Intl.DateTimeFormat().resolvedOptions().timeZone || '';
  } catch (e) {
    tz = '';
  }
  if (!tz) return true;
  return tz.startsWith('Europe/') || EU_ATLANTIC_ZONES.includes(tz);
}

function defaultConsent() {
  const eu = isEuRegion();
  return { level: eu ? 'diagnostic' : 'usage', source: eu ? 'default_eu' : 'default' };
}

let gaLoaded = false;

function gaDisableKey() {
  return `ga-disable-${config().gaId}`;
}

function pageLocation(pageName) {
  if (config().platform === 'desktop') {
    // The Electron page origin is a localhost port; give GA a stable synthetic location.
    return `https://shinybeez.app/desktop/${pageName}`;
  }
  return window.location.href;
}

function currentNav() {
  const nav = document.querySelector('#nav .nav-link.active');
  return (nav && nav.getAttribute('data-value')) || 'Welcome';
}

function trackPage(pageName) {
  if (!gaLoaded || typeof window.gtag !== 'function') return;
  window.gtag('event', 'page_view', {
    page_title: pageName,
    page_location: pageLocation(pageName),
    page_referrer: config().platform === 'desktop' ? '' : document.referrer,
  });
}

function loadGA() {
  const { gaId } = config();
  if (!gaId) return;
  window[gaDisableKey()] = false;
  if (gaLoaded) return;
  window.dataLayer = window.dataLayer || [];
  window.gtag = window.gtag || function gtag() {
    window.dataLayer.push(arguments); // eslint-disable-line prefer-rest-params
  };
  window.gtag('js', new Date());
  window.gtag('config', gaId, { send_page_view: false });
  const script = document.createElement('script');
  script.async = true;
  script.src = `https://www.googletagmanager.com/gtag/js?id=${encodeURIComponent(gaId)}`;
  document.head.appendChild(script);
  gaLoaded = true;
  trackPage(currentNav());
}

function deleteGaCookies() {
  const names = document.cookie.split(';')
    .map((c) => c.trim().split('=')[0])
    .filter((name) => name.startsWith('_ga'));
  const host = window.location.hostname;
  const parent = host.split('.').slice(-2).join('.');
  names.forEach((name) => {
    [host, parent, ''].forEach((domain) => {
      const scope = domain ? `; domain=${domain}` : '';
      document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/${scope}`;
    });
  });
}

function disableGA() {
  if (!config().gaId) return;
  window[gaDisableKey()] = true;
  if (typeof window.gtag === 'function') {
    window.gtag('consent', 'update', { analytics_storage: 'denied' });
  }
  deleteGaCookies();
}

function applyGA(level) {
  if (level === 'usage') {
    loadGA();
  } else {
    disableGA();
  }
}

function applyLocally(level) {
  writeStoredConsent(level);
  applyGA(level);
}

// Handshake: tell R the session's level as soon as the connection is up. Shiny fires its
// lifecycle events through jQuery, so they must be bound with jQuery: a native
// addEventListener never sees them.
$(document).on('shiny:connected', () => {
  const stored = readStoredConsent();
  const payload = stored
    ? { level: stored.level, source: 'stored', ts: stored.ts }
    : { ...defaultConsent(), ts: Date.now() };
  applyGA(payload.level);
  Shiny.setInputValue('consent_level', { ...payload, policy: config().policy }, { priority: 'event' });
});

// R confirms a choice (banner button or settings modal): persist it and apply GA.
if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
  Shiny.addCustomMessageHandler('shinybeez-consent', (msg) => {
    if (msg && LEVELS.includes(msg.level)) applyLocally(msg.level);
  });
}

// Banner buttons persist the choice at click time, before the server round trip, so a
// reconnect in between cannot resurrect the old stored value.
document.addEventListener('click', (event) => {
  const btn = event.target.closest('[data-consent-level]');
  if (!btn) return;
  const level = btn.getAttribute('data-consent-level');
  if (LEVELS.includes(level)) applyLocally(level);
});

// "Change your choice" on the Welcome page opens the settings modal.
document.addEventListener('click', (event) => {
  if (!event.target.closest('#consent-open-welcome') || !window.Shiny) return;
  Shiny.setInputValue('consent_open', Date.now(), { priority: 'event' });
});

$(document).on('shiny:inputchanged', (event) => {
  if (event.name === 'nav') {
    trackPage(event.value);
  }
});

// --- Telemetry: Track client-side actions invisible to R server ---

// Track DT table export button clicks (Copy, Print, CSV, Excel, PDF)
// DT Buttons with Bootstrap styling use .dt-buttons container, not .dt-button per button
document.addEventListener('click', (event) => {
  const btn = event.target.closest('.dt-buttons button');
  if (!btn || !window.Shiny) return;

  const wrapper = btn.closest('.dataTables_wrapper');
  const tableEl = wrapper ? wrapper.querySelector('table') : null;
  const tableId = tableEl ? tableEl.id : 'unknown';
  const exportType = btn.textContent.trim().toLowerCase();

  // eslint-disable-next-line no-undef
  Shiny.setInputValue('_telemetry_dt_export', {
    export_type: exportType,
    table_id: tableId,
    ts: Date.now(),
  }, { priority: 'event' });
});

// Track plot/file downloads (esquisse and other <a download> links)
document.addEventListener('click', (event) => {
  const link = event.target.closest('a[download]');
  if (!link || !window.Shiny) return;

  const filename = link.getAttribute('download') || '';
  // eslint-disable-next-line no-undef
  Shiny.setInputValue('_telemetry_plot_download', {
    filename,
    format: filename.split('.').pop() || 'unknown',
    id: link.id || '',
    ts: Date.now(),
  }, { priority: 'event' });
});

// Capture uncaught JS errors
window.addEventListener('error', (event) => {
  // Chromium's benign ResizeObserver loop warning (fired by the plot export
  // dropdown) is not an app error; keep it out of the error table.
  if (/^ResizeObserver loop/.test(event.message || '')) return;
  if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()) {
    // eslint-disable-next-line no-undef
    Shiny.setInputValue('_telemetry_client_error', {
      message: event.message || 'Unknown error',
      source: event.filename || '',
      line: event.lineno || 0,
      col: event.colno || 0,
      ts: Date.now(),
    }, { priority: 'event' });
  }
});

// Capture unhandled promise rejections
window.addEventListener('unhandledrejection', (event) => {
  if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()) {
    // eslint-disable-next-line no-undef
    Shiny.setInputValue('_telemetry_client_error', {
      message: event.reason ? String(event.reason) : 'Unhandled promise rejection',
      source: 'promise',
      ts: Date.now(),
    }, { priority: 'event' });
  }
});
