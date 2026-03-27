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

document.addEventListener('shiny:inputchanged', (event) => {
  if (event.name === 'nav') {
    trackPage(event.value); // eslint-disable-line no-undef
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
    ts: Date.now(),
  }, { priority: 'event' });
});

// Capture uncaught JS errors
window.addEventListener('error', (event) => {
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
