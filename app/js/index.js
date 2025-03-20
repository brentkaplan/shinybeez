document.addEventListener('DOMContentLoaded', function () {
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
                    const bootstrapCollapse = new bootstrap.Collapse(collapseElement, {
                        toggle: false
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
        window.addEventListener('hashchange', function () {
            // Remove active class from all links
            document.querySelectorAll('.btn-toggle-nav a').forEach(link => {
                link.classList.remove('active');
            });

            // Add active class to current link
            const currentLink = document.querySelector(`.btn-toggle-nav a[href="${window.location.hash}"]`);
            if (currentLink) {
                currentLink.classList.add('active');
            }
        });


    });

document.addEventListener('shiny:inputchanged', function(event) {
          if (event.name === 'nav') {
            trackPage(event.value);
          }
        });
