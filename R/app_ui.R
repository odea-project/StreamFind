#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
app_ui <- function(request) {
  init_project_path <- golem::get_golem_options("projectPath")
  boot_loading <- !is.null(init_project_path) && !is.na(init_project_path) && dir.exists(init_project_path)

  shiny::tagList(
    golem_add_external_resources(),
    htmltools::div(
      id = "sf-app", class = "sf-light",
      # ---- Top bar (logo + horizontal nav + right controls) ----
      htmltools::div(
        id = "sf-topbar",
        htmltools::div(
          id = "sf-logo",
          htmltools::tags$img(
            src = "www/logo_StreamFind.png"
          )
        ),
        # ---- Horizontal navigation (replaces sidebar) ----
        htmltools::div(
          id = "sf-nav",
          # Project
          htmltools::div(class = "sf-nav-group active", `data-tab` = "project",
            htmltools::tags$button(class = "sf-nav-btn active", `data-tab` = "project", title = "Project",
              "Project")
          ),
          # Analyses
          htmltools::div(class = "sf-nav-group", `data-tab` = "analyses",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "analyses", title = "Analyses",
              "Analyses")
          ),
          # Explorer + sub-pages
          htmltools::div(class = "sf-nav-group", `data-tab` = "explorer",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "explorer", title = "Explorer",
              "Explorer"),
            htmltools::div(
              class = "sf-sub-menu",
              htmltools::tags$button(class = "sf-sub-btn", `data-tab` = "explorer",
                `data-subtab` = "spectra", title = "Spectra", "Spectra"),
              htmltools::tags$button(class = "sf-sub-btn", `data-tab` = "explorer",
                `data-subtab` = "chromatograms", title = "Chromatograms", "Chromatograms"),
              htmltools::tags$button(class = "sf-sub-btn", `data-tab` = "explorer",
                `data-subtab` = "eic", title = "Extract Ion Chrom.", "EIC")
            )
          ),
          # Workflow
          htmltools::div(class = "sf-nav-group", `data-tab` = "workflow",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "workflow", title = "Workflow",
              "Workflow")
          ),
          # Results + dynamic sub-pages
          htmltools::div(class = "sf-nav-group", `data-tab` = "results",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "results", title = "Results",
              "Results"),
            shiny::uiOutput("results_sidebar_subnav")
          ),
          # Report
          htmltools::div(class = "sf-nav-group", `data-tab` = "report",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "report", title = "Report",
              "Report")
          ),
          # Audit Trail
          htmltools::div(class = "sf-nav-group", `data-tab` = "audit",
            htmltools::tags$button(class = "sf-nav-btn", `data-tab` = "audit", title = "Audit Trail",
              "Audit Trail")
          )
        ),
        htmltools::div(
          id = "sf-topbar-right",
          shiny::uiOutput("engine_data_type"),
          htmltools::tags$span(
            class = "sf-cache-label",
            "Cache: ",
            shiny::textOutput("cache_size", inline = TRUE)
          ),
          shiny::actionButton(
            "clear_cache_button",
            label = "Clear Cache",
            title = "Clear Cache",
            class = "sf-topbar-btn sf-topbar-text-btn"
          ),
          shiny::actionButton(
            "restart_app",
            label = "Restart",
            title = "Reset / Restart",
            class = "sf-topbar-btn sf-topbar-text-btn"
          ),
          shiny::actionButton(
            "settings_button",
            label = NULL,
            icon = shiny::icon("gear"),
            title = "Settings",
            class = "sf-topbar-btn"
          ),
          shiny::uiOutput("notifications_ui")
        )
      ),
      # ---- Body = full-width content (no sidebar) ----
      htmltools::div(
        id = "sf-body",
        # Content area: one .sf-page div per tab, shown via conditionalPanel
        htmltools::div(
          id = "sf-content",
          htmltools::div(
            id = "sf-boot-overlay",
            class = if (isTRUE(boot_loading)) "sf-boot-overlay visible" else "sf-boot-overlay",
            htmltools::div(
              class = "sf-boot-overlay-inner",
              htmltools::tags$img(
                src = "www/logo_StreamFind.png",
                alt = "StreamFind loading"
              )
            )
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'project' || !input.sf_active_tab",
            htmltools::div(class = "sf-page", shiny::uiOutput("project_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'analyses'",
            htmltools::div(class = "sf-page", shiny::uiOutput("analyses_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'explorer'",
            htmltools::div(class = "sf-page", shiny::uiOutput("explorer_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'workflow'",
            htmltools::div(class = "sf-page", shiny::uiOutput("workflow_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'results'",
            htmltools::div(class = "sf-page", shiny::uiOutput("results_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'report'",
            htmltools::div(class = "sf-page", shiny::uiOutput("report_ui"))
          ),
          shiny::conditionalPanel(
            "input.sf_active_tab === 'audit'",
            htmltools::div(class = "sf-page", shiny::uiOutput("audit_ui"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  htmltools::tags$head(
    golem::favicon(ext = "png"),
    htmltools::tags$link(
      rel = "icon",
      type = "image/png",
      href = "favicon.png"
    ),
    # Bootstrap 3.4.1 CSS — required for shinyFiles, modal layout,
    # glyphicons, and Bootstrap grid/button styling when not using fluidPage().
    shiny::singleton(
      htmltools::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "shared/bootstrap/css/bootstrap.min.css"
      )
    ),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "StreamFind"
    ),
    # Bootstrap 3.4.1 JS (jQuery plugin) — Shiny ships Bootstrap 3 CSS but
    # doesn't auto-load its JS when the UI is built with raw htmltools::div().
    # shiny::showModal(), shinyFiles, and other components call $.fn.modal()
    # which comes from this JS.
    shiny::singleton(
      htmltools::tags$script(src = "shared/bootstrap/js/bootstrap.min.js")
    ),
    # Navigation + theme toggle JS
    shiny::tags$script(htmltools::HTML("
      // sfNavigate: activate a main tab, expand its sub-menu, auto-select first sub-tab
      function sfNavigate(tab, subtab) {
        Shiny.setInputValue('sf_active_tab', tab, {priority: 'event'});

        // Toggle active state on nav-groups (CSS drives sub-menu visibility)
        document.querySelectorAll('#sf-nav .sf-nav-group').forEach(function(grp) {
          grp.classList.toggle('active', grp.getAttribute('data-tab') === tab);
        });

        // Toggle active state on main buttons
        document.querySelectorAll('#sf-nav .sf-nav-btn').forEach(function(btn) {
          btn.classList.toggle('active', btn.getAttribute('data-tab') === tab);
        });

        // Determine which subtab to activate
        var activeSubtab = subtab || null;
        if (!activeSubtab) {
          var grp = document.querySelector('#sf-nav .sf-nav-group[data-tab=\"' + tab + '\"]');
          var firstSub = grp ? grp.querySelector('.sf-sub-btn') : null;
          if (firstSub) activeSubtab = firstSub.getAttribute('data-subtab');
        }

        // Update sub-button active states
        document.querySelectorAll('#sf-nav .sf-sub-btn').forEach(function(btn) {
          btn.classList.toggle('active',
            btn.getAttribute('data-tab') === tab &&
            btn.getAttribute('data-subtab') === activeSubtab);
        });

        Shiny.setInputValue('sf_active_subtab', activeSubtab || '', {priority: 'event'});
      }

      // sfSubNavigate: switch sub-tab without changing main tab
      function sfSubNavigate(tab, subtab) {
        Shiny.setInputValue('sf_active_subtab', subtab, {priority: 'event'});
        document.querySelectorAll('#sf-nav .sf-sub-btn').forEach(function(btn) {
          btn.classList.toggle('active',
            btn.getAttribute('data-tab') === tab &&
            btn.getAttribute('data-subtab') === subtab);
        });
      }

      // Event delegation on nav bar (handles static + dynamically rendered sub-buttons)
      document.addEventListener('DOMContentLoaded', function() {
        document.getElementById('sf-nav').addEventListener('click', function(e) {
          var subBtn  = e.target.closest('.sf-sub-btn');
          var mainBtn = e.target.closest('.sf-nav-btn');
          if (subBtn) {
            sfSubNavigate(subBtn.getAttribute('data-tab'), subBtn.getAttribute('data-subtab'));
          } else if (mainBtn) {
            sfNavigate(mainBtn.getAttribute('data-tab'));
          }
        });
      });

      // Auto-activate first sub-tab after dynamic sub-menu renders (e.g. Results)
      Shiny.addCustomMessageHandler('activateFirstSubtab', function(tab) {
        var grp = document.querySelector('#sf-nav .sf-nav-group[data-tab=\"' + tab + '\"]');
        if (grp && grp.classList.contains('active')) {
          setTimeout(function() {
            var firstSub = grp.querySelector('.sf-sub-btn');
            if (firstSub && !grp.querySelector('.sf-sub-btn.active')) {
              sfSubNavigate(tab, firstSub.getAttribute('data-subtab'));
            }
          }, 50);
        }
      });

      // Theme toggle: flip .sf-light / .sf-dark on #sf-app + body[data-sf-theme] for modals
      Shiny.addCustomMessageHandler('toggleTheme', function(msg) {
        var app = document.getElementById('sf-app');
        if (app) {
          if (app.classList.contains('sf-dark')) {
            app.classList.remove('sf-dark');
            app.classList.add('sf-light');
            document.body.removeAttribute('data-sf-theme');
          } else {
            app.classList.remove('sf-light');
            app.classList.add('sf-dark');
            document.body.setAttribute('data-sf-theme', 'dark');
          }
        }
      });

      Shiny.addCustomMessageHandler('setBootOverlay', function(msg) {
        var overlay = document.getElementById('sf-boot-overlay');
        if (!overlay) return;
        if (msg && msg.visible) {
          overlay.classList.add('visible');
        } else {
          overlay.classList.remove('visible');
        }
      });

      // Hard cleanup for restart flow: remove any residual modal/backdrop nodes
      // (including shinyFiles custom containers) and clear body modal flags.
      Shiny.addCustomMessageHandler('cleanupAllModals', function(msg) {
        document.querySelectorAll('.sF-modalContainer, .sF-modalBackdrop, .modal-backdrop').forEach(function(el) {
          if (el && el.parentNode) {
            el.parentNode.removeChild(el);
          }
        });

        var shinyModal = document.getElementById('shiny-modal');
        if (shinyModal) {
          shinyModal.classList.remove('in', 'show');
          shinyModal.style.display = 'none';
        }

        document.body.classList.remove('modal-open');
      });

      // Ensure Shiny's modal wrapper is always rendered as a floating overlay.
      // This guards against missing Bootstrap modal CSS in custom page layouts.
      function enforceShinyModalFloating() {
        var wrapper = document.getElementById('shiny-modal-wrapper');
        var modal = document.getElementById('shiny-modal');
        if (!wrapper || !modal) return;

        wrapper.style.position = 'fixed';
        wrapper.style.top = '0';
        wrapper.style.right = '0';
        wrapper.style.bottom = '0';
        wrapper.style.left = '0';
        wrapper.style.zIndex = '1060';

        modal.style.position = 'fixed';
        modal.style.top = '0';
        modal.style.right = '0';
        modal.style.bottom = '0';
        modal.style.left = '0';
        modal.style.zIndex = '1060';
        modal.style.display = 'flex';
        modal.style.alignItems = 'center';
        modal.style.justifyContent = 'center';
        modal.style.padding = '16px';
        modal.style.overflowX = 'hidden';
        modal.style.overflowY = 'auto';

        var dialog = modal.querySelector('.modal-dialog');
        if (dialog) {
          dialog.style.margin = '0';
          dialog.style.width = 'min(640px, calc(100vw - 32px))';
          dialog.style.maxWidth = '640px';
        }
      }

      document.addEventListener('DOMContentLoaded', function() {
        enforceShinyModalFloating();
        var obs = new MutationObserver(function() {
          enforceShinyModalFloating();
        });
        obs.observe(document.body, { childList: true, subtree: true });
      });

      // Init body theme attribute on page load
      document.addEventListener('DOMContentLoaded', function() {
        var app = document.getElementById('sf-app');
        if (app && app.classList.contains('sf-dark')) {
          document.body.setAttribute('data-sf-theme', 'dark');
        }
      });

      // Notification bell: close dropdown when clicking outside
      document.addEventListener('click', function(e) {
        var wrapper = document.querySelector('.sf-notif-wrapper');
        if (wrapper && !wrapper.contains(e.target)) {
          var dd = document.getElementById('sf-notif-dropdown');
          if (dd) dd.classList.remove('open');
        }
      });
    "))
  )
}
