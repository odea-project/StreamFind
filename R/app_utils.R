# MARK: Notifications
#' @noRd
.app_util_add_notifications <- function(warnings, name_msg, msg) {
  shiny::showNotification(msg, duration = 5, type = "warning")
  warnings[[name_msg]] <- msg
  warnings
}

#' @noRd
.app_util_remove_notifications <- function(warnings, name_msgs) {
  warnings[name_msgs] <- NULL
  warnings
}

# MARK: Volumes for shinyFiles
#' @noRd
.app_util_get_volumes <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    # drives <- system("wmic logicaldisk get name", intern = TRUE)
    # drives <- drives[grepl(":", drives)]
    # drives <- gsub("\\s+", "", drives)
    # names(drives) <- drives
    drives <- system(
      'powershell -NoProfile -Command "Get-PSDrive -PSProvider FileSystem | Select-Object -ExpandProperty Name"',
      intern = TRUE
    )
    drives <- paste0(drives, ":")
    names(drives) <- drives
  } else {
    drives <- list.files("/media", full.names = TRUE)
    names(drives) <- basename(drives)
    if (length(drives) == 0) {
      drives <- list.files("/mnt", full.names = TRUE)
      names(drives) <- basename(drives)
    }
  }
  c("wd" = getwd(), drives)
}

# MARK: Initial Modal
#' @noRd
.app_util_use_initial_modal <- function(reactive_app_mode,
                                        reactive_engine_type,
                                        reactive_project_path,
                                        reactive_show_init_modal,
                                        volumes,
                                        input, output, session) {

  available_engines <- .get_available_engines()
  available_engines <- available_engines[grepl("Engine", available_engines)]

  time_var <- format(Sys.time(), "%Y%m%d%H%M%S")

  model_elements <- list()

  model_elements[[1]] <- shiny::img(
    src = "www/logo_StreamFind.png",
    width = 250,
    style = "display: block; margin-left: auto; margin-right: auto; margin-bottom: 30px;"
  )

  # Color palette for tiles based on StreamFind logo (green and blue theme)
  tile_colors <- c(
    "#1e7e34", "#0066cc", "#2d9f4f", "#0052a3",
    "#27ae60", "#2874a6", "#229954", "#1f618d",
    "#16a085", "#004d99", "#0d7c2b", "#00ff99"
  )

  # Create tile container with grid layout
  tiles_content <- lapply(seq_along(available_engines), function(i) {
    obj <- available_engines[i]
    color <- tile_colors[(i - 1) %% length(tile_colors) + 1]
    btn_label <- obj
    if (grepl("DB", obj)) {
      btn_label <- gsub("Engine", "", obj)
      btn_label <- shiny::HTML(paste0(btn_label, "<br>(Database Backend)"))
    }
    shiny::column(
      4,
      shiny::div(
        shiny::actionButton(
          inputId = paste0(time_var, "_select_", obj),
          label = btn_label,
          style = paste0(
            "width: 100%; height: 120px; ",
            "background-color: ", color, "; ",
            "border: none; color: white; font-weight: bold; ",
            "font-size: 14px; border-radius: 8px; ",
            "display: flex; align-items: center; justify-content: center; ",
            "text-align: center; padding: 10px; ",
            "transition: all 0.3s ease; ",
            "box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
          ),
          onmouseover = paste0(
            "this.style.boxShadow='0 4px 8px rgba(0,0,0,0.3)'; ",
            "this.style.transform='translateY(-2px)';"
          ),
          onmouseout = paste0(
            "this.style.boxShadow='0 2px 4px rgba(0,0,0,0.2)'; ",
            "this.style.transform='translateY(0)';"
          )
        ),
        style = "margin-bottom: 15px;"
      )
    )
  })

  model_elements[[2]] <- shiny::fluidRow(
    do.call(shiny::tagList, tiles_content),
    style = "margin-top: 20px;"
  )

  shiny::showModal(shiny::modalDialog(
    title = " ",
    easyClose = TRUE,
    footer = shiny::tagList(shiny::modalButton("Cancel")),
    do.call(shiny::tagList, model_elements)
  ))

  lapply(available_engines, function(obj) {

    shiny::observeEvent(input[[paste0(time_var, "_select_", obj)]], {
      reactive_engine_type(obj)
      shiny::removeModal()
      reactive_show_init_modal(FALSE)
      # Setup shinyFiles for project directory selection
      project_dir_var <- paste0(time_var, "_select_ProjectDir")
      shinyFiles::shinyDirChoose(
        input,
        project_dir_var,
        roots = volumes,
        defaultRoot = "wd",
        session = session
      )
      # Create a modal to ask for project path
      shiny::showModal(shiny::modalDialog(
        title = "Select Project Directory",
        shiny::p("Please select a directory for your project:"),
        shiny::div(
          shinyFiles::shinyDirButton(
            project_dir_var,
            "Choose Directory",
            "Select a folder for the project",
            class = "btn btn-primary"
          ),
          style = "text-align: center; margin: 20px 0;"
        ),
        shiny::div(
          id = paste0(project_dir_var, "_display"),
          style = "margin-top: 15px; padding: 10px; background-color: var(--sf-content-bg, #f5f5f5); border: 1px solid rgba(128,128,128,0.2); border-radius: 4px; min-height: 30px;",
          shiny::p("No directory selected", style = "margin: 0; color: #999;")
        ),
        footer = shiny::tagList(
          shiny::actionButton(
            paste0(project_dir_var, "_confirm"),
            "Confirm",
            class = "btn btn-success"
          ),
          shiny::modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
      # Handle directory selection
      shiny::observeEvent(input[[project_dir_var]], {
        shiny::req(input[[project_dir_var]])
        dirinfo <- shinyFiles::parseDirPath(volumes, input[[project_dir_var]])
        if (length(dirinfo) > 0) {
          project_path <- dirinfo
          # Update display
          shiny::removeUI(selector = paste0("#", project_dir_var, "_display > *"))
          shiny::insertUI(
            selector = paste0("#", project_dir_var, "_display"),
            where = "afterBegin",
            shiny::p(project_path, style = "margin: 0; color: #333; word-break: break-all;")
          )
        }
      })
      # Handle confirmation
      shiny::observeEvent(input[[paste0(project_dir_var, "_confirm")]], {
        dirinfo <- shinyFiles::parseDirPath(volumes, input[[project_dir_var]])
        if (length(dirinfo) > 0) {
          reactive_project_path(dirinfo)
          reactive_app_mode("WADB")
          shiny::removeModal()
        } else {
          shiny::showNotification(
            "Please select a valid directory",
            duration = 5,
            type = "warning"
          )
        }
      })
    })
  })
}

#' Maximize button for plot
#'
#' @param plot_id ID of plot output to maximize
#' @param ns_full Namespace function for the Shiny module
#' @return A shiny tag containing the maximize button
#' @noRd
.app_util_create_maximize_button <- function(plot_id, ns_full) {
  button_id <- paste0(plot_id, "_maximize")

  shiny::tags$button(
    id = ns_full(button_id),
    class = "btn btn-sm btn-light plot-maximize-btn",
    title = "Maximize plot",
    style = "
      position: absolute;
      top: 0;
      left: 0;
      width: 25px;
      height: 25px;
      padding: 2.5px;
      display: flex;
      align-items: center;
      justify-content: center;
      z-index: 10;
    ",
    onclick = paste0("maximizePlot('", ns_full(plot_id), "', '", ns_full(button_id), "');"),
    shiny::icon("expand")
  )
}

#' Modal container for plots
#'
#' @param ns_full Namespace function for the Shiny module
#' @return A shiny tag containing the modal container
#' @noRd
.app_util_create_plot_modal <- function(ns_full) {
  shiny::tags$div(
    id = ns_full("plot_modal_container"),
    class = "modal fade plot-modal",
    tabindex = "-1",
    role = "dialog",
    'aria-hidden' = "true",

    shiny::tags$div(
      class = "modal-dialog modal-lg modal-dialog-centered",
      style = "max-width: 90%; width: 90%;",

      shiny::tags$div(
        class = "modal-content",

        # Modal header
        shiny::tags$div(
          class = "modal-header",
          shiny::tags$h5(class = "modal-title", id = ns_full("plot_modal_title"), "Plot"),
          shiny::tags$button(
            type = "button",
            class = "close",
            'data-dismiss' = "modal",
            'aria-label' = "Close",
            shiny::tags$span('aria-hidden' = "true", HTML("×"))
          )
        ),

        # Modal body
        shiny::tags$div(
          class = "modal-body p-0",
          id = ns_full("plot_modal_body")
        )
      )
    )
  )
}

#' JavaScript functions for plot maximization
#'
#' @return A shiny tag containing the JavaScript code
#' @noRd
.app_util_plot_maximize_js <- function() {
  shiny::tags$script(HTML("
    // Function to maximize a plot in a modal
    function maximizePlot(plotId, buttonId) {
      // Get the original plot div
      var originalPlot = document.getElementById(plotId);

      // If not found, try with the plotly class
      if (!originalPlot) {
        originalPlot = document.querySelector('.js-plotly-plot[id^=\"' + plotId + '\"]');
      }

      if (!originalPlot) {
        console.error('Plot not found:', plotId);
        return;
      }

      // Get the button element to extract plot title
      var button = document.getElementById(buttonId);
      var plotTitle = '';

      // Find the closest card header or section title
      var header = button.closest('.card-header');
      if (header) {
        plotTitle = header.textContent.trim();
      } else {
        var section = button.closest('div').querySelector('.section-title');
        if (section) {
          plotTitle = section.textContent.trim();
        } else {
          // Default title
          plotTitle = 'Plot View';
        }
      }

      // Set the modal title
      document.getElementById(plotId.replace(/[^-]*$/, 'plot_modal_title')).textContent = plotTitle;

      // Get the modal body
      var modalBody = document.getElementById(plotId.replace(/[^-]*$/, 'plot_modal_body'));

      // Clear previous content
      modalBody.innerHTML = '';

      // If it's a plotly plot
      if (originalPlot.classList.contains('js-plotly-plot')) {
        // Create a new container for the plot
        var newPlotContainer = document.createElement('div');
        newPlotContainer.id = 'modal-' + plotId;
        newPlotContainer.style.width = '100%';
        newPlotContainer.style.height = '100%';
        modalBody.appendChild(newPlotContainer);

        // Clone the plot to the modal with updated layout
        var newLayout = JSON.parse(JSON.stringify(originalPlot.layout));
        newLayout.width = null;
        newLayout.height = 800;
        newLayout.autosize = true;

        // Ensure margins allow full width usage
        newLayout.margin = {
          l: 50,
          r: 30,
          t: 30,
          b: 50
        };

        // Calculate 90% of viewport height
        var plotHeight = Math.floor(window.innerHeight * 0.9);
        newLayout.height = plotHeight;

        Plotly.newPlot(
          newPlotContainer.id,
          JSON.parse(JSON.stringify(originalPlot.data)),
          newLayout,
          {responsive: true}
        );

        // Trigger resize after modal is shown with a slight delay
        $('#' + plotId.replace(/[^-]*$/, 'plot_modal_container')).on('shown.bs.modal', function() {
          setTimeout(function() {
            Plotly.Plots.resize(newPlotContainer.id);
          }, 100);
        });
      } else {
        // For other types of plots or content
        var clone = originalPlot.cloneNode(true);
        clone.style.width = '100%';
        clone.style.height = Math.floor(window.innerHeight * 0.9) + 'px';
        modalBody.appendChild(clone);
      }

      // Show the modal
      $('#' + plotId.replace(/[^-]*$/, 'plot_modal_container')).modal('show');
    }

    // Custom CSS for the maximize button and plot modal body
    document.head.insertAdjacentHTML('beforeend', `
      <style>
        .plot-maximize-btn {
          position: absolute;
          top: 10px;
          right: 10px;
          z-index: 100;
          opacity: 0.6;
          font-size: 0.8rem;
          padding: 3px 6px;
        }
        .plot-maximize-btn:hover {
          opacity: 1;
        }
        .plot-container {
          position: relative;
        }
        /* Only apply to plot modals, not all modals */
        .plot-modal .modal-body {
          height: 90vh;
          padding: 0 !important;
        }
        .plot-modal .modal-body > div {
          width: 100% !important;
          height: 100% !important;
        }
        /* Ensure shinyFiles modals keep their default styling */
        .shinyFiles .modal-dialog {
          max-width: 900px !important;
          width: auto !important;
        }
        .shinyFiles .modal-body {
          height: auto !important;
          max-height: 70vh !important;
          overflow-y: auto !important;
          padding: 15px !important;
        }
        .shinyFiles .modal-body > div {
          width: auto !important;
          height: auto !important;
        }
        .shinyFiles {
          z-index: 1060 !important;
        }
      </style>
    `);
  "))
}
