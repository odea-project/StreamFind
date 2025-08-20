#' @noRd
.app_util_add_notifications <- function(warnings, name_msg, msg) {
  shiny::showNotification(msg, duration = 5, type = "warning")
  warnings[[name_msg]] <- msg
  return(warnings)
}

#' @noRd
.app_util_remove_notifications <- function(warnings, name_msgs) {
  warnings[name_msgs] <- NULL
  return(warnings)
}

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

#' @noRd
.app_util_use_initial_model <- function(reactive_app_mode,
                                        reactive_engine_type,
                                        reactive_engine_save_file,
                                        reactive_clean_start,
                                        reactive_show_init_modal,
                                        volumes,
                                        input, output, session) {
  
  available_engines <- .get_available_engines()
  
  time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  model_elements <- list()
  
  model_elements[[1]] <- shiny::img(
    src = "www/logo_StreamFind.png",
    width = 250,
    style = "display: block; margin-left: auto; margin-right: auto;"
  )
  
  model_elements[[2]] <- shiny::fluidRow(
    shiny::p(
      "Select an engine to start a new project: ",
      style = "text-align: center;margin-top: 40px;"
    )
  )
  
  model_elements[[3]] <- htmltools::div(
    lapply(available_engines, function(obj) {
      shiny::actionButton(inputId = paste0(time_var, "_select_", obj), label = obj)
    }),
    style = "text-align: center;"
  )
  
  model_elements[[4]] <- shiny::fluidRow(
    shiny::p("Load an existing engine: ", style = "text-align: center;margin-top: 40px;")
  )
  
  shinyFiles::shinyFileChoose(
    input,
    paste0(time_var, "_select_LoadEngine"),
    roots = volumes,
    defaultRoot = "wd",
    session = session,
    filetypes = list(sqlite = "sqlite", rds = "rds")
  )
  
  model_elements[[5]] <- htmltools::div(
    shinyFiles::shinyFilesButton(
      paste0(time_var, "_select_LoadEngine"),
      "Load Engine (.sqlite or .rds)",
      "Load Engine from .sqlite or .rds file",
      multiple = FALSE
    ),
    style = "text-align: center;"
  )
  
  shiny::showModal(shiny::modalDialog(
    title = " ",
    easyClose = TRUE,
    footer = shiny::tagList(shiny::modalButton("Cancel")),
    do.call(shiny::tagList, model_elements)
  ))
  
  available_engines <- c(available_engines, "LoadEngine")
  
  lapply(available_engines, function(obj) {
    
    shiny::observeEvent(input[[paste0(time_var, "_select_", obj)]], {
      if (paste0(time_var, "_select_LoadEngine") %in% paste0(time_var, "_select_", obj) ) {
        input_name <- paste0(time_var, "_select_LoadEngine")
        shiny::req(input[[input_name]])
        fileinfo <- shinyFiles::parseFilePaths(volumes, input[[input_name]])
        if (nrow(fileinfo) > 0) {
          engine_save_file <- fileinfo$datapath
          if (tools::file_ext(engine_save_file) %in% c("sqlite", "rds")) {
            if (file.exists(engine_save_file)) {
              file_format <- tools::file_ext(engine_save_file)
              if (file_format %in% "sqlite") {
                db <- .openCacheDBScope(file = engine_save_file)
                engine_name <- DBI::dbListTables(db)
                if (length(engine_name) == 0) engine_name <- NA_character_
              } else if (file_format %in% "rds") {
                data <- readRDS(engine_save_file)
                if (is.list(data)) {
                  if ("type" %in% names(data)) {
                  engine_name <- paste0(data$type, "Engine")
                  } else {
                    engine_name <- NA_character_
                  }
                } else {
                  engine_name <- NA_character_
                }
              } else {
                engine_name <- NA_character_
              }
            } else {
              msg <- paste("The file", engine_save_file, "does not exist!")
              shiny::showNotification(msg, duration = 10, type = "error")
              reactive_engine_save_file(NA_character_)
              shiny::removeModal()
              reactive_show_init_modal(TRUE)
              return()
            }
          } else {
            engine_name = NA_character_
          }
          if (is.na(engine_name)) {
            msg <- paste("The file", engine_save_file, "is not a valid engine file!")
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_engine_save_file(NA_character_)
            shiny::removeModal()
            reactive_show_init_modal(TRUE)
            return()
          }
          if (!engine_name %in% available_engines) {
            msg <- paste("The engine", engine_name, "is not valid!")
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_engine_save_file(NA_character_)
            shiny::removeModal()
            reactive_show_init_modal(TRUE)
            return()
          }
          if (engine_name %in% available_engines) {
            reactive_app_mode("WorkflowAssembler")
            reactive_engine_type(engine_name)
            reactive_engine_save_file(engine_save_file)
            reactive_clean_start(TRUE)
            reactive_show_init_modal(FALSE)
            shiny::removeModal()
            return()
          }
        }
      } else {
        reactive_engine_save_file(NA_character_)
        reactive_engine_type(obj)
        shiny::removeModal()
        if (!obj %in% "Engine") {
          reactive_show_init_modal(FALSE)
          reactive_app_mode("WorkflowAssembler")
          reactive_clean_start(TRUE)
        } else {
          reactive_show_init_modal(TRUE)
        }
        return()
      }
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
    class = "modal fade",
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
        clone.style.height = '800px';
        modalBody.appendChild(clone);
      }
      
      // Show the modal
      $('#' + plotId.replace(/[^-]*$/, 'plot_modal_container')).modal('show');
    }
    
    // Custom CSS for the maximize button and modal body
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
        .modal-body {
          height: 800px;
          padding: 0 !important;
        }
        .modal-body > div {
          width: 100% !important;
          height: 100% !important;
        }
      </style>
    `);
  "))
}