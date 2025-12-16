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
                                        reactive_engine_save_file,
                                        reactive_project_path,
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
      btn_label <- gsub("DB_", "", obj)
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

  # Setup shinyFiles for LoadEngine
  shinyFiles::shinyFileChoose(
    input,
    paste0(time_var, "_select_LoadEngine"),
    roots = volumes,
    defaultRoot = "wd",
    session = session,
    filetypes = list(sqlite = "sqlite", rds = "rds")
  )

  tiles_content_length <- length(tiles_content)

  tiles_content[[tiles_content_length + 1]] <- shiny::column(
    4,
    shinyFiles::shinyFilesButton(
      paste0(time_var, "_select_LoadEngine"),
      shiny::HTML("Load Engine<br>(.sqlite/.rds)"),
      "Load Engine from .sqlite or .rds file",
      multiple = FALSE,
      style = paste0(
        "width: 100%; height: 120px; ",
        "background-color: #95a5a6; ",
        "border: none; color: white; font-weight: bold; ",
        "font-size: 14px; border-radius: 8px; ",
        "display: flex; align-items: center; justify-content: center; ",
        "text-align: center; padding: 10px; ",
        "transition: all 0.3s ease; ",
        "box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
      )
    ),
    style = "margin-bottom: 15px;"
  )

  tiles_content[[tiles_content_length + 2]] <- shiny::column(
    4,
    shiny::actionButton(
      inputId = paste0(time_var, "_select_MassSpecDemo"),
      label = shiny::HTML("MassSpec<br>Demo Project"),
      style = paste0(
        "width: 100%; height: 120px; ",
        "background-color: #d35400; ",
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

  available_engines <- c(available_engines, "LoadEngine", "MassSpecDemo")

  lapply(available_engines, function(obj) {

    shiny::observeEvent(input[[paste0(time_var, "_select_", obj)]], {

      # MARK: Load Engine from file
      if (paste0(time_var, "_select_LoadEngine") %in% paste0(time_var, "_select_", obj)) {
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
        # MARK: MassSpec Demo case
        if (obj == "MassSpecDemo") {
          # Show loading modal with spinner
          shiny::showModal(shiny::modalDialog(
            title = "Loading MassSpec Demo",
            shiny::div(
              style = "text-align: center; padding: 20px;",
              shiny::div(
                style = "display: inline-block; border: 4px solid #f3f3f3; border-top: 4px solid #3498db; border-radius: 50%; width: 40px; height: 40px; animation: spin 1s linear infinite;",
                shiny::tags$style(HTML("
                  @keyframes spin {
                    0% { transform: rotate(0deg); }
                    100% { transform: rotate(360deg); }
                  }
                "))
              ),
              shiny::br(),
              shiny::br(),
              shiny::p("Please wait while the demo data is being prepared...")
            ),
            footer = NULL,
            easyClose = FALSE
          ))
          # Process demo loading with progress
          tryCatch({
            demo_engine <- .app_MassSpecDemo()
            if (!is.null(demo_engine)) {
              # Save the demo engine to a temporary file
              temp_file <- file.path(tempdir(), "ms_demo_ww_ozone.rds")
              demo_engine$save(temp_file)
              reactive_engine_type("MassSpecEngine")
              reactive_engine_save_file(temp_file)
              reactive_app_mode("WorkflowAssembler")
              reactive_clean_start(TRUE)
              reactive_show_init_modal(FALSE)
              shiny::removeModal()
              shiny::showNotification(
                "MassSpec Demo loaded successfully!",
                duration = 5, 
                type = "message"
              )
            } else {
              shiny::removeModal()
              shiny::showNotification(
                "Failed to load MassSpec Demo. Please ensure StreamFindData package is installed.", 
                duration = 10, 
                type = "error"
              )
              reactive_show_init_modal(TRUE)
            }
          }, error = function(e) {
            shiny::removeModal()
            shiny::showNotification(
              paste("Error loading MassSpec Demo:", e$message),
              duration = 10,
              type = "error"
            )
            reactive_show_init_modal(TRUE)
          })
        
        # MARK: Other engine types
        } else {
          reactive_engine_type(obj)
          shiny::removeModal()
          if (!obj %in% "Engine") {
            reactive_show_init_modal(FALSE)
            if (grepl("DB_", obj)) {
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
                  style = "margin-top: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; min-height: 30px;",
                  shiny::p("No directory selected", style = "margin: 0; color: #999;")
                ),
                footer = shiny::tagList(
                  shiny::actionButton(
                    paste0(project_dir_var, "_confirm"),
                    "Confirm",
                    class = "btn btn-success",
                    disabled = "disabled"
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
                  # Enable confirm button
                  shinyjs::removeClass(paste0(project_dir_var, "_confirm"), "disabled")
                  shinyjs::runjs(paste0("document.getElementById('", paste0(project_dir_var, "_confirm"), "').disabled = false;"))
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
            } else {
              reactive_app_mode("WorkflowAssembler")
              reactive_clean_start(TRUE)
            }
          } else {
            reactive_show_init_modal(TRUE)
          }
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

#' Utility function to create a MassSpecEngine for the MassSpec demo project.
#' @return A MassSpecEngine object or NULL if the required package is not installed.
#' @noRd
#' 
.app_MassSpecDemo <- function() {

  if (!require(StreamFindData)) {
    warning("The package StreamFindData is required to run the demo.")
    return(NULL)
  }

  all_files <- StreamFindData::get_ms_file_paths()
  files <- all_files[grepl("blank|influent|o3sw", all_files)]
  db_all <- StreamFindData::get_ms_tof_spiked_chemicals()
  db_all <- db_all[grepl("S", db_all$tag), ]
  cols <- c("name", "formula", "mass", "rt", "tag")
  db_is <- db_all[db_all$tag %in% "IS", ]
  db_is <- db_is[, cols, with = FALSE]
  db_is <- db_is[!db_is$name %in% c("Ibuprofen-d3", "Naproxen-d3"), ]
  db <- db_all[db_all$tag %in% "S", ]
  db <- db[, cols, with = FALSE]
  db_with_ms2 <- StreamFindData::get_ms_tof_spiked_chemicals_with_ms2()
  db_with_ms2 <- db_with_ms2[db_with_ms2$tag %in% "S", ]
  db_with_ms2 <- db_with_ms2[, c("name", "formula", "mass", "SMILES", "rt", "polarity", "fragments"), with = FALSE]
  db_with_ms2$polarity[db_with_ms2$polarity == 1] <- "positive"
  db_with_ms2$polarity[is.na(db_with_ms2$polarity)] <- "positive"
  db_with_ms2$polarity[db_with_ms2$polarity == -1] <- "negative"
  ms <- MassSpecEngine$new(analyses = files)
  ms$Metadata <- list(
    name = "Wastewater Ozonation Demo",
    author = "Ricardo Cunha",
    description = "Demonstration for non-target screening with mass spectrometry data from wastewater samples before and after ozonation"
  )
  rpls <- c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("influent_neg", 3),
    rep("influent_pos", 3),
    rep("effluent_neg", 3),
    rep("effluent_pos", 3)
  )
  blks <- c(
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3),
    rep("blank_neg", 3),
    rep("blank_pos", 3)
  )
  ms$Analyses <- set_replicate_names(ms$Analyses, rpls)
  ms$Analyses <- set_blank_names(ms$Analyses, blks)
  workflow <- list(
    MassSpecMethod_FindFeatures_openms(
      noiseThrInt = 1000,
      chromSNR = 3,
      chromFWHM = 7,
      mzPPM = 15,
      reEstimateMTSD = TRUE,
      traceTermCriterion = "sample_rate",
      traceTermOutliers = 5,
      minSampleRate = 1,
      minTraceLength = 4,
      maxTraceLength = 70,
      widthFiltering = "fixed",
      minFWHM = 4,
      maxFWHM = 35,
      traceSNRFiltering = TRUE,
      localRTRange = 0,
      localMZRange = 0,
      isotopeFilteringModel = "none",
      MZScoring13C = FALSE,
      useSmoothedInts = FALSE,
      intSearchRTWindow = 3,
      useFFMIntensities = FALSE,
      verbose = FALSE
    ),
    MassSpecMethod_AnnotateFeatures_StreamFind(
      rtWindowAlignment = 0.3,
      maxIsotopes = 8,
      maxCharge = 2,
      maxGaps = 1
    ),
    MassSpecMethod_FilterFeatures_StreamFind(
      excludeIsotopes = TRUE,
      excludeAdducts = TRUE
    ),
    MassSpecMethod_GroupFeatures_openms(
      rtalign = FALSE,
      QT = FALSE,
      maxAlignRT = 5,
      maxAlignMZ = 0.008,
      maxGroupRT = 5,
      maxGroupMZ = 0.008,
      verbose = FALSE
    ),
    MassSpecMethod_FilterFeatures_StreamFind(
      minIntensity = 3000
    ),
    MassSpecMethod_FillFeatures_StreamFind(
      withinReplicate = FALSE,
      rtExpand = 2,
      mzExpand = 0.0005,
      minTracesIntensity = 1000,
      minNumberTraces = 6,
      baseCut = 0.3,
      minSignalToNoiseRatio = 3,
      minGaussianFit = 0.2
    ),
    MassSpecMethod_CalculateFeaturesQuality_StreamFind(
      filtered = FALSE,
      rtExpand = 2,
      mzExpand = 0.0005,
      minTracesIntensity = 1000,
      minNumberTraces = 6,
      baseCut = 0
    ),
    MassSpecMethod_FilterFeatures_StreamFind(
      minSnRatio = 5
    ),
    MassSpecMethod_FilterFeatures_patRoon(
      maxReplicateIntRSD = 40,
      blankThreshold = 5,
      absMinReplicateAbundance = 3
    ),
    MassSpecMethod_FindInternalStandards_StreamFind(
      database = db_is,
      ppm = 8,
      sec = 10
    ),
    MassSpecMethod_CorrectMatrixSuppression_TiChri(
      mpRtWindow = 10,
      istdAssignment = "range",
      istdRtWindow = 50,
      istdN = 2
    ),
    MassSpecMethod_LoadFeaturesMS1_StreamFind(
      filtered = FALSE
    ),
    MassSpecMethod_LoadFeaturesMS2_StreamFind(
      filtered = FALSE
    ),
    MassSpecMethod_LoadFeaturesEIC_StreamFind(
      filtered = FALSE
    ),
    MassSpecMethod_SuspectScreening_StreamFind(
      database = db_with_ms2,
      ppm = 10,
      sec = 15,
      ppmMS2 = 10,
      minFragments = 3
    )
  )
  workflow <- Workflow(workflow)
  ms$Workflow <- workflow
  ms$save("ms_demo_ww_ozone.rds")
  ms
}