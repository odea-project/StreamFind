.mod_workflow_UI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("workflow_settings"))),
      shiny::column(6, shiny::uiOutput(ns("selected_settings_details")))
    ),
    htmltools::tags$style(htmltools::HTML("
      .custom-button {
        background-color: #3498DB;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
      }
      .custom-buttonred {
        background-color: #F1948A;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
        padding: 2px 6px;
        font-size: 12px;
      }
      .custom-button:hover {
        background-color: #5DADE2;
      }
      .custom-buttonred:hover {
        background-color: #F5B7B1;
      }
      .workflow-item {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .workflow-item span {
        flex-grow: 1;
      }
      .workflow-item button {
        margin-left: auto;
      }
      .workflow-box {
        background-color: white;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .function-details {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 5px;
        padding: 20px;
      }
      .function-details dt {
        font-weight: bold;
        float: left;
        clear: left;
        width: 120px;
      }
      .function-details dd {
        margin-left: 130px;
      }
      .parameters-section {
        margin-top: 20px;
        border-top: 1px solid #e9ecef;
        padding-top: 20px;
      }
    "))
  )
}

.mod_workflow_Server <- function(id, engine, engine_type, reactive_workflow, reactive_warnings, reactive_history, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$update_settings, {
      rw <- reactive_workflow_local()
      function_name <- reactive_selected_settings()
      shiny::req(function_name %in% names(rw))

      settings <- rw[[function_name]]
      param_names <- names(settings$parameters)

      for (param_name in param_names) {
        input_value <- input[[param_name]]
        if (!is.null(input_value)) {
          settings$parameters[[param_name]] <- input_value
        }
      }

      rw[[function_name]] <- settings
      reactive_workflow_local(rw)
      shiny::showNotification("Settings updated successfully!", type = "message")
    })

    .add_notifications <- function(warnings, name_msg, msg) {
      shiny::showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }

    available_processing_methods <- engine$processing_methods()
    StreamFind_env <- as.environment("package:StreamFind")
    engine_data_type <- gsub("Engine", "", is(engine))
    engine_settings_key <- paste0(engine_data_type, "Settings_")
    Settings_functions <- ls(envir = StreamFind_env, pattern = paste0("^", engine_settings_key))
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) is.function(get(x, envir = .GlobalEnv)))]
    Settings_functions <- Settings_functions[sapply(Settings_functions, function(x) any(sapply(available_processing_methods$name, function(y) grepl(y, x))))]
    Settings_functions_short <- gsub(engine_settings_key, "", Settings_functions)
    names(Settings_functions) <- Settings_functions_short

    reactive_workflow_local <- shiny::reactiveVal(list())
    init_workflow <- reactive_workflow()
    reactive_workflow_local(init_workflow)

    shiny::observe({
      rw <- reactive_workflow_local()
      if (length(rw) > 0) {
        rw_idx <- seq_along(rw)
        rw_names <- vapply(rw, function(x) paste0(x$call, "_", x$algorithm, " - "), NA_character_)
        rw_names <- paste0(rw_names, rw_idx)
        names(rw) <- rw_names
        reactive_workflow_local(rw)
      }
    })

    output$save_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        if (!"workflow_not_updated" %in% names(reactive_warnings())) {
          msg <- "Workflow not updated in engine"
          reactive_warnings(.add_notifications(reactive_warnings(), "workflow_not_updated", msg))
        }
        shiny::actionButton(ns("save_workflow"), "Save Workflow", class = "btn-danger")
      }
    })

    output$discard_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) shiny::actionButton(ns("discard_workflow"), "Discard Workflow", class = "btn-danger")
    })

    output$run_workflow_ui <- shiny::renderUI({
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (identical(init_rw, rw) && length(rw) > 0) {
        shiny::actionButton(ns("run_workflow"), "Run Workflow", class = "btn-success")
      }
    })

    reactive_selected_settings <- shiny::reactiveVal(NULL)

    output$workflow_settings <- shiny::renderUI({
      rw <- reactive_workflow_local()

      labels <- lapply(names(rw), function(i) {
        htmltools::tagList(
          htmltools::div(class = "workflow-item",
            shiny::actionButton(ns(paste0("workflow_del_", i)), "X", class = "custom-buttonred", style = "margin-right: 10px;"),
            shiny::span(i),
            shiny::actionButton(ns(paste0("workflow_edit_", i)), "Details", class = "custom-button")
          )
        )
      })

      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_edit_", i)]], {
          reactive_selected_settings(i)
        }, ignoreInit = TRUE)
      })

      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_del_", i)]], {
          rw <- reactive_workflow_local()
          rw <- rw[!names(rw) %in% i]
          reactive_workflow_local(rw)
        }, ignoreInit = TRUE)
      })

      shinydashboard::box(width = 12, title = NULL, solidHeader = TRUE, class = "workflow-box",
        shiny::column(12,
          htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(style = "margin-right: 10px;", shinyFiles::shinyFilesButton(ns("load_workflow"), "Load Workflow", "Select a JSON or RDS file with workflow processing settings", multiple = FALSE)),
            htmltools::div(style = "margin-right: 10px;", shiny::actionButton(ns("clear_workflow"), "Clear Workflow")),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("save_workflow_ui"))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("discard_workflow_ui"))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns("run_workflow_ui"))),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        shiny::column(12, htmltools::p(" ")),
        shiny::column(12, htmltools::p("Select Processing Settings")),
        shiny::column(9, shiny::selectInput(ns("settings_selector"), label = NULL, choices = Settings_functions_short, multiple = FALSE)),
        shiny::column(3, shiny::actionButton(ns("add_workflow_step"), "Add Workflow Step")),
        shiny::column(12, htmltools::h3("Workflow")),
        shiny::column(12,
          sortable::rank_list(
            text = "Drag to order",
            labels = labels,
            input_id = ns("rank_workflow_names")
          )
        )
      )
    })

    shiny::observeEvent(input$rank_workflow_names, {
      rw <- reactive_workflow_local()
      new_order <- input$rank_workflow_names
      new_order <- vapply(new_order, function(x) strsplit(x, "\n")[[1]][2], NA_character_)
      rw <- rw[new_order]
      reactive_workflow_local(rw)
    })

    shiny::observeEvent(input$add_workflow_step, {
      rw <- reactive_workflow_local()
      settings_name <- input$settings_selector
      if (length(settings_name) > 0) {
        settings <- do.call(Settings_functions[settings_name], list())
        if (length(rw) > 0) {
          max <- available_processing_methods$max[available_processing_methods$name == settings$call]
          calls <- vapply(rw, function(x) x$call, NA_character_)
          if (sum(calls == settings$call) >= max) {
            shiny::showNotification("Maximum number of settings for ", settings$call, " reached!", duration = 5, type = "warning")
            return()
          }
        }
        rw <- c(rw, list(settings))
        reactive_workflow_local(rw)
      }
    })

    shinyFiles::shinyFileChoose(input, "load_workflow", roots = volumes, defaultRoot = "wd", session = session, filetypes = c("json", "rds"))
    reactive_workflow_file <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load_workflow, {
      settings <- NULL
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$load_workflow)
      if (nrow(fileinfo) > 0) {
        file <- fileinfo$datapath
        if (length(file) == 1) {
          if (file.exists(file)) {
            if (tools::file_ext(file) %in% "json") settings <- jsonlite::fromJSON(file)
            if (tools::file_ext(file) %in% "rds") settings <- readRDS(file)
          } else {
            shiny::showNotification("File does not exist!", duration = 5, type = "warning")
          }
        }
      }

      if (is.list(settings)) {
        cols_check <- c("call", "algorithm", "parameters")
        if (all(cols_check %in% names(settings))) settings <- list(settings)
        settings <- lapply(settings, as.ProcessingSettings)
        names(settings) <- NULL
        all_ps <- vapply(settings, function(x) class(x)[1], "")
        if (all(all_ps %in% "ProcessingSettings")) {
          possible_methods <- engine$processing_methods()
          if (nrow(possible_methods) == 0) {
            shiny::showNotification("No processing methods available in engine!", duration = 5, type = "warning")
            return()
          }
          only_one_possible <- possible_methods$name[possible_methods$max == 1]
          call_names <- vapply(settings, function(x) x$call, NA_character_)
          duplicated_names <- call_names[duplicated(call_names)]
          if (any(duplicated_names %in% only_one_possible)) {
            if (length(duplicated_names) == 1) {
              shiny::showNotification("\U2139 ", duplicated_names, " is duplicate and only one is possible!", duration = 5, type = "warning")
            } else {
              shiny::showNotification(
                paste0("\U2139 Duplicate settings for the following methods and only one is possible! Not added.\n",
                paste(only_one_possible[only_one_possible %in% duplicated_names], collapse = "\n")),
                duration = 5, type = "warning"
              )
            }
            return()
          }

          if (!identical(settings, unname(reactive_workflow_local()))) reactive_workflow_local(settings)
        } else {
          not_conform <- which(!all_ps %in% "ProcessingSettings")
          shiny::showNotification("Settings (number/s: ", paste(not_conform, collapse = "; "), ") content or structure not conform! Not added.", duration = 5, type = "warning")
        }
      }
    })

    shiny::observeEvent(input$save_workflow, {
      rw <- unname(reactive_workflow_local())

      if (length(rw) == 0) {
        shiny::showNotification("No workflow to save.", duration = 5, type = "warning")
        return()
      }

      save_directory <- volumes
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      file_name <- paste0("workflow_", timestamp, ".rds")
      file_path <- file.path(save_directory, file_name)

      tryCatch({
        saveRDS(rw, file_path)
        shiny::showNotification(paste("Workflow saved successfully at:", file_path), duration = 5, type = "message")
        reactive_workflow(rw)
      }, error = function(e) {
        shiny::showNotification(paste("Error saving workflow:", e$message), duration = 5, type = "error")
      })
    })

    shiny::observeEvent(input$clear_workflow, {
      reactive_workflow_local(list())
    })

    shiny::observeEvent(input$save_workflow, {
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        engine$remove_settings()
        engine$add_settings(unname(reactive_workflow_local()))
        reactive_workflow(engine$settings)
        reactive_history(engine$history)
        warnings <- reactive_warnings()
        warnings[["workflow_not_updated"]] <- NULL
        reactive_warnings(warnings)
      }
    })

    shiny::observeEvent(input$discard_workflow, {
      init_rw <- reactive_workflow()
      rw <- unname(reactive_workflow_local())
      if (!identical(init_rw, rw)) {
        reactive_workflow_local(init_rw)
        warnings <- reactive_warnings()
        warnings[["workflow_not_updated"]] <- NULL
        reactive_warnings(warnings)
      }
    })

    output$selected_settings_details <- shiny::renderUI({
      shiny::req(reactive_selected_settings())
      function_name <- reactive_selected_settings()
      if (function_name %in% names(reactive_workflow_local())) {
        htmltools::tagList(
          shiny::h3(paste("Details of", function_name), style = "color: #3498DB; margin-bottom: 20px;"),
          shiny::uiOutput(ns("function_code")),
          shiny::actionButton(ns("update_settings"), "Update Settings", class = "btn-primary", style = "color: white; margin-top: 20px;")
        )
      }
    })

    output$function_code <- shiny::renderUI({
      shiny::req(reactive_selected_settings())
      rw <- reactive_workflow_local()
      function_name <- reactive_selected_settings()
      shiny::req(function_name %in% names(rw))

      settings <- rw[[function_name]]
      help_links <- list(
        "AnnotateFeatures_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_AnnotateFeatures_StreamFind.html",
        "AverageSpectra_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_AverageSpectra_StreamFind.html",
        "BinSpectra_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_BinSpectra_StreamFind.html",
        "CalculateQuality_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CalculateQuality_StreamFind.html",
        "CalculateSpectraCharges_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CalculateSpectraCharges_StreamFind.html",
        "CentroidSpectra_qCentroids" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CentroidSpectra_qCentroids.html",
        "ClusterSpectra_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_ClusterSpectra_StreamFind.html",
        "CorrectChromatogramsBaseline_airpls" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CorrectChromatogramsBaseline_airpls.html",
        "CorrectChromatogramsBaseline_baseline" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CorrectChromatogramsBaseline_baseline.html",
        "CorrectSpectraBaseline_airpls" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CorrectSpectraBaseline_airpls.html",
        "CorrectSpectraBaseline_baseline" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_CorrectSpectraBaseline_baseline.html",
        "DeconvoluteSpectra_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_DeconvoluteSpectra_StreamFind.html",
        "FillFeatures_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FillFeatures_StreamFind.html",
        "FilterFeatures_patRoon" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FilterFeatures_patRoon.html",
        "FilterFeatures_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FilterFeatures_StreamFind.html",
        "FindFeatures_kpic2" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindFeatures_kpic2.html",
        "FindFeatures_openms" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindFeatures_openms.html",
        "FindFeatures_qPeaks" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindFeatures_qPeaks.html",
        "FindFeatures_xcms3_centwave" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindFeatures_xcms3_centwave.html",
        "FindFeatures_xcms3_matchedfilter" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindFeatures_xcms3_matchedfilter.html",
        "FindInternalStandards_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_FindInternalStandards_StreamFind.html",
        "GenerateCompounds_metfrag" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_GenerateCompounds_metfrag.html",
        "GenerateFormulas_genform" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_GenerateFormulas_genform.html",
        "GroupFeatures_openms" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_GroupFeatures_openms.html",
        "GroupFeatures_xcms3_peakdensity" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_GroupFeatures_xcms3_peakdensity.html",
        "IntegrateChromatograms_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_IntegrateChromatograms_StreamFind.html",
        "LoadFeaturesEIC_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_LoadFeaturesEIC_StreamFind.html",
        "LoadFeaturesMS1_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_LoadFeaturesMS1_StreamFind.html",
        "LoadFeaturesMS2_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_LoadFeaturesMS2_StreamFind.html",
        "LoadMSPeakLists_patRoon" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_LoadMSPeakLists_patRoon.html",
        "LoadMSPeakLists_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_LoadMSPeakLists_StreamFind.html",
        "NormalizeSpectra_blockweight" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_NormalizeSpectra_blockweight.html",
        "NormalizeSpectra_meancenter" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_NormalizeSpectra_meancenter.html",
        "NormalizeSpectra_minmax" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_NormalizeSpectra_minmax.html",
        "NormalizeSpectra_scale" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_NormalizeSpectra_scale.html",
        "NormalizeSpectra_snv" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_NormalizeSpectra_snv.html",
        "SmoothChromatograms_movingaverage" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SmoothChromatograms_movingaverage.html",
        "SmoothChromatograms_savgol" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SmoothChromatograms_savgol.html",
        "SmoothSpectra_movingaverage" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SmoothSpectra_movingaverage.html",
        "SmoothSpectra_savgol" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SmoothSpectra_savgol.html",
        "SubtractBlankSpectra_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SubtractBlankSpectra_StreamFind.html",
        "SuspectScreening_forident" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SuspectScreening_forident.html",
        "SuspectScreening_patRoon" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SuspectScreening_patRoon.html",
        "SuspectScreening_StreamFind" = "https://odea-project.github.io/StreamFind/reference/MassSpecSettings_SuspectScreening_StreamFind.html"
      )

      short_function_name <- gsub(" - \\d+$", "", function_name)
      help_url <- help_links[[short_function_name]]

      create_parameter_ui_noedit <- function(param_name, param_value) {
        value_display <- if (is.atomic(param_value) && length(param_value) == 1) {
          as.character(param_value)
        } else if (is.list(param_value)) {
          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            lapply(names(param_value), function(sub_param) {
              shiny::tags$li(
                shiny::tags$span(style = "color: #2980B9; font-weight: bold;", sub_param), ": ",
                if (is.atomic(param_value[[sub_param]])) {
                  as.character(param_value[[sub_param]])
                } else {
                  "NULL"
                }
              )
            })
          )
        } else {
          "NULL"
        }

        shiny::tagList(
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(value_display)
        )
      }

      create_parameter_ui <- function(param_name, param_value) {
        ns <- session$ns

        input_element <- NULL
        if (is.null(param_value)) {
          input_element <- shiny::textInput(ns(param_name), label = NULL, value = "")
        } else if (is.logical(param_value)) {
          input_element <- shiny::checkboxInput(ns(param_name), label = NULL, value = param_value)
        } else if (is.numeric(param_value)) {
          input_element <- shiny::numericInput(ns(param_name), label = NULL, value = param_value)
        } else if (is.character(param_value)) {
          input_element <- shiny::textInput(ns(param_name), label = NULL, value = param_value)
        } else if (is.list(param_value) && all(sapply(param_value, is.character))) {
          input_element <- shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            lapply(seq_along(param_value), function(i) {
              shiny::tags$li(
                shiny::textInput(ns(paste0(param_name, "_", i)), label = NULL, value = param_value[[i]])
              )
            })
          )
        } else {
          input_element <- shiny::tags$p(paste("Unsupported parameter type: ", class(param_value)))
        }

        shiny::tagList(
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(style = "display: flex; align-items: center;", input_element)
        )
      }

      param_names <- names(settings$parameters)
      other_names <- setdiff(names(settings), c("parameters"))

      shiny::tags$div(
        class = "function-details",
        shiny::tags$dl(
          lapply(other_names, function(param) {
            create_parameter_ui_noedit(param, settings[[param]])
          })
        ),
        shiny::tags$div(
          class = "parameters-section",
          shiny::tags$h4("Parameters"),
          shiny::tags$dl(
            lapply(param_names, function(param) {
              create_parameter_ui(param, settings$parameters[[param]])
            })
          )
        ),
        if (!is.null(help_url)) {
          shiny::tags$div(
            style = "margin-top: 20px;",
            shiny::tags$a(
              href = help_url,
              target = "_blank",
              "View Help Documentation",
              style = "color: #3498DB; text-decoration: underline; cursor: pointer; font-size: 16px;"
            )
          )
        } else {
          shiny::tags$div(style = "margin-top: 20px;", "No help documentation available.")
        }
      )
    })
  })
}