#' @noRd
.mod_WorkflowAssembler_UI <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = ns("project"),
      shiny::fluidRow(
        shiny::uiOutput(ns("project_control_ui")),
      ),
      shiny::fluidRow(
        shiny::uiOutput(ns("metadata_ui"))
      )
    ),
    shinydashboard::tabItem(
      tabName = ns("analyses"),
      shiny::fluidRow(shiny::uiOutput(ns("analyses_ui")))
    ),
    shinydashboard::tabItem(
      tabName = ns("explorer"),
      shiny::fluidRow(shiny::uiOutput(ns("explorer_ui")))
    ),
    shinydashboard::tabItem(
      tabName = ns("workflow"),
      shiny::fluidRow(shiny::uiOutput(ns("workflow_ui")))
    ),
    shinydashboard::tabItem(
      tabName = ns("results"),
      shiny::fluidRow(shiny::uiOutput(ns("results_ui")))
    ),
    shinydashboard::tabItem(
      tabName = ns("report"),
      shiny::fluidRow(
        shiny::uiOutput(ns("report_ui"))
      )
    ),
    shinydashboard::tabItem(
      tabName = ns("audit"),
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          solidHeader = TRUE,
          DT::dataTableOutput(ns("audit_ui"), height = "calc(100vh - 50px - 30px - 50px)")
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = ns("config"),
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          solidHeader = TRUE,
          DT::dataTableOutput(ns("config_ui"), height = "calc(100vh - 50px - 30px - 50px)")
        )
      )
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Server <- function(
  id,
  reactive_clean_start,
  reactive_engine_type,
  reactive_engine_save_file,
  reactive_warnings
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # MARK: Global Constants/Mutable
    # Global Constants/Mutable -----
    pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
    volumes <- .app_util_get_volumes()
    engine <- NULL
    analyses_class_dummy <- NULL

    # MARK: Global Reactive Variables
    # Global Reactive Variables -----
    reactive_wdir <- shiny::reactiveVal(getwd())
    reactive_volumes <- shiny::reactiveVal(volumes)
    reactive_metadata <- shiny::reactiveVal(NULL)
    reactive_analyses <- shiny::reactiveVal(NULL)
    reactive_workflow <- shiny::reactiveVal(NULL)
    reactive_results <- shiny::reactiveVal(NULL)
    reactive_audit <- shiny::reactiveVal(NULL)
    reactive_engine_config <- shiny::reactiveVal(NULL)
    reactive_saved_metadata <- shiny::reactiveVal(NULL)
    reactive_saved_analyses <- shiny::reactiveVal(NULL)
    reactive_saved_workflow <- shiny::reactiveVal(NULL)
    reactive_saved_results <- shiny::reactiveVal(NULL)
    reactive_audit_saved <- shiny::reactiveVal(NULL)
    reactive_engine_config_saved <- shiny::reactiveVal(NULL)
    reactive_config <- shiny::reactiveVal(AppConfig())

    # MARK: obs Engine Save File
    # obs Engine Save File -----
    shiny::observeEvent(reactive_engine_save_file(), {
      engine_save_file <- reactive_engine_save_file()
      if (!is.na(engine_save_file)) {
        if (!grepl(".sqlite$|.rds$", engine_save_file)) {
          msg <- paste(
            "The file",
            engine_save_file,
            "is not an sqlite or rds file!"
          )
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_engine_save_file(NA_character_)
        }
      }
    })

    # MARK: obs Clean Start
    # obs Clean Start -----
    shiny::observeEvent(reactive_clean_start(), {
      if (reactive_clean_start()) {
        envSF <- asNamespace("StreamFind")
        engine_type <- reactive_engine_type()
        engine_call <- get(engine_type, envir = envSF)
        engine_call_new <- engine_call[["new"]]
        engine <<- suppressMessages(do.call(engine_call_new, list()))
        engine_data_type <- gsub("Engine", "", engine_type)
        analyses_data_type <- paste0(engine_data_type, "Analyses")
        analyses_call <- get(analyses_data_type, envir = envSF)
        analyses_class_dummy <<- suppressMessages(do.call(
          analyses_call,
          list()
        ))
        if (!is.na(reactive_engine_save_file())) {
          tryCatch(
            {
              engine$load(reactive_engine_save_file())
            },
            warning = function(w) {
              msg <- paste(
                "Warning for",
                reactive_engine_save_file(),
                ":",
                conditionMessage(w)
              )
              reactive_warnings(.app_util_add_notifications(
                reactive_warnings(),
                "load_engine",
                msg
              ))
            },
            error = function(e) {
              msg <- paste(
                "Error for",
                reactive_engine_save_file(),
                ":",
                conditionMessage(e)
              )
              reactive_warnings(.app_util_add_notifications(
                reactive_warnings(),
                "load_engine",
                msg
              ))
            }
          )
        }
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_engine_config(engine$Config)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
        reactive_engine_config_saved(engine$Config)
        reactive_clean_start(FALSE)
      }
    })

    # MARK: obs Warning Unsaved engine
    # obs Warning Unsaved engine -----
    shiny::observe({
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      equal_history <- all(
        identical(reactive_metadata(), reactive_saved_metadata()),
        identical(reactive_analyses(), reactive_saved_analyses()),
        identical(reactive_workflow(), reactive_saved_workflow()),
        identical(reactive_results(), reactive_saved_results()),
        identical(reactive_audit(), reactive_audit_saved()),
        identical(reactive_engine_config(), reactive_engine_config_saved())
      )
      if (!equal_history && !has_unsaved_changes) {
        reactive_warnings(
          .app_util_add_notifications(
            reactive_warnings(),
            "unsaved_changes",
            "Unsaved changes in the engine!"
          )
        )
      }
      if (equal_history) {
        reactive_warnings(.app_util_remove_notifications(
          reactive_warnings(),
          "unsaved_changes"
        ))
      }
    })

    # MARK: obs Save Engine
    # obs Save Engine -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(reactive_engine_save_file())
      reactive_warnings(.app_util_remove_notifications(
        reactive_warnings(),
        "unsaved_changes"
      ))
      reactive_metadata(engine$Metadata)
      reactive_analyses(engine$Analyses)
      reactive_workflow(engine$Workflow)
      reactive_results(engine$Analyses$results)
      reactive_audit(engine$AuditTrail)
      reactive_engine_config(engine$Config)
      reactive_saved_metadata(engine$Metadata)
      reactive_saved_analyses(engine$Analyses)
      reactive_saved_workflow(engine$workflow)
      reactive_saved_results(engine$Analyses$results)
      reactive_audit_saved(engine$AuditTrail)
      reactive_engine_config_saved(engine$Config)
    })

    # MARK: obs Save Engine File
    # obs Save Engine File -----
    shiny::observeEvent(input$save_engine_button_file, {
      shiny::req(input$save_engine_button_file)
      file_info <- shinyFiles::parseSavePath(
        roots = reactive_volumes(),
        input$save_engine_button_file
      )
      if (nrow(file_info) > 0) {
        file_path <- file_info$datapath
        engine$Metadata <- reactive_metadata()
        engine$Analyses <- reactive_analyses()
        engine$Workflow <- reactive_workflow()
        engine$Analyses$results <- reactive_results()
        engine$save(file_path)
        reactive_warnings(.app_util_remove_notifications(
          reactive_warnings(),
          "unsaved_changes"
        ))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_engine_config(engine$Config)
        reactive_engine_save_file(engine$Metadata$file)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
        reactive_engine_config_saved(engine$Config)
      }
    })

    # MARK: obs Reset Engine
    # obs Reset Engine -----
    shiny::observeEvent(input$reset_engine_button, {
      if (is.na(reactive_engine_save_file())) {
        reactive_warnings(.app_util_remove_notifications(
          reactive_warnings(),
          "unsaved_changes"
        ))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_engine_config(engine$Config)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
        reactive_engine_config_saved(engine$Config)
      } else {
        engine$load(reactive_engine_save_file())
        reactive_warnings(.app_util_remove_notifications(
          reactive_warnings(),
          "unsaved_changes"
        ))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_engine_config(engine$Config)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
        reactive_engine_config_saved(engine$Config)
      }
    })

    # MARK: Project
    # Project -----

    # MARK: out Project Control
    ## out Project Control -----
    output$project_control_ui <- shiny::renderUI({
      engine_type <- reactive_engine_type()
      engine_save_file <- reactive_engine_save_file()
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      shinyFiles::shinyDirChoose(
        input,
        "set_wdir_button",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session
      )
      if (has_unsaved_changes) {
        shinyFiles::shinyFileSave(
          input,
          "save_engine_button_file",
          roots = reactive_volumes(),
          defaultRoot = "wd",
          session = session
        )
      }
      left_content <- list(
        htmltools::div(
          style = "margin-bottom: 5px;",
          htmltools::strong("Working Directory: "),
          htmltools::span(reactive_wdir(), style = "font-size: 12px; color: #666;")
        ),
        htmltools::div(
          style = "margin-bottom: 5px;",
          htmltools::strong("Engine Type: "),
          htmltools::span(engine_type, style = "color: #337ab7;")
        )
      )
      if (!is.na(engine_save_file)) {
        left_content <- append(left_content, list(
          htmltools::div(
            style = "margin-bottom: 5px;",
            htmltools::strong("Engine File: "),
            htmltools::span(engine_save_file, style = "font-size: 12px; color: #666;")
          )
        ))
      }
      right_buttons <- list(
        shinyFiles::shinyDirButton(
          ns("set_wdir_button"),
          "Change Directory",
          "Select Working Directory",
          "wd",
          style = "width: 150px; margin-bottom: 5px;",
          class = "btn-light"
        )
      )
      if (has_unsaved_changes) {
        filename <- if (is.na(engine_save_file)) engine_type else engine_save_file
        extensions <- if (grepl(".sqlite", filename)) {
          list(sqlite = "sqlite", rds = "rds")
        } else {
          list(rds = "rds", sqlite = "sqlite")
        }
        save_button <- shinyFiles::shinySaveButton(
          ns("save_engine_button_file"),
          label = "Save Engine",
          title = "Save the engine as .sqlite or .rds",
          class = "btn-warning",
          filename = gsub(".sqlite|.rds", "", basename(filename)),
          filetype = extensions,
          style = "width: 150px; margin-bottom: 5px;"
        )
        reset_button <- shiny::actionButton(
          ns("reset_engine_button"),
          label = "Discard Changes",
          width = 150,
          class = "btn-danger",
          style = "margin-bottom: 5px;"
        )
        right_buttons <- append(right_buttons, list(
          save_button,
          reset_button
        ))
      }
      shinydashboard::box(
        width = 12,
        height = "200px",
        title = "Project Control",
        solidHeader = TRUE,
        style = "margin-bottom: 10px;",
        htmltools::div(
          style = "display: flex; justify-content: space-between; align-items: flex-start; height: 100%; padding: 5px;",
          htmltools::div(
            style = "flex: 1; padding-right: 15px;",
            htmltools::tagList(left_content)
          ),
          htmltools::div(
            style = "flex-shrink: 0; display: flex; flex-direction: column; align-items: flex-end; gap: 5px;",
            htmltools::tagList(right_buttons)
          )
        )
      )
    })

    # MARK: obs Change Working Directory
    ## obs Change Working Directory -----
    shiny::observeEvent(input$set_wdir_button, {
      shiny::req(input$set_wdir_button)
      file_info <- shinyFiles::parseDirPath(
        roots = reactive_volumes(),
        input$set_wdir_button
      )
      if (length(file_info) > 0) {
        setwd(file_info)
        reactive_wdir(file_info)
        volumes <<- .app_util_get_volumes()
        reactive_volumes(volumes)
      }
    })

    # MARK: Metadata
    # Metadata -----
    output$metadata_ui <- shiny::renderUI({
      .mod_WorkflowAssembler_Metadata_Server(
        "metadata",
        ns,
        reactive_metadata,
        reactive_config
      )
      .mod_WorkflowAssembler_Metadata_UI("metadata", ns)
      # htmltools::div(
      #   style = "height: calc(100vh - 50px - 200px - 65px); overflow-y: auto; padding: 0px; box-sizing: border-box;",
      #   .mod_WorkflowAssembler_Metadata_UI("metadata", ns)
      # )
    })

    # MARK: Analyses
    # Analyses -----
    output$analyses_ui <- shiny::renderUI({
      engine_type <- reactive_engine_type()
      if (engine_type %in% "Engine") {
        shiny::showNotification(
          "Analyses not implemented for Engine without an assigned data type!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      engine_data_type <- gsub("Engine", "", engine_type)
      analyses_dummy_call <- get(
        paste0(engine_data_type, "Analyses"),
        envir = asNamespace("StreamFind")
      )
      analyses_class_dummy <<- suppressMessages(do.call(
        analyses_dummy_call,
        list()
      ))
      .mod_WorkflowAssembler_Analyses_Server(
        analyses_class_dummy,
        "analyses",
        ns,
        reactive_analyses,
        reactive_warnings,
        reactive_volumes,
        reactive_config
      )
      .mod_WorkflowAssembler_Analyses_UI(analyses_class_dummy, "analyses", ns)
    })

    # MARK: Explorer
    # Explorer -----
    output$explorer_ui <- shiny::renderUI({
      if (is.null(analyses_class_dummy)) {
        shiny::showNotification(
          "No analyses class defined!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      tryCatch(
        {
          .mod_WorkflowAssembler_Explorer_Server(
            analyses_class_dummy,
            "summary",
            ns,
            reactive_analyses,
            reactive_volumes,
            reactive_config
          )
          .mod_WorkflowAssembler_Explorer_UI(
            analyses_class_dummy,
            "summary",
            ns
          )
        },
        error = function(e) {
          msg <- paste(
            "Explorer not rendering for class ",
            class(analyses_class_dummy)[1],
            ":",
            conditionMessage(e),
            collapse = ""
          )
          shiny::showNotification(msg, duration = 10, type = "error")
          shiny::div(style = "color: red;", msg)
        }
      )
    })

    # MARK: Workflow
    # Workflow -----
    output$workflow_ui <- shiny::renderUI({
      engine_type <- reactive_engine_type()
      if (engine_type %in% "Engine") {
        shiny::showNotification(
          "Workflow not implemented for Engine without an assigned data type!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }

      .mod_WorkflowAssembler_workflow_Server(
        "workflow",
        ns,
        engine,
        engine_type,
        reactive_analyses,
        reactive_workflow,
        reactive_saved_workflow,
        reactive_results,
        reactive_audit,
        reactive_engine_config,
        reactive_warnings,
        reactive_volumes,
        reactive_config
      )

      .mod_WorkflowAssembler_workflow_UI("workflow", ns)
    })

    # MARK: Results
    # Results -----
    output$results_ui <- shiny::renderUI({
      if (reactive_engine_type() %in% "Engine") {
        shiny::showNotification(
          "Results not implemented for Engine without an assigned data type!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }

      res <- reactive_results()

      if (length(res) > 0) {
        result_methods <- methods(.mod_WorkflowAssembler_Result_Server)
        tab_list <- list()
        for (i in seq_along(res)) {
          has_result_method <- any(vapply(
            result_methods,
            function(z) grepl(class(res[[i]])[1], z),
            FALSE
          ))

          if (has_result_method) {
            .mod_WorkflowAssembler_Result_Server(
              res[[i]],
              paste0("tab_", names(res)[i]),
              ns,
              reactive_analyses,
              reactive_volumes
            )
            tab_list[[i]] <- shiny::tabPanel(
              title = class(res[[i]])[1],
              .mod_WorkflowAssembler_Result_UI(
                res[[i]],
                paste0("tab_", names(res)[i]),
                ns
              )
            )
          } else {
            shiny::showNotification(
              paste("No results method for ", class(res[[i]])[1], "!"),
              duration = 5,
              type = "warning"
            )
            tab_list[[i]] <- shiny::tabPanel(
              title = class(res[[i]])[1],
              htmltools::div(paste0(" ", i, ": ", class(res[[i]])[1]))
            )
          }
        }
        do.call(
          shinydashboard::tabBox,
          c(
            list(
              width = 12,
              height = "calc(100vh - 50px - 30px)"
            ),
            tab_list
          )
        )
      } else {
        htmltools::div(htmltools::h4("No results found!"))
      }
    })

    # MARK: AuditTrail
    # AuditTrail -----
    output$audit_ui <- DT::renderDT({
      audit_trail <- reactive_audit()
      if (length(audit_trail) > 0) {
        audit_trail <- as.data.table(audit_trail)
        audit_trail$value <- gsub("\n", "<br>", audit_trail$value)
        DT::datatable(
          audit_trail,
          filter = "top",
          selection = list(mode = "single", selected = 1, target = "row"),
          options = list(
            dom = "ft",
            paging = FALSE,
            scrollX = TRUE,
            scrollY = "calc(100vh - 50px - 30px - 20px - 170px)",
            scrollCollapse = TRUE,
            columnDefs = list(
              list(
                targets = which(names(audit_trail) == "value"),
                createdCell = DT::JS(
                  "function(td, cellData, rowData, row, col) {",
                  "  td.style.fontFamily = 'Courier, monospace';",
                  "  td.style.whiteSpace = 'pre';",
                  "}"
                )
              )
            )
          ),
          escape = FALSE
        )
      } else {
        DT::datatable(data.table::data.table())
      }
    })

    # TODO update configuration based on the golem-config.yml?
    reactive_config_change_trigger <- shiny::reactiveVal(0)

    # MARK: Config
    # Config -----
    output$config_ui <- DT::renderDT({
      config <- reactive_config()
      modified_variable_trigger <- reactive_config_change_trigger()
      DT::datatable(
        as.data.table(config),
        filter = "top",
        selection = list(mode = "single", selected = 1, target = "row"),
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "calc(100vh - 50px - 30px - 20px - 170px)",
          scrollCollapse = TRUE
        ),
        escape = FALSE,
        editable = list(target = "cell", columns = c("value"))
      )
    })

    # MARK: obs Config Table Editing
    ## obs Config Table Editing -----
    shiny::observeEvent(input$config_ui_cell_edit, {
      info <- input$config_ui_cell_edit
      info_index <- info$row
      info_value <- info$value
      config <- reactive_config()
      name_value <- config[[info_index]]$name
      tryCatch(
        {
          config_call <- names(config[info_index])[1]
          config[[config_call]] <- do.call(config_call, list(info_value))
          reactive_config(config)
        },
        error = function(e) {
          msg <- paste(
            "Error in modifying ",
            name_value,
            ":",
            conditionMessage(e)
          )
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_config_change_trigger(reactive_config_change_trigger() + 1)
        },
        warning = function(w) {
          msg <- paste(
            "Warning in modifying ",
            name_value,
            ":",
            conditionMessage(w)
          )
          shiny::showNotification(msg, duration = 10, type = "warning")
          reactive_config_change_trigger(reactive_config_change_trigger() + 1)
        }
      )
    })

    # MARK: out Cache Size
    # out Cache Size -----
    output$cache_size <- shiny::renderText({
      tryCatch(
        {
          config <- reactive_engine_config()
          audit <- reactive_audit()
          cache_config <- config[["ConfigCache"]]
          # Check if cache exists based on mode
          cache_exists <- FALSE
          if ("sqlite" %in% cache_config$mode && !is.null(cache_config$file)) {
            cache_exists <- file.exists(cache_config$file)
          } else if ("rds" %in% cache_config$mode && !is.null(cache_config$folder)) {
            cache_exists <- dir.exists(cache_config$folder)
          }
          
          if (cache_exists) {
            cache_size_result <- size(cache_config)
            if (is.numeric(cache_size_result) && !is.na(cache_size_result)) {
              # size() returns a named numeric vector with unit as name
              unit <- names(cache_size_result)
              value <- round(as.numeric(cache_size_result), 2)
              paste0(value, " ", unit)
            } else {
              "Unknown"
            }
          } else {
            "0 bytes"
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error calculating cache size:", e$message),
            duration = 5,
            type = "error"
          )
          "Error"
        }
      )
    })

    # MARK: obs Cache Size
    # obs Clear Cache -----
    shiny::observeEvent(input$clear_cache_button, {
      tryCatch(
        {
          config <- reactive_engine_config()
          if (size(config[["ConfigCache"]]) == 0) {
            shiny::showNotification(
              "Cache is already empty!",
              duration = 3
            )
            return()
          }
          clear_cache(config[["ConfigCache"]], "all")
          shiny::showNotification(
            "Cache cleared successfully!",
            duration = 3
          )
          reactive_engine_config(config)
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error clearing cache:", e$message),
            duration = 5
          )
        }
      )
    })

    # MARK: Report
    # Report -----
    output$report_ui <- shiny::renderUI({
      shinyFiles::shinyFileChoose(
        input,
        "select_qmd_file",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session,
        filetypes = c("qmd")
      )

      shinyFiles::shinyDirChoose(
        input,
        "select_execute_dir",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session
      )

      shinyFiles::shinyDirChoose(
        input,
        "select_output_dir",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session
      )

      shinydashboard::box(
        width = 12,
        title = "Quarto Report Generator",
        solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::h4("Select QMD Template"),
            shiny::p("Choose a Quarto (.qmd) document to render as a report:"),
            shinyFiles::shinyFilesButton(
              ns("select_qmd_file"),
              label = "Select QMD File",
              title = "Choose a QMD template file",
              multiple = FALSE,
              class = "btn-primary",
              style = "width: 200px; margin-bottom: 10px;"
            ),
            shiny::br(),
            shiny::verbatimTextOutput(ns("selected_file_display")),

            shiny::hr(),

            shiny::h4("Execution Directory"),
            shiny::p("Directory where the report will be executed:"),
            shinyFiles::shinyDirButton(
              ns("select_execute_dir"),
              label = "Select Execute Directory",
              title = "Choose execution directory",
              class = "btn-info",
              style = "width: 200px; margin-bottom: 10px;"
            ),
            shiny::br(),
            shiny::verbatimTextOutput(ns("execute_dir_display"))
          ),
          shiny::column(
            width = 6,
            shiny::h4("Output Configuration"),
            shiny::p("Configure where the generated report will be saved:"),

            shiny::div(
              style = "margin-bottom: 15px;",
              shiny::strong("Output Directory:"),
              shiny::br(),
              shinyFiles::shinyDirButton(
                ns("select_output_dir"),
                label = "Select Output Directory",
                title = "Choose output directory",
                class = "btn-secondary",
                style = "width: 200px; margin-top: 5px; margin-bottom: 10px;"
              ),
              shiny::br(),
              shiny::verbatimTextOutput(ns("output_dir_display"))
            ),

            shiny::div(
              style = "margin-bottom: 15px;",
              shiny::strong("Output Filename (without extension):"),
              shiny::textInput(
                ns("output_filename"),
                label = NULL,
                placeholder = "Enter filename (e.g., my_report)",
                width = "200px"
              ),
              shiny::p("Extension will be determined by the QMD header",
                      style = "font-size: 12px; color: #666; margin-top: 5px;")
            ),

            shiny::hr(),

            shiny::h4("Generate Report"),
            shiny::p("Click to render the selected QMD template:"),
            shiny::actionButton(
              ns("generate_report_button"),
              label = "Generate Report",
              class = "btn-success",
              style = "width: 150px; margin-bottom: 10px;"
            ),
            shiny::br(),
            shiny::div(
              id = ns("report_status"),
              style = "margin-top: 10px;"
            )
          )
        )
      )
    })

    # MARK: obs Select QMD File
    ## obs Select QMD File -----
    reactive_selected_qmd_file <- shiny::reactiveVal(NULL)
    reactive_execute_dir <- shiny::reactiveVal(getwd())
    reactive_output_dir <- shiny::reactiveVal(getwd())

    shiny::observeEvent(input$select_qmd_file, {
      shiny::req(input$select_qmd_file)
      file_info <- shinyFiles::parseFilePaths(
        roots = reactive_volumes(),
        input$select_qmd_file
      )
      if (nrow(file_info) > 0) {
        file_path <- file_info$datapath[1]
        reactive_selected_qmd_file(file_path)
      }
    })

    # MARK: obs Select Execute Directory
    ## obs Select Execute Directory -----
    shiny::observeEvent(input$select_execute_dir, {
      shiny::req(input$select_execute_dir)
      dir_info <- shinyFiles::parseDirPath(
        roots = reactive_volumes(),
        input$select_execute_dir
      )
      if (length(dir_info) > 0) {
        reactive_execute_dir(dir_info)
      }
    })

    # MARK: obs Select Output Directory
    ## obs Select Output Directory -----
    shiny::observeEvent(input$select_output_dir, {
      shiny::req(input$select_output_dir)
      dir_info <- shinyFiles::parseDirPath(
        roots = reactive_volumes(),
        input$select_output_dir
      )
      if (length(dir_info) > 0) {
        reactive_output_dir(dir_info)
      }
    })

    # MARK: out Selected File Display
    ## out Selected File Display -----
    output$selected_file_display <- shiny::renderText({
      selected_file <- reactive_selected_qmd_file()
      if (is.null(selected_file)) {
        "No file selected"
      } else {
        paste("Selected file:", selected_file)
      }
    })

    # MARK: out Execute Directory Display
    ## out Execute Directory Display -----
    output$execute_dir_display <- shiny::renderText({
      execute_dir <- reactive_execute_dir()
      paste("Execute directory:", execute_dir)
    })

    # MARK: out Output Directory Display
    ## out Output Directory Display -----
    output$output_dir_display <- shiny::renderText({
      output_dir <- reactive_output_dir()
      paste("Output directory:", output_dir)
    })

    # MARK: obs Generate Report
    ## obs Generate Report -----
    shiny::observeEvent(input$generate_report_button, {
      selected_file <- reactive_selected_qmd_file()
      execute_dir <- reactive_execute_dir()
      output_dir <- reactive_output_dir()
      output_filename <- input$output_filename

      # Check if a file is selected
      if (is.null(selected_file) || !file.exists(selected_file)) {
        shiny::showNotification(
          "Please select a valid QMD file first!",
          duration = 5,
          type = "error"
        )
        return()
      }

      # Check if execute directory exists
      if (!dir.exists(execute_dir)) {
        shiny::showNotification(
          "Execute directory does not exist!",
          duration = 5,
          type = "error"
        )
        return()
      }

      # Prepare output_file parameter (filename only) and handle output directory separately
      output_file <- NULL
      final_output_dir <- NULL

      if (!is.null(output_filename) && nzchar(trimws(output_filename))) {
        # Check if output directory exists
        if (!dir.exists(output_dir)) {
          shiny::showNotification(
            "Output directory does not exist!",
            duration = 5,
            type = "error"
          )
          return()
        }

        # Set filename only (without extension)
        output_file <- trimws(output_filename)
        # Store the target output directory
        final_output_dir <- output_dir
      }      # Check if engine has unsaved changes
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      if (has_unsaved_changes) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Unsaved Changes Detected",
            "The engine has unsaved changes. Please save your work before generating a report.",
            footer = shiny::tagList(
              shiny::actionButton(
                ns("save_before_report"),
                "Save & Generate Report",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            ),
            easyClose = FALSE
          )
        )
        return()
      }

      # Generate report
      tryCatch({
        shiny::showNotification(
          "Generating report... This may take a few moments.",
          duration = 5,
          type = "message"
        )

        # Update engine with current reactive values
        engine$Metadata <- reactive_metadata()
        engine$Analyses <- reactive_analyses()
        engine$Workflow <- reactive_workflow()
        engine$Analyses$results <- reactive_results()

        # Call the enhanced engine's report_quarto method
        engine$report_quarto(
          template = selected_file,
          output_file = output_file,
          output_dir = final_output_dir,
          execute_dir = execute_dir
        )

        shiny::showNotification(
          "Report generated successfully!",
          duration = 5,
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error generating report:", conditionMessage(e)),
          duration = 10,
          type = "error"
        )
      })
    })

    # MARK: obs Save Before Report
    ## obs Save Before Report -----
    shiny::observeEvent(input$save_before_report, {
      selected_file <- reactive_selected_qmd_file()
      execute_dir <- reactive_execute_dir()
      output_dir <- reactive_output_dir()
      output_filename <- input$output_filename

      tryCatch({
        # Save the engine first
        if (!is.na(reactive_engine_save_file())) {
          engine$save(reactive_engine_save_file())
        } else {
          # If no save file is set, create a temporary save
          temp_file <- tempfile(fileext = ".rds")
          engine$save(temp_file)
          reactive_engine_save_file(temp_file)
        }

        # Update reactive values to reflect saved state
        reactive_warnings(.app_util_remove_notifications(
          reactive_warnings(),
          "unsaved_changes"
        ))
        reactive_saved_metadata(reactive_metadata())
        reactive_saved_analyses(reactive_analyses())
        reactive_saved_workflow(reactive_workflow())
        reactive_saved_results(reactive_results())
        reactive_audit_saved(reactive_audit())
        reactive_engine_config_saved(reactive_engine_config())

        # Close modal
        shiny::removeModal()

        # Prepare output_file parameter (filename only) and handle output directory separately
        output_file <- NULL
        final_output_dir <- NULL

        if (!is.null(output_filename) && nzchar(trimws(output_filename))) {
          # Set filename only (without extension)
          output_file <- trimws(output_filename)
          # Store the target output directory
          final_output_dir <- output_dir
        }

        # Generate report
        shiny::showNotification(
          "Engine saved. Generating report... This may take a few moments.",
          duration = 5,
          type = "message"
        )

        # Update engine with current reactive values
        engine$Metadata <- reactive_metadata()
        engine$Analyses <- reactive_analyses()
        engine$Workflow <- reactive_workflow()
        engine$Analyses$results <- reactive_results()

        # Call the enhanced engine's report_quarto method
        engine$report_quarto(
          template = selected_file,
          output_file = output_file,
          output_dir = final_output_dir,
          execute_dir = execute_dir
        )

        shiny::showNotification(
          "Report generated successfully!",
          duration = 5,
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)),
          duration = 10,
          type = "error"
        )
        shiny::removeModal()
      })
    })
  })
}
