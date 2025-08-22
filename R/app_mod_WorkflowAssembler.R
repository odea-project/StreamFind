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
      tabName = ns("audit"),
      shiny::fluidRow(DT::dataTableOutput(ns("audit_ui")))
    ),
    shinydashboard::tabItem(
      tabName = ns("config"),
      shiny::fluidRow(DT::dataTableOutput(ns("config_ui")))
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
            function(z) grepl(class(res[[1]])[1], z),
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
              paste("No results method for", class(res[[i]]), "!"),
              duration = 5,
              type = "warning"
            )
            tab_list[[i]] <- shiny::tabPanel(
              title = class(res[[i]])[1],
              htmltools::div(paste0(" ", i, ": ", class(res[[i]])[1]))
            )
          }
        }
        shiny::div(
          class = "results-wrapper",
          do.call(
            shiny::tabsetPanel,
            c(list(type = "tabs", id = "results_tabs"), tab_list)
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
            pageLength = 15,
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
        options = list(pageLength = 15),
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
          cache_size <- size(config[["ConfigCache"]])
          if (is.numeric(cache_size)) {
            if (cache_size >= 1024^3) {
              paste0(round(cache_size / (1024^3), 2), " GB")
            } else if (cache_size >= 1024^2) {
              paste0(round(cache_size / (1024^2), 2), " MB")
            } else if (cache_size >= 1024) {
              paste0(round(cache_size / 1024, 2), " KB")
            } else {
              paste0(round(cache_size, 0), " bytes")
            }
          } else {
            as.character(cache_size)
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error calculating cache size:", e$message),
            duration = 5,
            type = "error"
          )
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
  })
}
