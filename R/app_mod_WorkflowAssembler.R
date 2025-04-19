#' @noRd
.mod_WorkflowAssembler_UI <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = ns("project"),
      shiny::fluidRow(
        shiny::uiOutput(ns("wdir")),
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("save_engine")), shiny::uiOutput(ns("reset_engine"))
        ),
        shiny::uiOutput(ns("engine_save_file_ui")),
        shiny::fluidRow(shiny::uiOutput(ns("metadata_ui")))
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
.mod_WorkflowAssembler_Server <- function(id,
                                          reactive_clean_start,
                                          reactive_engine_type,
                                          reactive_engine_save_file,
                                          reactive_warnings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # _Global Constants/Mutable -----
    pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
    volumes <- .app_util_get_volumes()
    engine <- NULL
    analyses_class_dummy <- NULL
    
    # _Global Reactive Variables -----
    reactive_wdir <- shiny::reactiveVal(getwd())
    reactive_volumes <- shiny::reactiveVal(volumes)
    reactive_metadata <- shiny::reactiveVal(NULL)
    reactive_analyses <- shiny::reactiveVal(NULL)
    reactive_workflow <- shiny::reactiveVal(NULL)
    reactive_results <- shiny::reactiveVal(NULL)
    reactive_audit <- shiny::reactiveVal(NULL)
    reactive_saved_metadata <- shiny::reactiveVal(NULL)
    reactive_saved_analyses <- shiny::reactiveVal(NULL)
    reactive_saved_workflow <- shiny::reactiveVal(NULL)
    reactive_saved_results <- shiny::reactiveVal(NULL)
    reactive_audit_saved <- shiny::reactiveVal(NULL)
    reactive_config <- shiny::reactiveVal(AppConfig())
    
    ## obs Engine Save File -----
    shiny::observeEvent(reactive_engine_save_file(), {
      engine_save_file <- reactive_engine_save_file()
      if (!is.na(engine_save_file)) {
        if (!grepl(".sqlite$|.rds$", engine_save_file)) {
          msg <- paste("The file", engine_save_file, "is not an sqlite or rds file!")
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_engine_save_file(NA_character_)
        }
      }
    })
    
    ## obs Clean Start -----
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
        analyses_class_dummy <<- suppressMessages(do.call(analyses_call, list()))
        
        if (!is.na(reactive_engine_save_file())) {
          tryCatch({
            engine$load(reactive_engine_save_file())
          }, warning = function(w) {
            msg <- paste("Warning for", reactive_engine_save_file(), ":", conditionMessage(w))
            reactive_warnings(.app_util_add_notifications(reactive_warnings(), "load_engine", msg))
          }, error = function(e) {
            msg <- paste("Error for", reactive_engine_save_file(), ":", conditionMessage(e))
            reactive_warnings(.app_util_add_notifications(reactive_warnings(), "load_engine", msg))
          })
        }
        
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
        reactive_clean_start(FALSE)
      }
    })
    
    # _Warnings -----
    
    ## obs Unsaved engine -----
    shiny::observe({
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      equal_history <- all(
        identical(reactive_metadata(), reactive_saved_metadata()),
        identical(reactive_analyses(), reactive_saved_analyses()),
        identical(reactive_workflow(), reactive_saved_workflow()),
        identical(reactive_results(), reactive_saved_results()),
        identical(reactive_audit(), reactive_audit_saved())
      )
      if (!equal_history && !has_unsaved_changes) {
        reactive_warnings(
          .app_util_add_notifications(
            reactive_warnings(),
            "unsaved_changes",
            "Unsaved changes in the engine!")
          )
      }
      if (equal_history) {
        reactive_warnings(.app_util_remove_notifications(reactive_warnings(), "unsaved_changes"))
      }
    })
    
    ## out Save engine -----
    output$save_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        shinyFiles::shinyFileSave(
          input,
          "save_engine_button_file",
          roots = reactive_volumes(),
          defaultRoot = "wd",
          session = session
        )
        filename <- reactive_engine_save_file()
        if (is.na(filename)) filename <- reactive_engine_type()
        
        if (grepl(".sqlite", filename)) {
          extensions <- list(sqlite = "sqlite", rds = "rds")
        } else {
          extensions <- list(rds = "rds", sqlite = "sqlite")
        }
        
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinySaveButton(
            ns("save_engine_button_file"),
            label = "Save Engine",
            title = "Save the engine as .sqlite or .rds",
            class = "btn-success",
            filename = gsub(".sqlite|.rds", "", basename(filename)),
            filetype = extensions, style = "width: 200px;")
        )
      }
    })
    
    ## event Save -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(reactive_engine_save_file())
      reactive_warnings(.app_util_remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_metadata(engine$Metadata)
      reactive_analyses(engine$Analyses)
      reactive_workflow(engine$Workflow)
      reactive_results(engine$Analyses$results)
      reactive_audit(engine$AuditTrail)
      reactive_saved_metadata(engine$Metadata)
      reactive_saved_analyses(engine$Analyses)
      reactive_saved_workflow(engine$workflow)
      reactive_saved_results(engine$Analyses$results)
      reactive_audit_saved(engine$AuditTrail)
    })
    
    ## event Save Engine File -----
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
        reactive_warnings(.app_util_remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_engine_save_file(engine$Metadata$file)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
      }
    })
    
    ## out Reset engine -----
    output$reset_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton(
            ns("reset_engine_button"),
            label = "Discard Changes",
            width = 200,
            class = "btn-danger"
          )
        )
      }
    })
    
    ## event Reset -----
    shiny::observeEvent(input$reset_engine_button, {
      if (is.na(reactive_engine_save_file())) {
        reactive_warnings(.app_util_remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
      } else {
        engine$load(reactive_engine_save_file())
        reactive_warnings(.app_util_remove_notifications(reactive_warnings(), "unsaved_changes"))
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_results(engine$Analyses$results)
        reactive_audit(engine$AuditTrail)
        reactive_saved_metadata(engine$Metadata)
        reactive_saved_analyses(engine$Analyses)
        reactive_saved_workflow(engine$Workflow)
        reactive_saved_results(engine$Analyses$results)
        reactive_audit_saved(engine$AuditTrail)
      }
    })
    
    # _Project -----
    
    ## out Working Directory -----
    output$wdir <- shiny::renderUI({
      shinyFiles::shinyDirChoose(
        input,
        "set_wdir_button",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session
      )
      shinydashboard::box(
        width = 12,
        title = "Working Directory",
        solidHeader = TRUE,
        shinyFiles::shinyDirButton(
          ns("set_wdir_button"),
          "Change Working Directory",
          "Select Working Directory",
          "wd",
          style = "width: 200px;"
        ),
        htmltools::HTML(paste("  ", reactive_wdir()))
      )
    })
    
    ## event Change Working Directory -----
    shiny::observeEvent(input$set_wdir_button, {
      shiny::req(input$set_wdir_button)
      file_info <- shinyFiles::parseDirPath(roots = reactive_volumes(), input$set_wdir_button)
      if (length(file_info) > 0) {
        setwd(file_info)
        reactive_wdir(file_info)
        volumes <<- .app_util_get_volumes()
        reactive_volumes(volumes)
      }
    })
    
    ## out Engine Save File -----
    output$engine_save_file_ui <- shiny::renderUI({
      if (!is.na(reactive_engine_save_file())) {
        shinydashboard::box(width = 12, title =  "Engine Save File", solidHeader = TRUE,
          htmltools::div(style = "margin-bottom: 20px;", shiny::p(reactive_engine_save_file()))
        )
      }
    })
    
    ## module Headers -----
    output$metadata_ui <- shiny::renderUI({
      .mod_WorkflowAssembler_Metadata_Server("metadata", ns, reactive_metadata, reactive_config)
      .mod_WorkflowAssembler_Metadata_UI("metadata", ns)
    })
    
    ## _Analyses -----
    output$analyses_ui <- shiny::renderUI({
      if (reactive_engine_type() %in% "CoreEngine") {
        shiny::showNotification(
          "Analyses not implemented for CoreEngine",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      .mod_WorkflowAssembler_Analyses_Server(
        "analyses",
        ns,
        reactive_analyses,
        reactive_warnings,
        reactive_volumes,
        reactive_config
      )
      .mod_WorkflowAssembler_Analyses_UI("analyses", ns)
    })
    
    # _Explorer -----
    output$explorer_ui <- shiny::renderUI({
      if (is.null(analyses_class_dummy)) {
        shiny::showNotification("No analyses class defined!", duration = 5, type = "warning")
        return(htmltools::div(" "))
      }
      
      tryCatch({
        .mod_WorkflowAssembler_Explorer_Server(
          analyses_class_dummy,
          "summary",
          ns,
          reactive_analyses,
          reactive_volumes,
          reactive_config
        )
        .mod_WorkflowAssembler_Explorer_UI(analyses_class_dummy, "summary", ns)
      }, error = function(e) {
        msg <- paste(
          "Explorer not rendering for class ",
          class(analyses_class_dummy)[1], ":",
          conditionMessage(e),
          collapse = ""
        )
        shiny::showNotification(msg, duration = 10, type = "error")
        shiny::div(style = "color: red;", msg)
      })
    })
    
    # _Workflow -----
    output$workflow_ui <- shiny::renderUI({
      engine_type <- reactive_engine_type()
      if (engine_type %in% "CoreEngine") {
        shiny::showNotification(
          "Workflow not implemented for CoreEngine",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      
      .mod_WorkflowAssembler_workflow_Server(
        "workflow", ns,
        engine, engine_type,
        reactive_analyses,
        reactive_workflow,
        reactive_saved_workflow,
        reactive_results,
        reactive_audit,
        reactive_warnings,
        reactive_volumes,
        reactive_config
      )
      
      .mod_WorkflowAssembler_workflow_UI("workflow", ns)
    })
    
    # _Results -----
    output$results_ui <- shiny::renderUI({
      if (reactive_engine_type() %in% "CoreEngine") {
        shiny::showNotification(
          "Results not implemented for CoreEngine",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      res <- reactive_results()
      
      if (length(res) > 0) {
        result_methods <- capture.output(.mod_WorkflowAssembler_Result_Server)
        tab_list <- list()
        for (i in seq_along(res)) {
          has_result_method <- any(
            vapply(result_methods, function(z) grepl(class(res[[1]])[1], z), FALSE)
          )
          
          if (has_result_method) {
            
            .mod_WorkflowAssembler_Result_Server(
              res[[i]], paste0("tab_", names(res)[i]),
              ns,
              reactive_analyses,
              reactive_volumes,
              reactive_config
            )
            
            tab_list[[i]] <- shiny::tabPanel(
              title = names(res)[i],
              .mod_WorkflowAssembler_Result_UI(res[[i]], paste0("tab_", names(res)[i]), ns)
            )
          } else {
            shiny::showNotification(
              paste("No results method for", names(res)[i], "!"),
              duration = 5,
              type = "warning"
            )
            tab_list[[i]] <- shiny::tabPanel(
              title = class(res[[i]])[1],
              htmltools::div(paste0(" ", i, ": ", names(res)[i]))
            )
          }
        }
        do.call(shinydashboard::tabBox, c(list(width = 12), tab_list))
      } else {
        htmltools::div(htmltools::h4("No results found!"))
      }
    })
    
    # _Audit -----
    output$audit_ui <- DT::renderDT({
      audit_trail <- reactive_audit()
      if (length(audit_trail) > 0) {
        audit_trail <- as.data.frame(audit_trail)
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
    
    # _Config -----
    output$config_ui <- DT::renderDT({
      config <- reactive_config()
      modified_variable_trigger <- reactive_config_change_trigger()
      DT::datatable(
        config@config_frame,
        filter = "top",
        selection = list(mode = "single", selected = 1, target = "row"),
        options = list(pageLength = 15),
        escape = FALSE,
        editable = list(target = "cell", columns = c("value"))
      )
    })
    
    # event Config Table Editing -----
    shiny::observeEvent(input$config_ui_cell_edit, {
      info <- input$config_ui_cell_edit
      info_index <- info$row
      info_value <- info$value
      config <- reactive_config()
      name_value <- config@parameters[[info_index]]@name
      tryCatch(
        {
          config_call <- names(config@parameters[info_index])[1]
          config@parameters[[config_call]] <- do.call(config_call, list(info_value))
          reactive_config(config)
        },
        error = function(e) {
          msg <- paste("Error in modifying ", name_value, ":", conditionMessage(e))
          shiny::showNotification(msg, duration = 10, type = "error")
          reactive_config_change_trigger(reactive_config_change_trigger() + 1)
        },
        warning = function(w) {
          msg <- paste("Warning in modifying ", name_value, ":", conditionMessage(w))
          shiny::showNotification(msg, duration = 10, type = "warning")
          reactive_config_change_trigger(reactive_config_change_trigger() + 1)
        }
      )
    })
  })
}
