#' @noRd
.mod_WADB_UI <- function(id) {
  ns <- shiny::NS(id)
  list(
    shiny::tags$head(
      shiny::tags$style(htmltools::HTML("
        .box.box-solid {
          margin-bottom: 0 !important;
        }
        .content {
          padding: 5px !important;
        }
        .nav-tabs-custom {
          margin-bottom: 0 !important;
        }
        .nav-tabs-custom>.tab-content {
          padding: 5px !important;
        }
      "))
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("project"),
        shiny::fluidRow(shiny::uiOutput(ns("project_ui")))
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
          shiny::uiOutput(ns("audit_ui"))
        )
      )
    )
  )
}

#' @noRd
.mod_WADB_Server <- function(
    id,
    reactive_engine_type,
    reactive_project_path,
    reactive_warnings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # MARK: Global Constants/Mutable
    # Global Constants/Mutable -----
    pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
    volumes <- .app_util_get_volumes()
    engine <- NULL

    # MARK: Global Reactive Variables
    # Global Reactive Variables -----
    reactive_wdir <- shiny::reactiveVal(getwd())
    reactive_volumes <- shiny::reactiveVal(volumes)
    reactive_metadata <- shiny::reactiveVal(NULL)
    reactive_analyses <- shiny::reactiveVal(NULL)
    reactive_workflow <- shiny::reactiveVal(NULL)
    reactive_results <- shiny::reactiveVal(NULL)
    reactive_audit <- shiny::reactiveVal(NULL)
    reactive_update_cache_size <- shiny::reactiveVal(0)
    reactive_update_trigger <- shiny::reactiveVal(0)

    # MARK: obs Start/Load
    # obs Start/Load -----
    shiny::observeEvent(reactive_project_path(), {
      envSF <- asNamespace("StreamFind")
      engine_type <- reactive_engine_type()
      engine_call <- get(engine_type, envir = envSF)
      engine_call_new <- engine_call[["new"]]
      engine <<- suppressMessages(do.call(engine_call_new, list(projectPath = reactive_project_path())))
      reactive_metadata(engine$Metadata)
      reactive_analyses(engine$Analyses)
      reactive_workflow(engine$Workflow)
      reactive_audit(engine$AuditTrail)
    })

    # MARK: obs Update Trigger
    # obs Update Trigger -----
    shiny::observeEvent(reactive_update_trigger(), {
      if (!is.null(engine)) {
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_audit(engine$AuditTrail)
      }
    })

    # MARK: Project
    # Project -----



    # MARK: out Project UI
    # out Project UI -----
    output$project_ui <- shiny::renderUI({
      htmltools::div(
        style = "display: flex; flex-direction: column; height: calc(100vh - 60px); gap: 5px; box-sizing: border-box;",
        shinydashboard::box(
          width = 12,
          height = "300px",
          title = "Project Database Files",
          solidHeader = TRUE,
          style = "padding: 0px; box-sizing: border-box; overflow-y: auto; display: block; margin: 0;",
          shiny::uiOutput(ns("project_control_ui"))
        ),
        shinydashboard::box(
          width = 12,
          height = "calc(100vh - 60px - 300px - 10px)",
          title = "Metadata",
          solidHeader = TRUE,
          style = "padding: 0px; box-sizing: border-box; overflow-y: auto; display: block; margin: 0;",
          shiny::uiOutput(ns("metadata_ui"))
        )
      )
    })

    # MARK: out Project Control
    # out Project Control -----
    output$project_control_ui <- shiny::renderUI({
      project_path <- reactive_project_path()
      htmltools::div(
        style = "padding: 10px;",
        htmltools::div(
          style = "margin-bottom: 5px;",
          htmltools::strong("Project Path: "),
          htmltools::span(project_path)
        ),
        htmltools::div(
          style = "height: 200px; overflow-y: auto; padding: 0px;",
          shiny::uiOutput(ns("project_files_list"))
        )
      )
    })

    # MARK: out Project Files List
    # out Project Files List -----
    output$project_files_list <- shiny::renderUI({
      project_path <- reactive_project_path()
      reactive_update_trigger()
      reactive_update_cache_size()
      if (is.na(project_path) || !dir.exists(project_path)) {
        return(htmltools::div("No valid project path"))
      }
      tryCatch(
        {
          files <- list.files(project_path, full.names = TRUE, pattern = ".duckdb$")
          if (length(files) == 0) {
            return(htmltools::div("No files in project directory"))
          }
          file_info <- data.frame(
            name = basename(files),
            path = files,
            stringsAsFactors = FALSE
          )
          file_info$size <- file.size(file_info$path)
          file_info <- file_info[order(file_info$name), ]
          format_size <- function(bytes) {
            if (is.na(bytes) || bytes == 0) {
              return("0 B")
            }
            units <- c("B", "KB", "MB", "GB")
            size <- bytes
            unit_idx <- 1
            while (size >= 1024 && unit_idx < length(units)) {
              size <- size / 1024
              unit_idx <- unit_idx + 1
            }
            paste0(round(size, 2), " ", units[unit_idx])
          }
          file_items <- lapply(seq_len(nrow(file_info)), function(i) {
            htmltools::div(
              style = "font-weight: 500; color: #333; padding: 5px;",
              paste0("- ", file_info$name[i], " (", format_size(file_info$size[i]), ")")
            )
          })
          htmltools::div(file_items)
        },
        error = function(e) {
          htmltools::div(
            style = "color: red;",
            paste("Error reading project files:", conditionMessage(e))
          )
        }
      )
    })

    # MARK: Metadata
    # Metadata -----
    # Metadata DT reactive value
    metadata_dt <- shiny::reactiveVal()

    # Initialize metadata_dt when reactive_metadata changes
    shiny::observe({
      tryCatch(
        {
          meta <- reactive_metadata()
          if (!is.null(meta)) {
            dt <- as.data.table(meta)
            data.table::setnames(dt, c("Name", "Value"))
            metadata_dt(dt)
          }
        },
        error = function(e) {
          message("Error initializing metadata: ", e)
        }
      )
    })

    # Reactive to check if metadata has changed
    metadata_changed <- shiny::reactive({
      tryCatch(
        {
          dt <- data.table::copy(metadata_dt())
          data.table::setnames(dt, c("name", "value"))
          dt_meta <- as.Metadata(dt)
          saved_meta <- reactive_metadata()
          !identical(dt_meta, saved_meta)
        },
        error = function(e) {
          message("Error Metadata format: ", e)
          TRUE
        }
      )
    })

    # Render update button based on changes
    output$update_metadata_ui <- shiny::renderUI({
      if (metadata_changed()) {
        htmltools::div(
          style = "display: flex; gap: 10px;",
          shiny::actionButton(ns("update_metadata"), "Update Metadata", class = "btn-warning"),
          shiny::actionButton(ns("discard_changes"), "Discard Changes", class = "btn-danger")
        )
      }
    })

    # Render metadata UI
    output$metadata_ui <- shiny::renderUI({
      htmltools::div(
        style = "padding: 10px;",
        htmltools::div(
          style = "margin-bottom: 0px; display: flex; gap: 10px; align-items: center; padding: 0px;",
          shiny::actionButton(ns("add_row"), "Add New Row", class = "btn-light"),
          shiny::uiOutput(ns("update_metadata_ui"))
        ),
        htmltools::div(
          style = "margin-bottom: 0px; color: #666; font-size: 12px; padding: 5px;",
          shiny::HTML("<i class='fa fa-info-circle'></i> Double-click on any cell to edit its value")
        ),
        htmltools::div(
          style = "padding: 0px; box-sizing: border-box; height: calc(100vh - 60px - 300px - 150px); overflow-y: auto;",
          DT::DTOutput(ns("metadata_dt"))
        )
      )
    })

    # Render DT ----
    output$metadata_dt <- DT::renderDT(
      {
        dt_display <- metadata_dt()
        if (!is.null(dt_display) && nrow(dt_display) > 0) {
          dt_display$Delete <- paste0('<button class="btn btn-danger btn-sm delete-btn" data-row="', seq_len(nrow(dt_display)), '"><i class="fa fa-trash"></i></button>')
        }

        DT::datatable(
          dt_display,
          rownames = FALSE,
          editable = list(
            target = "cell",
            disable = list(
              columns = c(2)
            )
          ),
          selection = "none",
          escape = FALSE,
          callback = DT::JS(paste0("
            table.on('click', '.delete-btn', function() {
              var row = $(this).data('row');
              var timestamp = Date.now();
              Shiny.setInputValue('", session$ns("delete_row"), "', row + '_' + timestamp, {priority: 'event'});
            });
          ")),
          options = list(
            searching = TRUE,
            processing = TRUE,
            paging = FALSE,
            dom = "ft",
            scrollY = "calc(100vh - 60px - 300px - 230px)",
            scrollCollapse = TRUE,
            columnDefs = list(
              list(orderable = FALSE, targets = 2),
              list(width = "60px", targets = 2)
            )
          ),
          class = "cell-border stripe"
        )
      },
      server = TRUE
    )

    # Handle edits ----
    shiny::observeEvent(input$metadata_dt_cell_edit, {
      info <- input$metadata_dt_cell_edit
      dt <- metadata_dt()
      dt[info$row, (info$col + 1)] <- info$value
      metadata_dt(dt)
    })

    # Add new row ----
    shiny::observeEvent(input$add_row, {
      dt <- metadata_dt()
      if (!is.null(dt)) {
        place_holder_idx <- nrow(dt) + 1
        place_holder_name <- paste0("place_holder_", place_holder_idx)
        dt <- rbind(dt, data.table::data.table(Name = place_holder_name, Value = place_holder_name))
        dt <- dt[!duplicated(dt), ]
        metadata_dt(dt)
      }
    })

    # Delete row ----
    shiny::observeEvent(input$delete_row,
      {
        delete_info <- input$delete_row
        row_to_delete <- as.numeric(strsplit(delete_info, "_")[[1]][1])
        dt <- metadata_dt()
        if (!is.null(dt) && !is.na(row_to_delete) && row_to_delete > 0 && row_to_delete <= nrow(dt)) {
          dt <- dt[-row_to_delete, ]
          metadata_dt(dt)
        }
      },
      ignoreInit = TRUE
    )

    # Update reactive_metadata ----
    shiny::observeEvent(input$update_metadata, {
      tryCatch(
        {
          dt <- metadata_dt()
          colnames(dt) <- c("name", "value")
          dt <- dt[!dt$name %in% c("place_holder"), ]
          new_metadata <- as.Metadata(dt)
          engine$Metadata <- new_metadata
          reactive_metadata(engine$Metadata)
        },
        error = function(e) {
          message("Error updating Metadata: ", e)
        }
      )
    })

    # Discard changes ----
    shiny::observeEvent(input$discard_changes, {
      dt <- as.data.table(reactive_metadata())
      data.table::setnames(dt, c("Name", "Value"))
      metadata_dt(dt)
    })

    # MARK: Analyses
    # Analyses -----
    output$analyses_ui <- shiny::renderUI({
      analyses <- reactive_analyses()
      .mod_WADB_Analyses_Server(
        analyses,
        "analyses",
        ns,
        reactive_update_cache_size,
        reactive_analyses,
        reactive_warnings,
        reactive_volumes
      )
      .mod_WADB_Analyses_UI(analyses, "analyses", ns)
    })

    # MARK: Explorer
    # Explorer -----
    output$explorer_ui <- shiny::renderUI({
      analyses <- reactive_analyses()
      if (is.null(analyses)) {
        shiny::showNotification(
          "No analyses class defined!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      tryCatch(
        {
          .mod_WADB_Explorer_Server(
            analyses,
            "summary",
            ns,
            reactive_analyses,
            reactive_volumes
          )
          .mod_WADB_Explorer_UI(
            analyses,
            "summary",
            ns
          )
        },
        error = function(e) {
          msg <- paste(
            "Explorer not rendering for class ",
            class(analyses)[1],
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
      if (is.null(engine)) {
        return(htmltools::div("Engine not initialized!"))
      }
      if (!inherits(engine, "DB_Engine")) {
        shiny::showNotification(
          "Workflow not implemented for non-DB engines!",
          duration = 5,
          type = "warning"
        )
        return(htmltools::div(" "))
      }
      .mod_WADB_Workflow_Server(
        engine,
        "workflow",
        ns,
        reactive_analyses,
        reactive_workflow,
        reactive_results,
        reactive_audit,
        reactive_warnings,
        reactive_volumes
      )
      .mod_WADB_Workflow_UI(engine, "workflow", ns)
    })

    # # MARK: Results
    # # Results -----
    # output$results_ui <- shiny::renderUI({
    #   if (reactive_engine_type() %in% "Engine") {
    #     shiny::showNotification(
    #       "Results not implemented for Engine without an assigned data type!",
    #       duration = 5,
    #       type = "warning"
    #     )
    #     return(htmltools::div(" "))
    #   }

    #   res <- reactive_results()

    #   if (length(res) > 0) {
    #     result_methods <- methods(.mod_WorkflowAssembler_Result_Server)
    #     tab_list <- list()
    #     for (i in seq_along(res)) {
    #       has_result_method <- any(vapply(
    #         result_methods,
    #         function(z) grepl(class(res[[i]])[1], z),
    #         FALSE
    #       ))

    #       if (has_result_method) {
    #         .mod_WorkflowAssembler_Result_Server(
    #           res[[i]],
    #           paste0("tab_", names(res)[i]),
    #           ns,
    #           reactive_analyses,
    #           reactive_volumes
    #         )
    #         tab_list[[i]] <- shiny::tabPanel(
    #           title = class(res[[i]])[1],
    #           .mod_WorkflowAssembler_Result_UI(
    #             res[[i]],
    #             paste0("tab_", names(res)[i]),
    #             ns
    #           )
    #         )
    #       } else {
    #         shiny::showNotification(
    #           paste("No results method for ", class(res[[i]])[1], "!"),
    #           duration = 5,
    #           type = "warning"
    #         )
    #         tab_list[[i]] <- shiny::tabPanel(
    #           title = class(res[[i]])[1],
    #           htmltools::div(paste0(" ", i, ": ", class(res[[i]])[1]))
    #         )
    #       }
    #     }
    #     do.call(
    #       shinydashboard::tabBox,
    #       c(
    #         list(
    #           width = 12,
    #           height = "calc(100vh - 50px - 30px)"
    #         ),
    #         tab_list
    #       )
    #     )
    #   } else {
    #     htmltools::div(htmltools::h4("No results found!"))
    #   }
    # })

    # MARK: AuditTrail
    # AuditTrail -----
    output$audit_ui <- shiny::renderUI({
      shinydashboard::box(
        width = 12,
        height = "calc(100vh - 60px)",
        title = NULL,
        solidHeader = TRUE,
        style = "padding: 0px; box-sizing: border-box; overflow-y: auto; display: block; margin: 0;",
        htmltools::tagList(
          htmltools::tags$style(htmltools::HTML(
            "
            .audit-table .dataTables_wrapper {
              width: 100% !important;
            }
            .audit-table table.dataTable {
              width: 100% !important;
            }
            .audit-table table.dataTable th,
            .audit-table table.dataTable td {
              text-align: left !important;
            }
            "
          )),
          htmltools::div(
            class = "audit-table",
            style = "padding: 0px; box-sizing: border-box; height: calc(100vh - 60px); overflow-y: auto;",
            DT::DTOutput(ns("audit_ui_dt"), width = "100%")
          )
        )
      )
    })

    # MARK: out AuditTrail DT
    # out AuditTrail DT -----
    output$audit_ui_dt <- DT::renderDT({
      audit_trail <- reactive_audit()
      audit_trail <- as.data.table(audit_trail)
      # audit_trail$value <- gsub("\n", "<br>", audit_trail$value)
      DT::datatable(
        audit_trail,
        width = "100%",
        filter = "top",
        selection = list(mode = "single", selected = 1, target = "row"),
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "calc(100vh - 60px - 10px - 170px)",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = "dt-left", targets = "_all"),
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
    })

    # MARK: out Cache Size
    # out Cache Size -----
    output$cache_size <- shiny::renderText({
      tryCatch(
        {
          reactive_update_cache_size()
          engine$get_cache_size()
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

    # MARK: obs Clear Cache
    # obs Clear Cache -----
    shiny::observeEvent(input$clear_cache_button, {
      tryCatch(
        {
          engine$clear_cache()
          reactive_update_cache_size(reactive_update_cache_size() + 1)
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
    # output$report_ui <- shiny::renderUI({
    #   shinyFiles::shinyFileChoose(
    #     input,
    #     "select_qmd_file",
    #     roots = reactive_volumes(),
    #     defaultRoot = "wd",
    #     session = session,
    #     filetypes = c("qmd")
    #   )

    #   shinyFiles::shinyDirChoose(
    #     input,
    #     "select_execute_dir",
    #     roots = reactive_volumes(),
    #     defaultRoot = "wd",
    #     session = session
    #   )

    #   shinyFiles::shinyDirChoose(
    #     input,
    #     "select_output_dir",
    #     roots = reactive_volumes(),
    #     defaultRoot = "wd",
    #     session = session
    #   )

    #   shinydashboard::box(
    #     width = 12,
    #     title = "Quarto Report Generator",
    #     solidHeader = TRUE,
    #     shiny::fluidRow(
    #       shiny::column(
    #         width = 6,
    #         shiny::h4("Select QMD Template"),
    #         shiny::p("Choose a Quarto (.qmd) document to render as a report:"),
    #         shinyFiles::shinyFilesButton(
    #           ns("select_qmd_file"),
    #           label = "Select QMD File",
    #           title = "Choose a QMD template file",
    #           multiple = FALSE,
    #           class = "btn-primary",
    #           style = "width: 200px; margin-bottom: 10px;"
    #         ),
    #         shiny::br(),
    #         shiny::verbatimTextOutput(ns("selected_file_display")),

    #         shiny::hr(),

    #         shiny::h4("Execution Directory"),
    #         shiny::p("Directory where the report will be executed:"),
    #         shinyFiles::shinyDirButton(
    #           ns("select_execute_dir"),
    #           label = "Select Execute Directory",
    #           title = "Choose execution directory",
    #           class = "btn-info",
    #           style = "width: 200px; margin-bottom: 10px;"
    #         ),
    #         shiny::br(),
    #         shiny::verbatimTextOutput(ns("execute_dir_display"))
    #       ),
    #       shiny::column(
    #         width = 6,
    #         shiny::h4("Output Configuration"),
    #         shiny::p("Configure where the generated report will be saved:"),

    #         shiny::div(
    #           style = "margin-bottom: 15px;",
    #           shiny::strong("Output Directory:"),
    #           shiny::br(),
    #           shinyFiles::shinyDirButton(
    #             ns("select_output_dir"),
    #             label = "Select Output Directory",
    #             title = "Choose output directory",
    #             class = "btn-secondary",
    #             style = "width: 200px; margin-top: 5px; margin-bottom: 10px;"
    #           ),
    #           shiny::br(),
    #           shiny::verbatimTextOutput(ns("output_dir_display"))
    #         ),

    #         shiny::div(
    #           style = "margin-bottom: 15px;",
    #           shiny::strong("Output Filename (without extension):"),
    #           shiny::textInput(
    #             ns("output_filename"),
    #             label = NULL,
    #             placeholder = "Enter filename (e.g., my_report)",
    #             width = "200px"
    #           ),
    #           shiny::p("Extension will be determined by the QMD header",
    #                   style = "font-size: 12px; color: #666; margin-top: 5px;")
    #         ),

    #         shiny::hr(),

    #         shiny::h4("Generate Report"),
    #         shiny::p("Click to render the selected QMD template:"),
    #         shiny::actionButton(
    #           ns("generate_report_button"),
    #           label = "Generate Report",
    #           class = "btn-success",
    #           style = "width: 150px; margin-bottom: 10px;"
    #         ),
    #         shiny::br(),
    #         shiny::div(
    #           id = ns("report_status"),
    #           style = "margin-top: 10px;"
    #         )
    #       )
    #     )
    #   )
    # })

    # MARK: obs Select QMD File
    ## obs Select QMD File -----
    # reactive_selected_qmd_file <- shiny::reactiveVal(NULL)
    # reactive_execute_dir <- shiny::reactiveVal(getwd())
    # reactive_output_dir <- shiny::reactiveVal(getwd())

    # shiny::observeEvent(input$select_qmd_file, {
    #   shiny::req(input$select_qmd_file)
    #   file_info <- shinyFiles::parseFilePaths(
    #     roots = reactive_volumes(),
    #     input$select_qmd_file
    #   )
    #   if (nrow(file_info) > 0) {
    #     file_path <- file_info$datapath[1]
    #     reactive_selected_qmd_file(file_path)
    #   }
    # })

    # MARK: obs Select Execute Directory
    ## obs Select Execute Directory -----
    # shiny::observeEvent(input$select_execute_dir, {
    #   shiny::req(input$select_execute_dir)
    #   dir_info <- shinyFiles::parseDirPath(
    #     roots = reactive_volumes(),
    #     input$select_execute_dir
    #   )
    #   if (length(dir_info) > 0) {
    #     reactive_execute_dir(dir_info)
    #   }
    # })

    # # MARK: obs Select Output Directory
    # ## obs Select Output Directory -----
    # shiny::observeEvent(input$select_output_dir, {
    #   shiny::req(input$select_output_dir)
    #   dir_info <- shinyFiles::parseDirPath(
    #     roots = reactive_volumes(),
    #     input$select_output_dir
    #   )
    #   if (length(dir_info) > 0) {
    #     reactive_output_dir(dir_info)
    #   }
    # })

    # # MARK: out Selected File Display
    # ## out Selected File Display -----
    # output$selected_file_display <- shiny::renderText({
    #   selected_file <- reactive_selected_qmd_file()
    #   if (is.null(selected_file)) {
    #     "No file selected"
    #   } else {
    #     paste("Selected file:", selected_file)
    #   }
    # })

    # # MARK: out Execute Directory Display
    # ## out Execute Directory Display -----
    # output$execute_dir_display <- shiny::renderText({
    #   execute_dir <- reactive_execute_dir()
    #   paste("Execute directory:", execute_dir)
    # })

    # # MARK: out Output Directory Display
    # ## out Output Directory Display -----
    # output$output_dir_display <- shiny::renderText({
    #   output_dir <- reactive_output_dir()
    #   paste("Output directory:", output_dir)
    # })

    # # MARK: obs Generate Report
    # ## obs Generate Report -----
    # shiny::observeEvent(input$generate_report_button, {
    #   selected_file <- reactive_selected_qmd_file()
    #   execute_dir <- reactive_execute_dir()
    #   output_dir <- reactive_output_dir()
    #   output_filename <- input$output_filename

    #   # Check if a file is selected
    #   if (is.null(selected_file) || !file.exists(selected_file)) {
    #     shiny::showNotification(
    #       "Please select a valid QMD file first!",
    #       duration = 5,
    #       type = "error"
    #     )
    #     return()
    #   }

    #   # Check if execute directory exists
    #   if (!dir.exists(execute_dir)) {
    #     shiny::showNotification(
    #       "Execute directory does not exist!",
    #       duration = 5,
    #       type = "error"
    #     )
    #     return()
    #   }

    #   # Prepare output_file parameter (filename only) and handle output directory separately
    #   output_file <- NULL
    #   final_output_dir <- NULL

    #   if (!is.null(output_filename) && nzchar(trimws(output_filename))) {
    #     # Check if output directory exists
    #     if (!dir.exists(output_dir)) {
    #       shiny::showNotification(
    #         "Output directory does not exist!",
    #         duration = 5,
    #         type = "error"
    #       )
    #       return()
    #     }

    #     # Set filename only (without extension)
    #     output_file <- trimws(output_filename)
    #     # Store the target output directory
    #     final_output_dir <- output_dir
    #   }      # Check if engine has unsaved changes
    #   has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
    #   if (has_unsaved_changes) {
    #     shiny::showModal(
    #       shiny::modalDialog(
    #         title = "Unsaved Changes Detected",
    #         "The engine has unsaved changes. Please save your work before generating a report.",
    #         footer = shiny::tagList(
    #           shiny::actionButton(
    #             ns("save_before_report"),
    #             "Save & Generate Report",
    #             class = "btn-primary"
    #           ),
    #           shiny::modalButton("Cancel")
    #         ),
    #         easyClose = FALSE
    #       )
    #     )
    #     return()
    #   }

    #   # Generate report
    #   tryCatch({
    #     shiny::showNotification(
    #       "Generating report... This may take a few moments.",
    #       duration = 5,
    #       type = "message"
    #     )

    #     # Update engine with current reactive values
    #     engine$Metadata <- reactive_metadata()
    #     engine$Analyses <- reactive_analyses()
    #     engine$Workflow <- reactive_workflow()
    #     engine$Analyses$results <- reactive_results()

    #     # Call the enhanced engine's report_quarto method
    #     engine$report_quarto(
    #       template = selected_file,
    #       output_file = output_file,
    #       output_dir = final_output_dir,
    #       execute_dir = execute_dir
    #     )

    #     shiny::showNotification(
    #       "Report generated successfully!",
    #       duration = 5,
    #       type = "message"
    #     )
    #   }, error = function(e) {
    #     shiny::showNotification(
    #       paste("Error generating report:", conditionMessage(e)),
    #       duration = 10,
    #       type = "error"
    #     )
    #   })
    # })

    # # MARK: obs Save Before Report
    # ## obs Save Before Report -----
    # shiny::observeEvent(input$save_before_report, {
    #   selected_file <- reactive_selected_qmd_file()
    #   execute_dir <- reactive_execute_dir()
    #   output_dir <- reactive_output_dir()
    #   output_filename <- input$output_filename

    #   tryCatch({
    #     # Save the engine first
    #     if (!is.na(reactive_project_path())) {
    #       engine$save(reactive_project_path())
    #     } else {
    #       # If no save file is set, create a temporary save
    #       temp_file <- tempfile(fileext = ".rds")
    #       engine$save(temp_file)
    #       reactive_project_path(temp_file)
    #     }

    #     # Update reactive values to reflect saved state
    #     reactive_warnings(.app_util_remove_notifications(
    #       reactive_warnings(),
    #       "unsaved_changes"
    #     ))
    #     reactive_saved_metadata(reactive_metadata())
    #     reactive_saved_analyses(reactive_analyses())
    #     reactive_saved_workflow(reactive_workflow())
    #     reactive_saved_results(reactive_results())
    #     reactive_audit_saved(reactive_audit())
    #     reactive_engine_config_saved(reactive_engine_config())

    #     # Close modal
    #     shiny::removeModal()

    #     # Prepare output_file parameter (filename only) and handle output directory separately
    #     output_file <- NULL
    #     final_output_dir <- NULL

    #     if (!is.null(output_filename) && nzchar(trimws(output_filename))) {
    #       # Set filename only (without extension)
    #       output_file <- trimws(output_filename)
    #       # Store the target output directory
    #       final_output_dir <- output_dir
    #     }

    #     # Generate report
    #     shiny::showNotification(
    #       "Engine saved. Generating report... This may take a few moments.",
    #       duration = 5,
    #       type = "message"
    #     )

    #     # Update engine with current reactive values
    #     engine$Metadata <- reactive_metadata()
    #     engine$Analyses <- reactive_analyses()
    #     engine$Workflow <- reactive_workflow()
    #     engine$Analyses$results <- reactive_results()

    #     # Call the enhanced engine's report_quarto method
    #     engine$report_quarto(
    #       template = selected_file,
    #       output_file = output_file,
    #       output_dir = final_output_dir,
    #       execute_dir = execute_dir
    #     )

    #     shiny::showNotification(
    #       "Report generated successfully!",
    #       duration = 5,
    #       type = "message"
    #     )
    #   }, error = function(e) {
    #     shiny::showNotification(
    #       paste("Error:", conditionMessage(e)),
    #       duration = 10,
    #       type = "error"
    #     )
    #     shiny::removeModal()
    #   })
    # })
  })
}
