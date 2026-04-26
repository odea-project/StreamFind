#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  # MARK: Global Constants/Mutable
  pkg_resources <- system.file(package = "StreamFind", dir = "extdata")
  volumes <- .app_util_get_volumes()
  engine <- NULL

  # MARK: Global Reactive Variables
  reactive_engine_type <- shiny::reactiveVal(NA_character_)
  reactive_project_path <- shiny::reactiveVal(NA_character_)
  reactive_show_init_modal <- shiny::reactiveVal(FALSE)

  # Pre-populate from golem options when launched via ms$run_app()
  .init_project_path <- golem::get_golem_options("projectPath")
  .init_engine_type  <- golem::get_golem_options("engine_type")
  if (!is.null(.init_project_path) && !is.na(.init_project_path) && dir.exists(.init_project_path)) {
    reactive_project_path(.init_project_path)
  }
  if (!is.null(.init_engine_type) && !is.na(.init_engine_type)) {
    reactive_engine_type(.init_engine_type)
  }
  reactive_warnings <- shiny::reactiveVal(list())
  reactive_wdir <- shiny::reactiveVal(getwd())
  reactive_volumes <- shiny::reactiveVal(volumes)
  reactive_metadata <- shiny::reactiveVal(NULL)
  reactive_analyses <- shiny::reactiveVal(NULL)
  reactive_workflow <- shiny::reactiveVal(NULL)
  reactive_results <- shiny::reactiveVal(NULL)
  reactive_audit <- shiny::reactiveVal(NULL)
  reactive_app_mode <- shiny::reactiveVal(NA_character_)
  reactive_update_cache_size <- shiny::reactiveVal(0)
  reactive_update_trigger <- shiny::reactiveVal(0)

  make_module_id <- function(prefix, engine_type = NA_character_, project_path = NA_character_) {
    if (is.null(engine_type) || is.na(engine_type)) engine_type <- ""
    if (is.null(project_path) || is.na(project_path)) project_path <- ""
    key <- paste(prefix, engine_type, project_path, sep = "_")
    key <- gsub("[^A-Za-z0-9_]+", "_", key)
    key <- gsub("_+", "_", key)
    key
  }

  load_project_results <- function(project_path, engine_type) {
    if (is.null(project_path) || is.na(project_path) || !dir.exists(project_path)) {
      return(list())
    }
    dto <- DataTypeObjects()
    data_type <- names(dto$engine)[match(engine_type, dto$engine)]
    result_classes <- dto$results[[data_type]]
    if (length(result_classes) == 0 || all(is.na(result_classes))) {
      return(list())
    }
    results_list <- list()
    for (cls in result_classes) {
      db_path <- file.path(project_path, paste0(cls, ".duckdb"))
      if (!file.exists(db_path)) next
      ctor <- get0(cls, envir = asNamespace("StreamFind"), inherits = FALSE)
      if (!is.function(ctor)) next
      ctor_formals <- names(formals(ctor))
      obj <- tryCatch(
        {
          if ("projectPath" %in% ctor_formals) {
            ctor(projectPath = project_path)
          } else {
            ctor(db = db_path)
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error loading", cls, "from", basename(db_path), ":", conditionMessage(e)),
            duration = 5,
            type = "error"
          )
          NULL
        }
      )
      if (!is.null(obj)) {
        results_list[[cls]] <- obj
      }
    }
    results_list
  }

  # MARK: out App Mode UI
  output$app_mode_ui <- shiny::renderUI({
    shiny::tags$span("StreamFind")
  })

  # MARK: out Notifications UI
  output$notifications_ui <- shiny::renderUI({
    warnings <- reactive_warnings()
    count <- length(warnings)
    bell_btn <- htmltools::tags$button(
      class = "sf-topbar-btn",
      onclick = "var dd = document.getElementById('sf-notif-dropdown'); if(dd) dd.classList.toggle('open');",
      title = "Notifications",
      shiny::icon("bell"),
      if (count > 0) htmltools::tags$span(class = "sf-notif-count visible", count)
    )
    items <- if (count == 0) {
      htmltools::div(class = "sf-notif-empty", "No notifications")
    } else {
      lapply(warnings, function(w) htmltools::div(class = "sf-notif-item", w))
    }
    htmltools::div(
      class = "sf-notif-wrapper",
      bell_btn,
      htmltools::div(
        id = "sf-notif-dropdown",
        class = "sf-notif-dropdown",
        items
      )
    )
  })

  # MARK: obs Theme Toggle
  shiny::observeEvent(input$theme_toggle, {
    session$sendCustomMessage("toggleTheme", list())
  })

  # MARK: obs Settings Modal
  shiny::observeEvent(input$settings_button, {
    shiny::showModal(shiny::modalDialog(
      title = "Settings",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      htmltools::div(
        class = "sf-settings-modal",
        htmltools::tags$h4("Appearance"),
        shiny::actionButton(
          "theme_toggle",
          label = "Toggle Light / Dark Mode",
          icon = shiny::icon("circle-half-stroke"),
          class = "btn-primary"
        ),
        htmltools::tags$hr(),
        htmltools::tags$p(
          class = "sf-settings-note",
          "Additional configuration options can be added here later."
        )
      )
    ))
  })

  # MARK: obs Start/Load
  shiny::observeEvent(reactive_project_path(), {
    engine_type <- reactive_engine_type()
    project_path <- reactive_project_path()
    shiny::req(!is.na(engine_type), !is.na(project_path), dir.exists(project_path))
    session$sendCustomMessage("setBootOverlay", list(visible = TRUE))

    tryCatch(
      {
        envSF <- asNamespace("StreamFind")
        engine_call <- get(engine_type, envir = envSF)
        engine_call_new <- engine_call[["new"]]
        engine <<- suppressMessages(do.call(engine_call_new, list(projectPath = project_path)))
        reactive_update_cache_size(reactive_update_cache_size() + 1)
        reactive_metadata(engine$Metadata)
        reactive_analyses(engine$Analyses)
        reactive_workflow(engine$Workflow)
        reactive_audit(engine$AuditTrail)
        reactive_results(load_project_results(project_path, engine_type))

        session$onFlushed(function() {
          session$sendCustomMessage("setBootOverlay", list(visible = FALSE))
        }, once = TRUE)
      },
      error = function(e) {
        session$sendCustomMessage("setBootOverlay", list(visible = FALSE))
        shiny::showNotification(
          paste("Error loading project:", conditionMessage(e)),
          duration = 10,
          type = "error"
        )
      }
    )
  }, ignoreInit = FALSE)

  # MARK: obs Update Trigger
  shiny::observeEvent(reactive_update_trigger(), {
    if (!is.null(engine)) {
      reactive_metadata(engine$Metadata)
      reactive_analyses(engine$Analyses)
      reactive_workflow(engine$Workflow)
      reactive_audit(engine$AuditTrail)
      reactive_results(load_project_results(reactive_project_path(), reactive_engine_type()))
    }
  })

  # MARK: out Project UI
  output$project_ui <- shiny::renderUI({
    htmltools::div(
      style = "height: calc(100vh - 35px); display: flex; flex-direction: column; overflow: hidden;",
      # Section 1: Project files (fixed height)
      htmltools::div(
        style = "flex: 0 0 auto; padding: 8px 10px;",
        shiny::uiOutput("project_control_ui")
      ),
      # Section 2: Metadata (fills remaining space)
      htmltools::div(
        style = "flex: 1; overflow: hidden; padding: 0 10px 8px;",
        shiny::uiOutput("metadata_ui")
      )
    )
  })

  # MARK: out Project Control UI
  output$project_control_ui <- shiny::renderUI({
    project_path <- reactive_project_path()
    htmltools::div(
      htmltools::div(
        style = "display: flex; align-items: center; gap: 8px; font-size: 12px; flex-wrap: nowrap;",
        htmltools::div(
          style = "flex-shrink: 0;",
          htmltools::strong("Project Path: "),
          htmltools::span(project_path, style = "word-break: break-all;")
        ),
        htmltools::div(
          style = "flex-shrink: 0; margin-left: auto;",
          shiny::uiOutput("project_files_list")
        )
      )
    )
  })

  # MARK: out Project Files List
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
          return(htmltools::div(
            class = "sf-info-text",
            shiny::icon("circle-info"),
            " No database files in project directory"
          ))
        }
        file_info <- data.frame(
          name = basename(files),
          path = files,
          stringsAsFactors = FALSE
        )
        file_info$size <- file.size(file_info$path)
        file_info <- file_info[order(file_info$name), ]
        format_size <- function(bytes) {
          if (is.na(bytes) || bytes == 0) return("0 B")
          units <- c("B", "KB", "MB", "GB")
          size <- bytes
          unit_idx <- 1
          while (size >= 1024 && unit_idx < length(units)) {
            size <- size / 1024
            unit_idx <- unit_idx + 1
          }
          paste0(round(size, 2), " ", units[unit_idx])
        }
        db_icons <- lapply(seq_len(nrow(file_info)), function(i) {
          nm <- file_info$name[i]
          sz <- format_size(file_info$size[i])
          tooltip_text <- paste0(nm, "\n", sz)
          htmltools::div(
            class = "sf-db-icon-wrap",
            title = tooltip_text,
            shiny::icon("database")
          )
        })
        htmltools::div(
          class = "sf-db-icon-row",
          db_icons
        )
      },
      error = function(e) {
        htmltools::div(style = "color: red;", paste("Error reading project files:", conditionMessage(e)))
      }
    )
  })

  # MARK: Metadata
  metadata_dt <- shiny::reactiveVal()

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
      error = function(e) message("Error initializing metadata: ", e)
    )
  })

  metadata_changed <- shiny::reactive({
    tryCatch(
      {
        dt <- metadata_dt()
        if (is.null(dt)) return(FALSE)
        dt <- data.table::copy(dt)
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

  output$update_metadata_ui <- shiny::renderUI({
    if (metadata_changed()) {
      htmltools::div(
        style = "display: flex; gap: 10px;",
        shiny::actionButton("update_metadata", "Update Metadata"),
        shiny::actionButton("discard_changes", "Discard Changes", class = "btn-danger")
      )
    }
  })

  output$metadata_ui <- shiny::renderUI({
    htmltools::div(
      style = "display: flex; flex-direction: column; height: 100%; overflow: hidden;",
      htmltools::div(
        style = "flex: 0 0 auto; display: flex; gap: 10px; align-items: center; padding: 0 0 4px 0;",
        shiny::actionButton("add_row", "Add New Row", class = "btn-light"),
        shiny::uiOutput("update_metadata_ui")
      ),
      htmltools::div(
        class = "sf-info-text",
        style = "flex: 0 0 auto; padding: 2px 0 4px 0;",
        shiny::HTML("<i class='fa fa-info-circle'></i> Double-click on any cell to edit its value")
      ),
      htmltools::div(
        style = "flex: 1; overflow: hidden;",
        DT::DTOutput("metadata_dt")
      )
    )
  })

  output$metadata_dt <- DT::renderDT(
    {
      dt_display <- metadata_dt()
      if (!is.null(dt_display) && nrow(dt_display) > 0) {
        dt_display$Delete <- paste0(
          '<button class="btn btn-danger btn-sm delete-btn" data-row="',
          seq_len(nrow(dt_display)), '"><i class="fa fa-trash"></i></button>'
        )
      }
      DT::datatable(
        dt_display,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(2))),
        selection = "none",
        escape = FALSE,
        callback = DT::JS(paste0("
          table.on('click', '.delete-btn', function() {
            var row = $(this).data('row');
            var timestamp = Date.now();
            Shiny.setInputValue('delete_row', row + '_' + timestamp, {priority: 'event'});
          });
        ")),
        options = list(
          searching = TRUE,
          processing = TRUE,
          paging = FALSE,
          dom = "ft",
          scrollY = "calc(100vh - 35px - 310px)",
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

  shiny::observeEvent(input$metadata_dt_cell_edit, {
    info <- input$metadata_dt_cell_edit
    dt <- metadata_dt()
    dt[info$row, (info$col + 1)] <- info$value
    metadata_dt(dt)
  })

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
      error = function(e) message("Error updating Metadata: ", e)
    )
  })

  shiny::observeEvent(input$discard_changes, {
    dt <- as.data.table(reactive_metadata())
    data.table::setnames(dt, c("Name", "Value"))
    metadata_dt(dt)
  })

  # MARK: Analyses
  output$analyses_ui <- shiny::renderUI({
    analyses <- reactive_analyses()
    if (is.null(analyses)) {
      return(htmltools::div("No analyses loaded yet."))
    }
    .mod_Analyses_Server(
      analyses, "analyses", session$ns,
      reactive_update_cache_size, reactive_analyses,
      reactive_warnings, reactive_volumes
    )
    .mod_Analyses_UI(analyses, "analyses", session$ns)
  })

  # MARK: Explorer
  output$explorer_ui <- shiny::renderUI({
    analyses <- reactive_analyses()
    if (is.null(analyses)) {
      shiny::showNotification("No analyses class defined!", duration = 5, type = "warning")
      return(htmltools::div(" "))
    }
    tryCatch(
      {
        .mod_Explorer_Server(analyses, "explorer", session$ns, reactive_analyses, reactive_volumes)
        .mod_Explorer_UI(analyses, "explorer", session$ns)
      },
      error = function(e) {
        msg <- paste("Explorer not rendering for class", class(analyses)[1], ":", conditionMessage(e))
        shiny::showNotification(msg, duration = 10, type = "error")
        shiny::div(style = "color: red;", msg)
      }
    )
  })

  # MARK: Workflow
  output$workflow_ui <- shiny::renderUI({
    project_path <- reactive_project_path()
    engine_type <- reactive_engine_type()
    if (is.null(engine)) return(htmltools::div("Engine not initialized!"))
    module_id <- make_module_id("workflow", engine_type, project_path)
    .mod_Workflow_Server(
      engine, module_id, session$ns,
      reactive_workflow, reactive_warnings,
      reactive_volumes, reactive_update_trigger
    )
    .mod_Workflow_UI(engine, module_id, session$ns)
  })

  # MARK: Results Sidebar Sub-Nav
  output$results_sidebar_subnav <- shiny::renderUI({
    res <- reactive_results()
    if (length(res) == 0) return(NULL)
    session$sendCustomMessage("activateFirstSubtab", "results")
    sub_btns <- lapply(seq_along(res), function(i) {
      cls <- class(res[[i]])[1]
      tab_id <- paste0("tab_", names(res)[i])
      lbl <- switch(cls,
        MassSpecResults_Chromatograms     = "Chromatograms",
        MassSpecResults_NonTargetAnalysis = "Non-Target",
        gsub("_", " ", names(res)[i], fixed = TRUE)
      )
      htmltools::tags$button(
        class = if (i == 1) "sf-sub-btn active" else "sf-sub-btn",
        `data-tab`    = "results",
        `data-subtab` = tab_id,
        title         = lbl,
        lbl
      )
    })
    htmltools::div(
      class = "sf-sub-menu",
      sub_btns
    )
  })

  # MARK: Results
  output$results_ui <- shiny::renderUI({
    res <- reactive_results()
    if (length(res) == 0) return(htmltools::div(htmltools::h4("No results found!")))
    panels <- lapply(seq_along(res), function(i) {
      res_obj <- res[[i]]
      cls <- class(res_obj)[1]
      tab_id <- paste0("tab_", names(res)[i])
      ui_fun     <- get0(paste0(".mod_Result_UI.",     cls), mode = "function")
      server_fun <- get0(paste0(".mod_Result_Server.", cls), mode = "function")
      if (is.function(server_fun)) {
        server_fun(res_obj, tab_id, session$ns, reactive_analyses, reactive_volumes)
      }
      shiny::conditionalPanel(
        paste0("input.sf_active_subtab === '", tab_id, "'"),
        if (is.function(ui_fun)) ui_fun(res_obj, tab_id, session$ns)
        else htmltools::div(paste0("No results UI available for ", cls))
      )
    })
    htmltools::div(panels)
  })

  # MARK: Audit Trail
  output$audit_ui <- shiny::renderUI({
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML("
        .audit-table .dataTables_wrapper { width: 100% !important; }
        .audit-table table.dataTable { width: 100% !important; }
        .audit-table table.dataTable th,
        .audit-table table.dataTable td { text-align: left !important; }
      ")),
      htmltools::div(
        class = "audit-table",
        style = "padding: 0px; box-sizing: border-box; height: calc(100vh - 35px); overflow-y: auto;",
        DT::DTOutput("audit_ui_dt", width = "100%")
      )
    )
  })

  output$audit_ui_dt <- DT::renderDT({
    audit_trail <- reactive_audit()
    audit_trail <- as.data.table(audit_trail)
    DT::datatable(
      audit_trail,
      width = "100%",
      filter = "top",
      selection = list(mode = "single", selected = 1, target = "row"),
      options = list(
        dom = "ft",
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "calc(100vh - 35px - 10px - 170px)",
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
  output$engine_data_type <- shiny::renderUI({
    engine_type <- reactive_engine_type()
    if (is.null(engine_type) || is.na(engine_type) || identical(engine_type, "")) {
      return(NULL)
    }

    dto <- DataTypeObjects()
    data_type <- names(dto$engine)[match(engine_type, dto$engine)]

    # Fallback for cases where the engine type is available but not in DTO map.
    if (is.null(data_type) || length(data_type) == 0 || is.na(data_type) || identical(data_type, "")) {
      data_type <- sub("Engine$", "", engine_type)
    }

    if (is.null(data_type) || length(data_type) == 0 || is.na(data_type) || identical(data_type, "")) {
      return(NULL)
    }

    htmltools::tags$span(class = "sf-cache-label", paste0("Type: ", data_type))
  })

  # MARK: out Cache Size
  output$cache_size <- shiny::renderText({
    reactive_update_cache_size()
    if (is.null(engine)) return("N/A")
    tryCatch(
      engine$get_cache_size(),
      error = function(e) "N/A"
    )
  })

  # MARK: obs Clear Cache
  shiny::observeEvent(input$clear_cache_button, {
    tryCatch(
      {
        engine$clear_cache()
        reactive_update_cache_size(reactive_update_cache_size() + 1)
      },
      error = function(e) {
        shiny::showNotification(paste("Error clearing cache:", e$message), duration = 5)
      }
    )
  })

  # MARK: Report
  output$report_ui <- shiny::renderUI({
    if (is.null(engine)) return(htmltools::div("Engine not initialized!"))
    .mod_Report_Server(engine, "report", session$ns, reactive_volumes)
    .mod_Report_UI(engine, "report", session$ns)
  })

  # MARK: obs Show Init Modal
  shiny::observe({
    if (reactive_show_init_modal()) {
      reactive_show_init_modal(FALSE)
      .app_util_use_initial_modal(
        reactive_app_mode,
        reactive_engine_type,
        reactive_project_path,
        reactive_show_init_modal,
        .app_util_get_volumes(),
        input,
        output,
        session
      )
    }
  })

  # MARK: obs Restart App
  shiny::observeEvent(input$restart_app, {
    time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
    btn_id <- paste0("confirm_restart_", time_var)
    shiny::showModal(shiny::modalDialog(
      "Are you sure you want to restart StreamFind?",
      title = "Restart StreamFind",
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(btn_id, "Confirm", class = "btn-danger")
      )
    ))
    shiny::observeEvent(input[[btn_id]], {
      shiny::removeModal()
      # Remove any stale shinyFiles / modal artifacts before resetting app state.
      # Otherwise body may remain modal-open and the initial wizard renders incorrectly.
      session$sendCustomMessage("cleanupAllModals", list())
      session$sendCustomMessage("setBootOverlay", list(visible = FALSE))

      # Reset all state so re-selecting the same path still triggers observers
      engine <<- NULL
      reactive_app_mode(NA_character_)
      reactive_engine_type(NA_character_)
      reactive_project_path(NA_character_)
      reactive_metadata(NULL)
      reactive_analyses(NULL)
      reactive_workflow(NULL)
      reactive_results(NULL)
      reactive_audit(NULL)
      reactive_warnings(list())
      # The observe at the bottom of the server detects both reactives being NA
      # and sets reactive_show_init_modal(TRUE), which triggers the modal chain
    }, ignoreNULL = TRUE, once = TRUE)
  })

  # Show modal if both engine type and project path are not set (fresh start)
  shiny::observe({
    if (is.na(reactive_engine_type()) && is.na(reactive_project_path())) {
      reactive_show_init_modal(TRUE)
    }
  })
}
