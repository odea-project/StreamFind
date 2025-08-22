#' @noRd
.mod_WorkflowAssembler_Metadata_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  shinydashboard::box(
    title = "Metadata",
    width = 12,
    height = "calc(100vh - 50px - 30px - 200px - 20px - 20px)",
    solidHeader = TRUE,
    style = "display: flex; flex-direction: column; padding: 5px; box-sizing: border-box;",
    shiny::div(
      style = "flex-shrink: 0; margin-bottom: 10px; display: flex; gap: 10px; height: 50px; align-items: center;padding: 10px;",
      shiny::actionButton(ns(ns2("add_row")), "Add New Row", class = "btn-light"),
      shiny::uiOutput(ns(ns2("update_metadata_ui")))
    ),
    shiny::div(
      style = "flex-shrink: 0; margin-bottom: 5px; color: #666; font-size: 12px; height: 30px; padding: 10px;",
      shiny::HTML("<i class='fa fa-info-circle'></i> Double-click on any cell to edit its value")
    ),
    shiny::div(
      style = "flex: 1; padding: 10px; min-height: 0;",
      DT::DTOutput(ns(ns2("metadata_dt")))
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Metadata_Server <- function(
    id,
    ns,
    reactive_metadata,
    reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    metadata_dt <- reactiveVal()
    dt <- as.data.table(reactive_metadata())
    data.table::setnames(dt, c("Name", "Value"))
    metadata_dt(dt)

    # Reactive to check if metadata has changed
    metadata_changed <- reactive({
      tryCatch({
        dt <- data.table::copy(metadata_dt())
        data.table::setnames(dt, c("name", "value"))
        dt_meta <- as.Metadata(dt)
        saved_meta <- reactive_metadata()
        !identical(dt_meta, saved_meta)
      }, error = function(e) {
        message("Error Metadata format: ", e)
        TRUE
      })
    })

    # Render update button based on changes
    output$update_metadata_ui <- renderUI({
      if (metadata_changed()) {
        shiny::div(
          style = "display: flex; gap: 10px;",
          shiny::actionButton(ns(ns2("update_metadata")), "Update Metadata", class = "btn-warning"),
          shiny::actionButton(ns(ns2("discard_changes")), "Discard Changes", class = "btn-danger")
        )
      }
    })

    # Render DT ----
    output$metadata_dt <- DT::renderDT(
      {
        dt_display <- metadata_dt()
        # Add delete button column
        if (nrow(dt_display) > 0) {
          dt_display$Delete <- paste0('<button class="btn btn-danger btn-sm delete-btn" data-row="', seq_len(nrow(dt_display)), '"><i class="fa fa-trash"></i></button>')
        } else {
          dt_display$Delete <- character(0)
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
          escape = FALSE, # Allow HTML in the Delete column
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
            scrollY = "calc(100vh - 320px - 40px - 60px - 35px - 110px)",
            scrollCollapse = TRUE,
            paging = FALSE,
            dom = "ft",
            columnDefs = list(
              list(orderable = FALSE, targets = 2), # Make Delete column non-sortable
              list(width = "60px", targets = 2) # Set width for Delete column
            )
          ),
          class = "cell-border stripe"
        )
      },
      server = TRUE
    )

    # Handle edits ----
    observeEvent(input$metadata_dt_cell_edit, {
      info <- input$metadata_dt_cell_edit
      dt <- metadata_dt()
      dt[info$row, (info$col + 1)] <- info$value # +1 because DT index starts at 0 for JS
      metadata_dt(dt)
    })

    # Add new row ----
    observeEvent(input$add_row, {
      dt <- metadata_dt()
      place_holder_idx <- nrow(dt) + 1
      place_holder_name <- paste0("place_holder_", place_holder_idx)
      dt <- rbind(dt, data.table(Name = place_holder_name, Value = place_holder_name))
      dt <- dt[!duplicated(dt), ]
      metadata_dt(dt)
      # proxy <- DT::dataTableProxy("metadata_dt", session = session)
      # DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
    })

    # Delete row ----
    observeEvent(input$delete_row, {
      delete_info <- input$delete_row

      # Parse row number from the string (format: "row_timestamp")
      row_to_delete <- as.numeric(strsplit(delete_info, "_")[[1]][1])

      dt <- metadata_dt()

      if (!is.na(row_to_delete) && row_to_delete > 0 && row_to_delete <= nrow(dt)) {
        dt <- dt[-row_to_delete, ]
        metadata_dt(dt)
        # proxy <- DT::dataTableProxy("metadata_dt", session = session)
        # DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
      }
    }, ignoreInit = TRUE)

    # Update reactive_metadata ----
    observeEvent(input$update_metadata, {
      tryCatch({
        dt <- metadata_dt()
        colnames(dt) <- c("name", "value")
        dt <- dt[!dt$name %in% c("place holder"), ]
        new_metadata <- as.Metadata(dt)
        reactive_metadata(new_metadata)
      }, error = function(e) {
        message("Error updating Metadata: ", e)
      })
    })

    # Discard changes ----
    observeEvent(input$discard_changes, {
      dt <- as.data.table(reactive_metadata())
      data.table::setnames(dt, c("Name", "Value"))
      metadata_dt(dt)
      # proxy <- DT::dataTableProxy("metadata_dt", session = session)
      # DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
    })
  })
}
