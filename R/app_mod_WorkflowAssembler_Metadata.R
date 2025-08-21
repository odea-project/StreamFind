#' @noRd
.mod_WorkflowAssembler_Metadata_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  shiny::column(
    width = 12,
    shinydashboard::box(
      title = "Metadata",
      width = 12,
      solidHeader = TRUE,
      style = "display: flex; flex-direction: column; height: 58vh;",
      shiny::div(
        style = "margin-top: 10px; margin-bottom: 10px; display: flex; gap: 10px;",
        shiny::actionButton(ns(ns2("add_row")), "Add New Row", class = "btn-primary"),
        shiny::uiOutput(ns(ns2("update_metadata_ui")))
      ),
      shiny::div(
        style = "margin-bottom: 5px; color: #666; font-size: 12px;",
        shiny::HTML("<i class='fa fa-info-circle'></i> Double-click on any cell to edit its value")
      ),
      shiny::div(
        style = "flex: 1; overflow: auto; min-height: 0;",
        DT::DTOutput(ns(ns2("metadata_dt")), height = "100%"),
      )
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
      dt <- metadata_dt()
      data.table::setnames(dt, c("name", "value"))
      dt_meta <- as.Metadata(dt)
      saved_meta <- reactive_metadata()
      !identical(dt_meta, saved_meta)
    })

    # Render update button based on changes
    output$update_metadata_ui <- renderUI({
      if (metadata_changed()) {
        shiny::div(
          style = "display: flex; gap: 10px;",
          shiny::actionButton(ns(ns2("update_metadata")), "Update Metadata", class = "btn-danger"),
          shiny::actionButton(ns(ns2("discard_changes")), "Discard Changes", class = "btn-warning")
        )
      } else {
        shiny::actionButton(ns(ns2("update_metadata")), "Update Metadata", class = "btn-success")
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

        # Find protected rows
        protected_rows <- which(tolower(dt_display$Name) %in% c("name", "date", "author", "file"))

        DT::datatable(
          dt_display,
          rownames = FALSE,
          editable = list(
            target = "cell",
            disable = list(
              columns = c(2), # Disable editing on Delete column
              cells = if (length(protected_rows) > 0) {
                # Create matrix of [row, col] pairs for protected cells
                # Column 0 = Name column, Column 1 = Value column for protected rows
                rbind(
                  cbind(protected_rows - 1, 0), # Name column (0-indexed)
                  cbind(protected_rows - 1, 1)  # Value column (0-indexed)
                )
              } else {
                NULL
              }
            )
          ),
          selection = "none",
          escape = FALSE, # Allow HTML in the Delete column
          callback = DT::JS(paste0("
            table.on('click', '.delete-btn', function() {
              var row = $(this).data('row');
              Shiny.setInputValue('", session$ns("delete_row"), "', row, {priority: 'event'});
            });
          ")),
          options = list(
            searching = TRUE,
            processing = TRUE,
            scrollY = "calc(58vh - 100px)",
            scrollCollapse = TRUE,
            paging = FALSE,
            dom = "fi",
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
      dt[info$row, (info$col + 1) := info$value] # +1 because DT index starts at 0 for JS
      metadata_dt(dt)
    })

    # Add new row ----
    observeEvent(input$add_row, {
      dt <- metadata_dt()
      dt <- rbind(dt, data.table(name = "place holder", value = "place holder"))
      dt <- dt[!duplicated(dt), ]
      metadata_dt(dt)
      proxy <- DT::dataTableProxy("metadata_dt", session = session)
      DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
    })

    # Delete row ----
    observeEvent(input$delete_row, {
      row_to_delete <- input$delete_row
      dt <- metadata_dt()
      if (row_to_delete > 0 && row_to_delete <= nrow(dt)) {
        dt <- dt[-row_to_delete, ]
        metadata_dt(dt)
        proxy <- DT::dataTableProxy("metadata_dt", session = session)
        DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
      }
    })

    # Update reactive_metadata ----
    observeEvent(input$update_metadata, {
      dt <- metadata_dt()
      data.table::setnames(dt, c("name", "value"))
      dt <- dt[!dt$name %in% c("place holder"), ]
      metadata_dt(dt)
      new_metadata <- as.Metadata(dt)
      reactive_metadata(new_metadata)
    })

    # Discard changes ----
    observeEvent(input$discard_changes, {
      dt <- as.data.table(reactive_metadata())
      data.table::setnames(dt, c("Name", "Value"))
      metadata_dt(dt)
      proxy <- DT::dataTableProxy("metadata_dt", session = session)
      DT::replaceData(proxy, metadata_dt(), resetPaging = FALSE, rownames = FALSE)
    })
  })
}
