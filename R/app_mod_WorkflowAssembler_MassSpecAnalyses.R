#' @noRd
.mod_WorkflowAssembler_Analyses_UI.MassSpecAnalyses <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  shinydashboard::box(
    title = NULL,
    width = 12,
    solidHeader = TRUE,
    shiny::column(12, shiny::uiOutput(ns(ns2("analyses_overview_buttons")))),
    shiny::column(12, shiny::uiOutput(ns(ns2("notes_analyses")))),
    shiny::column(12, DT::dataTableOutput(ns(ns2("AnalysesTable"))))
  )
}

#' @noRd
.mod_WorkflowAssembler_Analyses_Server.MassSpecAnalyses <- function(
    x,
    id,
    ns,
    reactive_analyses,
    reactive_warnings,
    reactive_volumes,
    reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    # out Analyses Table -----
    output$AnalysesTable <- DT::renderDT(
      {
        analyses <- reactive_analyses()
        analyses_info <- info(analyses)
        if (nrow(analyses_info) > 0) {
          analyses_info$Delete <- paste0('<button class="btn btn-danger btn-sm delete-btn" data-row="', seq_len(nrow(analyses_info)), '"><i class="fa fa-trash"></i></button>')
        } else {
          analyses_info$Delete <- character(0)
        }
        blocked_columns <- seq_len(ncol(analyses_info)) - 1
        blocked_columns <- blocked_columns[!colnames(analyses_info) %in% c("replicate", "blank")]

        # if (length(analyses) == 0) {
        #   analyses_info <- data.frame()
        #   edits <- FALSE
        # } else if ("replicate" %in% colnames(analyses_info)) {
        #   edits <- list(
        #     target = "column",
        #     disable = list(
        #       columns = which(
        #         !colnames(analyses_info) %in% c("replicate", "blank")
        #       )
        #     )
        #   )
        # } else {
        #   analyses_info <- data.frame()
        #   edits <- FALSE
        # }
        DT::datatable(
          analyses_info,
          rownames = FALSE,
          editable = list(
            target = "cell",
            disable = list(
              columns = blocked_columns
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
          extensions = c("Buttons"),
          options = list(
            searching = TRUE,
            processing = TRUE,
            scrollY = 700, # "calc(100vh - 320px - 40px - 60px - 35px - 110px)",
            scrollCollapse = TRUE,
            paging = FALSE,
            dom = "Bfrt",
            buttons = c("copy", "csv", "pdf", "print"),
            orderable = FALSE,
            columnDefs = list(
              list(orderable = FALSE, targets = seq_len(ncol(analyses_info)) - 1), # Make Delete column non-sortable
              list(width = "60px", targets = ncol(analyses_info) - 1) # Set width for Delete column
            )
          ),
          class = "cell-border stripe"
        )
        # DT::datatable(
        #   analyses_info,
        #   selection = list(
        #     mode = "multiple",
        #     selected = NULL,
        #     target = "cell",
        #     selectable = matrix(
        #       c(seq_len(nrow(analyses_info)), rep(1, nrow(analyses_info))),
        #       ncol = 2
        #     )
        #   ),
        #   editable = edits,
        #   extensions = c("Scroller", "Buttons"),
        #   options = list(
        #     pageLength = 10,
        #     deferRender = TRUE,
        #     scrollY = 700,
        #     scroller = TRUE,
        #     dom = "Bfrtip",
        #     buttons = c("copy", "csv", "pdf", "print")
        #   )
        # )
      },
      server = TRUE
    )

    # Delete row ----
    observeEvent(input$delete_row,
      {
        delete_info <- input$delete_row
        row_to_delete <- as.numeric(strsplit(delete_info, "_")[[1]][1])
        analyses <- reactive_analyses()
        analyses_info <- info(analyses)
        if (!is.na(row_to_delete) && row_to_delete > 0 && row_to_delete <= nrow(analyses_info)) {
          analyses <- analyses[-row_to_delete]
          reactive_analyses(analyses)
        }
      },
      ignoreInit = TRUE
    )

    # event Analyses Table Editing -----
    shiny::observeEvent(input$AnalysesTable_cell_edit, {
      analyses <- reactive_analyses()
      analyses_info <- info(analyses)
      analyses_info <- DT::editData(
        info(analyses),
        input$AnalysesTable_cell_edit,
        rownames = FALSE
      )
      analyses_info$blank[analyses_info$blank %in% ""] <- NA_character_
      analyses <- set_replicate_names(analyses, analyses_info$replicate)
      if (any(!(analyses_info$blank %in% analyses_info$replicate) & !is.na(analyses_info$blank))) {
        reactive_warnings(
          .app_util_add_notifications(
            reactive_warnings(),
            "blank_names_not_in_replicate",
            "Blank names must be in the replicate column!"
          )
        )
      } else {
        reactive_warnings(
          .app_util_remove_notifications(
            reactive_warnings(),
            "blank_names_not_in_replicate"
          )
        )
        analyses <- set_blank_names(analyses, analyses_info$blank)
      }
      reactive_analyses(analyses)
    })

    # out Analyses Overview Buttons -----
    output$analyses_overview_buttons <- shiny::renderUI({
      filetypes <- reactive_analyses()$formats
      filetypes <- gsub("[.]", "", filetypes)
      filetypes <- unlist(strsplit(filetypes, "[|]"))
      shinyFiles::shinyFileChoose(
        input,
        "add_analyses_button",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session,
        filetypes = filetypes
      )
      shinyFiles::shinyFileChoose(
        input,
        "upload_analyses_info",
        roots = reactive_volumes(),
        defaultRoot = "wd",
        session = session,
        filetypes = "csv"
      )
      if (length(reactive_analyses()) > 0) {
        htmltools::div(
          style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton(
            ns(ns2("add_analyses_button")),
            "Add Analyses",
            paste0(
              "Select Analyses (",
              paste(reactive_analyses()$formats, collapse = "|"),
              ")"
            ),
            multiple = TRUE,
            style = "width: 200px;"
          ),
          shiny::actionButton(
            ns(ns2("remove_all_analyses")),
            label = "Delete All Analyses",
            width = 200
          )
        )
      } else {
        htmltools::div(
          style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton(
            ns(ns2("add_analyses_button")),
            "Add Analyses",
            paste0(
              "Select Analyses (",
              paste(reactive_analyses()$formats, collapse = "|"),
              ")"
            ),
            multiple = TRUE,
            style = "width: 200px;"
          )
        )
      }
    })

    # event Add analyses -----
    shiny::observeEvent(input$add_analyses_button, {
      fileinfo <- shinyFiles::parseFilePaths(
        reactive_volumes(),
        input$add_analyses_button
      )
      if (nrow(fileinfo) > 0) {
        files <- fileinfo$datapath
        number_files <- length(files)
        if (number_files > 0) {
          if (
            all(
              tools::file_ext(files) %in% reactive_analyses()$formats
            )
          ) {
            analyses <- reactive_analyses()

            # TODO add modal to accept remove results

            output$loading_spinner <- shiny::renderUI({
              htmltools::div(style = "height: 100px; width: 100px;")
            })

            shiny::withProgress(message = "Loading files...", value = 0, {
              for (i in seq_len(number_files)) {
                tryCatch(
                  {
                    analyses <- add(analyses, files[i])
                  },
                  error = function(e) {
                    msg <- paste(
                      "Error for",
                      files[i],
                      ":",
                      conditionMessage(e)
                    )
                    shiny::showNotification(msg, duration = 10, type = "error")
                  }
                )
                shiny::incProgress(i / number_files)
              }
            })

            output$loading_spinner <- shiny::renderUI({
              NULL
            })

            reactive_analyses(analyses)
          } else {
            shiny::showNotification(
              "Invalid file/s format/s!",
              duration = 10,
              type = "warning"
            )
          }
        }
      }
    })

    # event Remove all analyses -----
    shiny::observeEvent(input$remove_all_analyses, {
      analyses <- reactive_analyses()
      if (length(analyses) == 0) {
        shiny::showNotification(
          "No analyses found!",
          duration = 10,
          type = "warning"
        )
        return()
      }
      analyses <- remove(analyses, seq_len(length(analyses)))
      reactive_analyses(analyses)
    })

    # out Notes Analyses -----
    output$notes_analyses <- shiny::renderUI({
      shiny::div(
        style = "margin-bottom: 20px;margin-top: 0px;",
        shiny::tagList(
          shiny::tags$ul(
            style = "list-style: none; padding-left: 0;", # Remove default bullets
            shiny::tags$li(
              style = "margin-bottom: 8px;",
              shiny::tags$i(class = "fa fa-info-circle", style = "margin-right: 10px; color: #666;"),
              sprintf(
                "Analyses can be added in the following formats: %s.",
                paste(reactive_analyses()$formats, collapse = ", ")
              )
            ),
            shiny::tags$li(
              style = "margin-bottom: 8px;",
              shiny::tags$i(class = "fa fa-info-circle", style = "margin-right: 10px; color: #666;"),
              "Replicate and blank names can be edited in the table by double clicking the cell."
            ),
            shiny::tags$li(
              style = "margin-bottom: 8px;",
              shiny::tags$i(class = "fa fa-info-circle", style = "margin-right: 10px; color: #666;"),
              "Note that blank names must be in the replicate column otherwise are not considered as blanks."
            )
          )
        )
        # shiny::tagList(
        #   shiny::tags$ul(
        #     shiny::tags$li(
        #       sprintf(
        #         "Analyses can be added in the following formats: %s.",
        #         paste(reactive_analyses()$formats, collapse = ", ")
        #       )
        #     ),
        #     shiny::tags$li(
        #       "Replicate and blank names can be edited in the table by double clicking the cell."
        #     ),
        #     shiny::tags$li(
        #       "Note that blank names must be in the replicate column otherwise are not considered as blanks."
        #     )
        #   )
        # )
      )
    })
  })
}
