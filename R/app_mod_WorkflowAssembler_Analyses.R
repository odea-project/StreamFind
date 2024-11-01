#' @noRd
.mod_WorkflowAssembler_Analyses_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  shiny::column(
    12,
    shinydashboard::box(
      title = "Analyses", width = 12, solidHeader = TRUE,
      shiny::column(width = 12, shiny::uiOutput(ns(ns2("analyses_overview_buttons")))),
      shiny::uiOutput(ns(ns2("notes_analyses"))),
      shiny::column(12, DT::dataTableOutput(ns(ns2("AnalysesTable"))))
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Analyses_Server <- function(id, ns, reactive_analyses, reactive_warnings, reactive_volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    .wrap_analyses_ui_in_divs <- function(elements) {
      lapply(elements, function(x) {
        htmltools::div(style = sprintf("min-width: %dpx; height: %dpx; display: flex; align-items: center;", 40, 40), x)
      })
    }

    # out Analyses Table -----
    output$AnalysesTable <- DT::renderDT(
      {
        analyses <- reactive_analyses()
        analyses_info <- analyses@info
        if (length(analyses) == 0) {
          analyses_info <- data.frame()
          edits <- FALSE
        } else if ("replicate" %in% colnames(analyses_info)) {
          edits <- list(
            target = "column",
            disable = list(columns = which(!colnames(analyses_info) %in% c("replicate", "blank")))
          )
        } else {
          analyses_info <- data.frame()
          edits <- FALSE
        }

        DT::datatable(
          analyses_info,
          selection = list(mode = "multiple", selected = NULL, target = "cell", selectable = matrix(c(seq_len(nrow(analyses_info)), rep(1, nrow(analyses_info))), ncol = 2)),
          editable = edits,
          extensions = c("Scroller", "Buttons"),
          options = list(
            pageLength = 10,
            deferRender = TRUE,
            scrollY = 700,
            scroller = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "pdf", "print")
          )
        )
      },
      server = TRUE
    )

    # event Analyses Table Editing -----
    shiny::observeEvent(input$AnalysesTable_cell_edit, {
      analyses <- reactive_analyses()
      analyses_info <- analyses@info
      analyses_info <- DT::editData(analyses@info, input$AnalysesTable_cell_edit)
      analyses$replicates <- analyses_info$replicate

      if (any(!(analyses_info$blank %in% analyses_info$replicate))) {
        shiny::showNotification("Blanks must be in the replicate column!", duration = 10, type = "warning")
      } else {
        analyses$blanks <- analyses_info$blank
      }
      reactive_analyses(analyses)
    })

    # out Analyses Overview Buttons -----
    output$analyses_overview_buttons <- shiny::renderUI({
      filetypes <- reactive_analyses()@possible_formats
      filetypes <- gsub("[.]", "", filetypes)
      filetypes <- unlist(strsplit(filetypes, "[|]"))
      shinyFiles::shinyFileChoose(input, "add_analyses_button", roots = reactive_volumes(), defaultRoot = "wd", session = session, filetypes = filetypes)
      shinyFiles::shinyFileChoose(input, "upload_analyses_info", roots = reactive_volumes(), defaultRoot = "wd", session = session, filetypes = "csv")

      if (length(reactive_analyses()) > 0) {
        htmltools::div(
          style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton(ns(ns2("add_analyses_button")), paste0("Add Analyses (", reactive_analyses()@possible_formats, ")"), paste0("Select Analyses (", reactive_analyses()@possible_formats, ")"), multiple = TRUE, style = "width: 200px;"),
          # shinyFiles::shinyFilesButton(ns(ns2("upload_analyses_info")), "Upload Analyses (.csv)", "Select Analyses (.csv)", multiple = FALSE, style = "width: 200px;"),
          shiny::actionButton(ns(ns2("remove_selected_analyses")), label = "Delete Selected Analyses", width = 200),
          shiny::actionButton(ns(ns2("remove_all_analyses")), label = "Delete All Analyses", width = 200)
        )
      } else {
        htmltools::div(
          style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton(ns(ns2("add_analyses_button")), paste0("Add Analyses (", reactive_analyses()@possible_formats, ")"), paste0("Select Analyses (", reactive_analyses()@possible_formats, ")"), multiple = TRUE, style = "width: 200px;")
          # shinyFiles::shinyFilesButton(ns(ns2("upload_analyses_info")), "Upload Analyses (.csv)", "Select Analyses (.csv)", multiple = FALSE, style = "width: 200px;")
        )
      }
    })

    # event Add analyses -----
    shiny::observeEvent(input$add_analyses_button, {
      fileinfo <- shinyFiles::parseFilePaths(reactive_volumes(), input$add_analyses_button)
      if (nrow(fileinfo) > 0) {
        files <- fileinfo$datapath
        number_files <- length(files)
        if (number_files > 0) {
          if (all(grepl(reactive_analyses()@possible_formats, files))) {
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
                    msg <- paste("Error for", files[i], ":", conditionMessage(e))
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
            shiny::showNotification("Invalid file format!", duration = 10, type = "warning")
          }
        }
      }
    })

    ## event Upload analyses info -----
    # shiny::observeEvent(input$upload_analyses_info, {
    #   shiny::req(input$upload_analyses_info)
    #   fileinfo <- shinyFiles::parseFilePaths(reactive_volumes(), input$upload_analyses_info)
    #   if (nrow(fileinfo) == 1) {
    #     file <- fileinfo$datapath
    #     if (all(grepl("csv", file))) {
    #       tryCatch({
    #         analyses <- reactive_analyses()
    #         analyses <- add(analyses, file)
    #         reactive_analyses(analyses)
    #       }, error = function(e) {
    #         msg <- paste("Error for", files[i], ":", conditionMessage(e))
    #         shiny::showNotification(msg, duration = 10, type = "error")
    #       })
    #     } else {
    #       shiny::showNotification("Invalid file format! Must be a csv.", duration = 10, type = "warning")
    #     }
    #   }
    # })

    # event Remove analyses -----
    shiny::observeEvent(input$remove_selected_analyses, {
      analyses <- reactive_analyses()
      if (length(input$AnalysesTable_cells_selected) == 0) {
        shiny::showNotification("No analyses selected!", duration = 10, type = "warning")
        return()
      }
      analyses <- remove(analyses, input$AnalysesTable_cells_selected[, 1])
      reactive_analyses(analyses)
    })

    # event Remove all analyses -----
    shiny::observeEvent(input$remove_all_analyses, {
      analyses <- reactive_analyses()
      if (length(analyses) == 0) {
        shiny::showNotification("No analyses found!", duration = 10, type = "warning")
        return()
      }
      analyses <- remove(analyses, seq_len(length(analyses)))
      reactive_analyses(analyses)
    })

    # out Notes Analyses -----
    output$notes_analyses <- shiny::renderUI({
      shiny::div(
        style = "margin-bottom: 25px;margin-top: 25px;",
        shiny::tagList(
          shiny::tags$ul(
            shiny::tags$li("Add analyses by selecting the 'Add Analyses' button."),
            shiny::tags$li("Remove analyses by selecting the analysis cells in the table and clicking the 'Delete Selected Analyses' button."),
            shiny::tags$li("Remove all analyses by clicking the 'Delete All Analyses' button."),
            shiny::tags$li("Replicate and blank names can be edited in the table by double clicking the cell. Changes are saved by CTRL + ENTER."),
            shiny::tags$li("Note that blank names must be in the replicate column otherwise are not considered as blanks.")
          )
        )
      )
    })
  })
}
