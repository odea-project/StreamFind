#' @noRd
.mod_WorkflowAssembler_Explorer_UI.RamanAnalyses <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  shinydashboard::tabBox(
    width = 12,
    height = "calc(100vh - 50px - 30px - 20px)",
    shiny::tabPanel(
      "Spectra",
      shiny::fluidRow(
        shiny::column(
          3,
          DT::dataTableOutput(
            ns(ns2("spectraAnalysesTable")),
            height = "calc(100vh - 50px - 30px - 20px - 44px - 10px)"
          )
        ),
        shiny::column(9,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bg = NULL,
              shiny::uiOutput(ns(ns2("summary_plot_controls")))
            ),
            shiny::uiOutput(ns(ns2("summary_plot_ui")))
          ),
          height = "calc(100vh - 50px - 30px - 20px - 44px - 10px)"
        )
      )
    )
  )
}

#' @noRd
.mod_WorkflowAssembler_Explorer_Server.RamanAnalyses <- function(
  x,
  id,
  ns,
  reactive_analyses,
  reactive_volumes,
  reactive_config
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)

    shinyFiles::shinyFileSave(
      input,
      "summary_plot_save",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )

    # out summary plot UI -----
    output$summary_plot_ui <- shiny::renderUI({
      if (length(reactive_analyses()$analyses) == 0) {
        htmltools::div(
          style = "margin-top: 20px;",
          htmltools::h4("No analyses found!")
        )
      } else if (!is.null(input$summary_plot_interactive)) {
        if (as.logical(input$summary_plot_interactive)) {
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              ns(ns2("summary_plotly")),
              height = "calc(100vh - 50px - 30px - 20px - 44px - 50px)"
            ),
            color = "black"
          )
        } else {
          shinycssloaders::withSpinner(
            shiny::plotOutput(
              ns(ns2("summary_plot")),
              height = "calc(100vh - 50px - 30px - 20px - 44px - 50px)"
            ),
            color = "black"
          )
        }
      }
    })

    # out summary controls -----
    output$summary_plot_controls <- shiny::renderUI({
      if (length(reactive_analyses()$analyses) == 0) {
        return()
      }
      htmltools::div(
        style = "display: flex; flex-direction: column; gap: 10px; padding: 10px;",
        htmltools::div(
          shinyFiles::shinySaveButton(
            ns(ns2("summary_plot_save")),
            "Export (.csv)",
            "Export (.csv)",
            filename = "spectra_summary_data",
            filetype = list(csv = "csv")
          )
        ),
        htmltools::div(
          shiny::selectInput(
            ns(ns2("summary_plot_interactive")),
            label = "Interactive",
            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
            selected = TRUE,
            width = "100%"
          )
        ),
        htmltools::div(
          shiny::selectInput(
            ns(ns2("summary_plot_colorby")),
            label = "Color by",
            choices = c("analyses", "replicates"),
            selected = "analyses",
            width = "100%"
          )
        ),
        htmltools::div(
          shiny::selectInput(
            ns(ns2("summary_plot_raw")),
            label = "Raw Spectra",
            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
            selected = TRUE,
            width = "100%"
          )
        )
      )
    })

    # out spectra analyses table -----
    output$spectraAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      if (length(analyses$analyses) == 0) {
        return()
      }
      DT::datatable(
        info(analyses)[, c("analysis", "replicate", "blank"), with = FALSE],
        selection = list(mode = "multiple", selected = 1, target = "row"),
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollY = "calc(100vh - 50px - 30px - 20px - 44px - 10px - 100px)",
          scrollCollapse = TRUE
        )
      )
    })

    # out Summary plotly -----
    output$summary_plotly <- plotly::renderPlotly({
      if (length(reactive_analyses()$analyses) == 0) {
        return()
      }
      selected <- input$spectraAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_spectra(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$summary_plot_colorby,
        interactive = as.logical(input$summary_plot_interactive),
        useRawData = as.logical(input$summary_plot_raw)
      )
    })

    # out Summary plot -----
    output$summary_plot <- shiny::renderPlot({
      if (length(reactive_analyses()$analyses) == 0) {
        return()
      }
      selected <- input$spectraAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_spectra(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$summary_plot_colorby,
        interactive = as.logical(input$summary_plot_interactive),
        useRawData = as.logical(input$summary_plot_raw)
      )
    })

    # event Summary plot export -----
    shiny::observeEvent(input$summary_plot_save, {
      if (length(reactive_analyses()$analyses) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      if (!is.null(input$summary_plot_interactive)) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) {
          return()
        }
        csv <- get_spectra(
          reactive_analyses(),
          analyses = selected,
          useRawData = as.logical(input$summary_plot_raw)
        )
        fileinfo <- shinyFiles::parseSavePath(
          reactive_volumes(),
          input$summary_plot_save
        )
        if (nrow(fileinfo) > 0) {
          write.csv(csv, fileinfo$datapath, row.names = FALSE)
          shiny::showNotification(
            "File saved successfully!",
            duration = 5,
            type = "message"
          )
        }
      }
    })
  })
}
