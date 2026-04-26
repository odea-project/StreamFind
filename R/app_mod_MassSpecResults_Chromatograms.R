## MARK: .mod_Result_UI.MassSpecResults_Chromatograms
##' @export
##' @noRd
.mod_Result_UI.MassSpecResults_Chromatograms <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  ns_full <- function(name) ns(ns2(name))

  bslib::navset_card_tab(
    id = ns_full("main_tabs"),
    height = "calc(100vh - 35px - 44px - 52.8px)",

    # MARK: Chromatograms Tab -----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("wave-square", class = "mr-2"), "Chromatograms"),
      shiny::fluidRow(
        shiny::column(
          3,
          DT::DTOutput(
            ns_full("chrom_analyses_table"),
            height = "calc(100vh - 35px - 10px - 38px - 52.8px)"
          )
        ),
        shiny::column(
          9,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bg = NULL,
              shiny::uiOutput(ns_full("chrom_plot_controls"))
            ),
            shiny::uiOutput(ns_full("chrom_plot_ui"))
          ),
          height = "calc(100vh - 35px - 10px - 38px - 52.8px)"
        )
      )
    ),

    # MARK: Peaks Tab -----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("chart-area", class = "mr-2"), "Peaks"),
      shiny::fluidRow(
        shiny::column(
          3,
          DT::DTOutput(
            ns_full("peaks_analyses_table"),
            height = "calc(100vh - 35px - 10px - 38px - 52.8px)"
          )
        ),
        shiny::column(
          9,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bg = NULL,
              shiny::uiOutput(ns_full("peaks_plot_controls"))
            ),
            shiny::div(
              style = "display: flex; flex-direction: column; height: calc(100vh - 35px - 10px - 38px - 52.8px);",
              shiny::div(
                style = "flex: 1; min-height: 0;",
                shiny::uiOutput(ns_full("peaks_plot_ui"))
              ),
              shiny::div(
                style = "flex: 1; min-height: 0; overflow-y: auto; padding-top: 5px;",
                DT::DTOutput(ns_full("peaks_table"))
              )
            )
          ),
          height = "calc(100vh - 35px - 10px - 38px - 52.8px)"
        )
      )
    ),

    # MARK: Summary Tab -----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("chart-pie", class = "mr-2"), "Summary"),
      shiny::div(
        style = "padding: 10px; overflow-y: auto; height: calc(100vh - 125.8px);",
        DT::DTOutput(ns_full("summary_table"))
      )
    )
  )
}

## MARK: .mod_Result_Server.MassSpecResults_Chromatograms
##' @export
##' @noRd
.mod_Result_Server.MassSpecResults_Chromatograms <- function(
    x,
    id,
    ns,
    reactive_analyses,
    reactive_volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns_full <- session$ns

    # MARK: reactive data -----
    chrom_results <- shiny::reactiveVal()

    shiny::observe({
      shiny::validate(shiny::need(!is.null(x), "Chromatograms results not available."))
      chrom_results(x)
    })

    analyses_info <- shiny::reactive({
      res <- chrom_results()
      shiny::req(!is.null(res))
      conn <- DBI::dbConnect(duckdb::duckdb(), res$db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbGetQuery(conn, "SELECT analysis, replicate FROM Analyses ORDER BY analysis")
    })

    has_peaks <- shiny::reactive({
      res <- chrom_results()
      shiny::req(!is.null(res))
      conn <- DBI::dbConnect(duckdb::duckdb(), res$db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM Peaks")$n > 0
    })

    # MARK: Summary Table -----
    output$summary_table <- DT::renderDT({
      res <- chrom_results()
      shiny::req(!is.null(res))
      conn <- DBI::dbConnect(duckdb::duckdb(), res$db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      analyses <- DBI::dbGetQuery(conn, "SELECT analysis, replicate, blank, polarity FROM Analyses")
      chrom_counts <- DBI::dbGetQuery(conn, "
        SELECT analysis, COUNT(DISTINCT index) AS chromatograms
        FROM Chromatograms GROUP BY analysis
      ")
      peak_counts <- DBI::dbGetQuery(conn, "
        SELECT analysis, COUNT(*) AS peaks FROM Peaks GROUP BY analysis
      ")

      dt <- data.table::as.data.table(analyses)
      dt$chromatograms <- chrom_counts$chromatograms[match(dt$analysis, chrom_counts$analysis)]
      dt$peaks <- peak_counts$peaks[match(dt$analysis, peak_counts$analysis)]
      dt$chromatograms[is.na(dt$chromatograms)] <- 0L
      dt$peaks[is.na(dt$peaks)] <- 0L

      DT::datatable(
        dt,
        rownames = FALSE,
        selection = "none",
        options = list(dom = "ft", paging = FALSE, scrollX = TRUE)
      )
    })

    # MARK: Chromatograms analyses table -----
    output$chrom_analyses_table <- DT::renderDT({
      ai <- analyses_info()
      shiny::req(!is.null(ai) && nrow(ai) > 0)
      DT::datatable(
        ai[, c("analysis", "replicate"), drop = FALSE],
        rownames = FALSE,
        selection = list(mode = "multiple", selected = seq_len(nrow(ai)), target = "row"),
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollY = "calc(100vh - 165px)",
          scrollCollapse = TRUE,
          columnDefs = list(list(width = "120px", targets = "_all"))
        )
      )
    })

    # MARK: Chromatograms sidebar controls -----
    output$chrom_plot_controls <- shiny::renderUI({
      shiny::req(!is.null(chrom_results()))
      htmltools::div(
        style = "display: flex; flex-direction: column; gap: 10px; padding: 10px;",
        shiny::selectInput(
          ns_full("chrom_color_by"),
          label = "Color by",
          choices = c(
            "analyses+targets",
            "analyses",
            "replicates",
            "targets",
            "replicates+targets"
          ),
          selected = "analyses+targets",
          width = "100%"
        ),
        shiny::checkboxInput(
          ns_full("chrom_normalized"),
          label = "Normalized",
          value = FALSE
        )
      )
    })

    # MARK: Chromatograms plot UI -----
    output$chrom_plot_ui <- shiny::renderUI({
      plotly::plotlyOutput(
        ns_full("chrom_plotly"), height = "calc(100vh - 25px - 10px - 28px - 30px - 52.8px)"
      )
    })

    output$chrom_plotly <- plotly::renderPlotly({
      res <- chrom_results()
      shiny::req(!is.null(res))
      ai <- analyses_info()
      shiny::req(!is.null(ai))
      selected <- input$chrom_analyses_table_rows_selected
      shiny::req(length(selected) > 0)
      sel_analyses <- ai$analysis[selected]
      p <- plot_chromatograms(
        res,
        analyses = sel_analyses,
        colorBy = input$chrom_color_by %||% "analyses+targets",
        normalized = isTRUE(input$chrom_normalized),
        interactive = TRUE
      )
      plotly::layout(
        p,
        title = NULL,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$chrom_plot <- shiny::renderPlot({
      res <- chrom_results()
      shiny::req(!is.null(res))
      ai <- analyses_info()
      shiny::req(!is.null(ai))
      selected <- input$chrom_analyses_table_rows_selected
      shiny::req(length(selected) > 0)
      sel_analyses <- ai$analysis[selected]
      plot_chromatograms(
        res,
        analyses = sel_analyses,
        colorBy = input$chrom_color_by %||% "analyses+targets",
        normalized = isTRUE(input$chrom_normalized),
        interactive = FALSE
      )
    }, bg = "transparent")

    # MARK: Peaks sidebar controls -----
    output$peaks_plot_controls <- shiny::renderUI({
      shiny::req(!is.null(chrom_results()))
      htmltools::div(
        style = "display: flex; flex-direction: column; gap: 10px; padding: 10px;",
        shiny::selectInput(
          ns_full("peaks_color_by"),
          label = "Color by",
          choices = c(
            "analyses+targets",
            "analyses",
            "replicates",
            "targets",
            "replicates+targets"
          ),
          selected = "analyses+targets",
          width = "100%"
        )
      )
    })

    # MARK: Peaks analyses table -----
    output$peaks_analyses_table <- DT::renderDT({
      ai <- analyses_info()
      shiny::req(!is.null(ai) && nrow(ai) > 0)
      DT::datatable(
        ai[, c("analysis", "replicate"), drop = FALSE],
        rownames = FALSE,
        selection = list(mode = "multiple", selected = seq_len(nrow(ai)), target = "row"),
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollY = "calc(100vh - 165px)",
          scrollCollapse = TRUE,
          columnDefs = list(list(width = "120px", targets = "_all"))
        )
      )
    })

    # MARK: Peaks plot UI -----
    output$peaks_plot_ui <- shiny::renderUI({
      shiny::req(has_peaks())
      plotly::plotlyOutput(ns_full("peaks_plotly"), height = "calc(50vh - 57.9px)")
    })

    output$peaks_plotly <- plotly::renderPlotly({
      res <- chrom_results()
      shiny::req(!is.null(res), has_peaks())
      ai <- analyses_info()
      shiny::req(!is.null(ai))
      selected <- input$peaks_analyses_table_rows_selected
      shiny::req(length(selected) > 0)
      sel_analyses <- ai$analysis[selected]
      p <- plot_chromatograms_peaks(
        res,
        analyses = sel_analyses,
        colorBy = input$peaks_color_by %||% "analyses+targets",
        interactive = TRUE
      )
      plotly::layout(
        p,
        title = NULL,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    })

    output$peaks_plot <- shiny::renderPlot({
      res <- chrom_results()
      shiny::req(!is.null(res), has_peaks())
      ai <- analyses_info()
      shiny::req(!is.null(ai))
      selected <- input$peaks_analyses_table_rows_selected
      shiny::req(length(selected) > 0)
      sel_analyses <- ai$analysis[selected]
      plot_chromatograms_peaks(
        res,
        analyses = sel_analyses,
        colorBy = input$peaks_color_by %||% "analyses+targets",
        interactive = FALSE
      )
    }, bg = "transparent")

    # MARK: Peaks table -----
    output$peaks_table <- DT::renderDT({
      res <- chrom_results()
      shiny::req(!is.null(res), has_peaks())
      ai <- analyses_info()
      shiny::req(!is.null(ai))
      selected <- input$peaks_analyses_table_rows_selected
      shiny::req(length(selected) > 0)
      sel_analyses <- ai$analysis[selected]
      pks <- get_chromatograms_peaks(res, analyses = sel_analyses)
      shiny::req(!is.null(pks) && nrow(pks) > 0)
      num_cols <- names(pks)[sapply(pks, is.numeric)]
      for (col in num_cols) {
        pks[[col]] <- round(pks[[col]], if (grepl("mz|mass", col, ignore.case = TRUE)) 4L else 2L)
      }
      DT::datatable(
        pks,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        options = list(
          dom = "ft",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "calc(50vh - 112.5px)",
          scrollCollapse = TRUE
        )
      )
    })
  })
}
