#' @noRd
S7::method(.mod_WorkflowAssembler_Explorer_UI, MassSpecAnalyses) <- function(x, id, ns) {
  ns2 <- shiny::NS(id)

  shinydashboard::tabBox(
    width = 12, height = "1080px",
    shiny::tabPanel(
      "Spectra",
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns(ns2("summary_plot_controls")))),
        shiny::column(12, shiny::uiOutput(ns(ns2("summary_plot_ui")))),
        shiny::column(12, DT::dataTableOutput(ns(ns2("spectraAnalysesTable"))))
      )
    ),
    shiny::tabPanel(
      "Chromatograms",
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns(ns2("chrom_plot_controls")))),
        shiny::column(12, shiny::uiOutput(ns(ns2("chrom_plot_ui")))),
        shiny::column(12, DT::dataTableOutput(ns(ns2("chromAnalysesTable"))))
      )
    ),
    shiny::tabPanel(
      "Extract Ion Chromatograms",
      shiny::uiOutput(ns(ns2("eics_interface")))
    )
  )
}

#' @noRd
S7::method(.mod_WorkflowAssembler_Explorer_Server, MassSpecAnalyses) <- function(x,
                                                                                 id,
                                                                                 ns,
                                                                                 reactive_analyses,
                                                                                 reactive_volumes,
                                                                                 reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)
    has_spectra <- shiny::reactiveVal(FALSE)
    has_chromatograms <- shiny::reactiveVal(FALSE)
    levels <- shiny::reactiveVal(1)
    rt_end <- shiny::reactiveVal(0)
    rt_start <- shiny::reactiveVal(0)

    init_analyses <- reactive_analyses()
    if (length(init_analyses) > 0) {
      has_spectra(max(init_analyses@spectra_number) > 0)
      has_chromatograms(max(init_analyses@chromatograms_number) > 0)
      levels(as.numeric(unlist(strsplit(unique(init_analyses@spectra_level), ", "))))
      rt_end(round(max(init_analyses@spectra_highest_rt), digits = 0))
      rt_start(round(min(init_analyses@spectra_lowest_rt), digits = 0))
    }

    shiny::observe({
      analyses <- reactive_analyses()
      if (length(analyses) > 0) {
        has_spectra(max(reactive_analyses()@spectra_number) > 0)
        has_chromatograms(max(reactive_analyses()@chromatograms_number) > 0)
        levels(as.numeric(unlist(strsplit(unique(reactive_analyses()@spectra_level), ", "))))
        rt_end(round(max(reactive_analyses()@spectra_highest_rt), digits = 0))
        rt_start(round(min(reactive_analyses()@spectra_lowest_rt), digits = 0))
      } else {
        has_spectra(FALSE)
        has_chromatograms(FALSE)
        levels(1)
        rt_end(0)
        rt_start(0)
      }
    })

    shinyFiles::shinyFileSave(
      input,
      "summary_plot_save",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )
    
    shinyFiles::shinyFileSave(
      input,
      "chrom_plot_save",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )

    colorby_spectra <- c(
      "analyses",
      "replicates",
      "polarities",
      "analyses+polarities",
      "replicates+polarities"
    )

    colorby_chromatograms <- c(
      "targets", "analyses",
      "replicates", "polarities",
      "targets+polarities",
      "analyses+polarities",
      "replicates+polarities",
      "targets+analyses",
      "targets+replicates"
    )
    
    # out Analyses Table -----
    output$spectraAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      if (length(analyses) == 0) {
        return()
      }
      analyses_info <- analyses@info
      DT::datatable(
        analyses_info[, c("analysis", "replicate", "blank", "polarity"), with = FALSE],
        selection = list(mode = "multiple", selected = 1, target = "row"),
        options = list(pageLength = 10)
      )
    })

    # out Summary plot UI -----
    output$summary_plot_ui <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_spectra()) {
        if (!is.null(input$summary_plot_interactive)) {
          if (input$summary_plot_interactive) {
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns(ns2("summary_plotly")), height = "600px"),
              color = "black"
            )
          } else {
            shinycssloaders::withSpinner(
              shiny::plotOutput(ns(ns2("summary_plot")), height = "600px"),
              color = "black"
            )
          }
        }
      } else {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No spectra found!"))
      }
    })

    # out Summary controls -----
    output$summary_plot_controls <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      if (has_spectra()) {
        htmltools::div(
          style = "display: flex; align-items: center;",
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::checkboxInput(
              ns(ns2("summary_plot_interactive")),
              label = "Interactive",
              value = TRUE,
              width = 100
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::selectInput(
              ns(ns2("summary_plot_type")),
              label = "Type",
              choices = c("TIC", "BPC"),
              selected = "TIC",
              width = 100
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::selectInput(
              ns(ns2("summary_plot_colorby")),
              label = "Color by",
              choices = colorby_spectra,
              selected = "analyses",
              width = 200
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::sliderInput(
              ns(ns2("summary_plot_level")),
              label = "MS levels",
              min = min(levels()),
              max = max(levels()),
              value = levels()[1],
              step = 1,
              width = 200
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::sliderInput(
              ns(ns2("summary_plot_rt")),
              label = "Retention Time (Seconds)",
              min = rt_start(),
              max = rt_end(),
              value = c(rt_start(), rt_end()),
              step = 1,
              width = 200
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::numericInput(
              ns(ns2("summary_plot_downsize")),
              label = "Downsize Integer",
              min = 1,
              max = 100,
              value = 1,
              step = 1,
              width = 140
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shinyFiles::shinySaveButton(
              ns(ns2("summary_plot_save")),
              "Save Plot Data (.csv)",
              "Save Plot Data (.csv)",
              filename = "spectra_summary_data",
              filetype = list(csv = "csv")
            )
          ),
          htmltools::div(style = "margin-bottom: 20px;")
        )
      }
    })

    # out Summary plotly -----
    output$summary_plotly <- plotly::renderPlotly({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      if (!is.null(input$summary_plot_type) && input$summary_plot_interactive) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) {
          return()
        }
        if (input$summary_plot_type %in% "TIC") {
          plot_spectra_tic(
            reactive_analyses(),
            analyses = selected,
            colorBy = input$summary_plot_colorby,
            level = input$summary_plot_level,
            rt = input$summary_plot_rt,
            downsize = input$summary_plot_downsize,
            interactive = input$summary_plot_interactive
          )
        } else if (input$summary_plot_type %in% "BPC") {
          plot_spectra_bpc(
            reactive_analyses(),
            analyses = selected,
            colorBy = input$summary_plot_colorby,
            level = input$summary_plot_level,
            rt = input$summary_plot_rt,
            interactive = input$summary_plot_interactive
          )
        }
      }
    })

    # out Summary plot -----
    output$summary_plot <- shiny::renderPlot({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      if (!is.null(input$summary_plot_type) && !input$summary_plot_interactive) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) {
          return()
        }
        if (input$summary_plot_type %in% "TIC") {
          plot_spectra_tic(
            reactive_analyses(),
            analyses = selected,
            colorBy = input$summary_plot_colorby,
            level = input$summary_plot_level,
            rt = input$summary_plot_rt,
            downsize = input$summary_plot_downsize,
            interactive = input$summary_plot_interactive
          )
        } else if (input$summary_plot_type %in% "BPC") {
          plot_spectra_bpc(
            reactive_analyses(),
            analyses = selected,
            colorBy = input$summary_plot_colorby,
            level = input$summary_plot_level,
            rt = input$summary_plot_rt,
            interactive = input$summary_plot_interactive
          )
        }
      }
    })

    # event Summary plot export -----
    shiny::observeEvent(input$summary_plot_save, {
      if (length(reactive_analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      if (!is.null(input$summary_plot_type)) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) {
          return()
        }
        if (input$summary_plot_type %in% "TIC") {
          csv <- get_spectra_tic(
            reactive_analyses(),
            analyses = selected,
            level = input$summary_plot_level
          )
        } else if (input$summary_plot_type %in% "BPC") {
          csv <- get_spectra_bpc(
            reactive_analyses(),
            analyses = selected,
            level = input$summary_plot_level
          )
        }
        fileinfo <- shinyFiles::parseSavePath(reactive_volumes(), input$summary_plot_save)
        if (nrow(fileinfo) > 0) {
          utils::write.csv(csv, fileinfo$datapath, row.names = FALSE)
          shiny::showNotification("File saved successfully!", duration = 5, type = "message")
        }
      }
    })
    
    # out Chromatograms Table -----
    output$chromAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      if (length(analyses) == 0) {
        return()
      }
      analyses_info <- analyses@info
      DT::datatable(
        analyses_info[, c("analysis", "replicate", "blank", "polarity"), with = FALSE],
        selection = list(mode = "multiple", selected = 1, target = "row"),
        options = list(pageLength = 10)
      )
    })

    # out Chromatograms plot UI -----
    output$chrom_plot_ui <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_chromatograms()) {
        if (!is.null(input$summary_chrom_interactive)) {
          if (input$summary_chrom_interactive) {
            plotly::plotlyOutput(ns(ns2("chrom_plotly")), height = "600px")
          } else {
            shiny::plotOutput(ns(ns2("chrom_plot")), height = "600px")
          }
        }
      } else {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No chromatograms found!"))
      }
    })

    # out Chromatograms controls -----
    output$chrom_plot_controls <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      if (has_chromatograms()) {
        htmltools::div(
          style = "display: flex; align-items: center;",
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::checkboxInput(
              ns(ns2("summary_chrom_interactive")),
              label = "Interactive",
              value = TRUE,
              width = 100
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shiny::selectInput(
              ns(ns2("summary_chrom_colorby")),
              label = "Color by",
              choices = colorby_chromatograms,
              selected = "targets",
              width = 200
            )
          ),
          htmltools::div(
            style = "margin-left: 20px;",
            shinyFiles::shinySaveButton(
              ns(ns2("chrom_plot_save")),
              "Save Plot Data (.csv)",
              "Save Plot Data (.csv)",
              filename = "chrom_data",
              filetype = list(csv = "csv")
            )
          ),
          htmltools::div(style = "margin-bottom: 20px;")
        )
      }
    })

    # out Chromatograms plotly -----
    output$chrom_plotly <- plotly::renderPlotly({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_chromatograms(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$summary_chrom_colorby,
        interactive = input$summary_chrom_interactive
      )
    })

    # out Chromatograms plot -----
    output$chrom_plot <- shiny::renderPlot({
      if (length(reactive_analyses()) == 0) {
        return()
      }
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      plot_chromatograms(
        reactive_analyses(),
        analyses = selected,
        colorBy = input$summary_chrom_colorby,
        interactive = input$summary_chrom_interactive
      )
    })

    # event Chromatograms plot export
    shiny::observeEvent(input$chrom_plot_save, {
      if (length(reactive_analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) {
        return()
      }
      csv <- get_chromatograms(reactive_analyses(), analyses = selected)
      fileinfo <- shinyFiles::parseSavePath(reactive_volumes(), input$chrom_plot_save)
      if (nrow(fileinfo) > 0) {
        utils::write.csv(csv, fileinfo$datapath, row.names = FALSE)
        shiny::showNotification("File saved successfully!", duration = 5, type = "message")
      }
    })

    targets <- shiny::reactiveVal(NULL)

    colorby_targets <- c(
      "targets",
      "analyses",
      "replicates",
      "polarities",
      "targets+analyses",
      "targets+replicates",
      "targets+polarities",
      "analyses+polarities",
      "replicates+polarities"
    )

    # out EICs plot UI -----
    output$eics_interface <- shiny::renderUI({
      if (length(reactive_analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_spectra()) {
        htmltools::div(
          htmltools::div(
            style = "display: flex; align-items: center;",
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::selectInput(
                ns(ns2("eics_analyses")),
                label = "Analyses",
                multiple = TRUE,
                choices = names(reactive_analyses()),
                width = 200
              )
            ),
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::textInput(
                ns(ns2("eics_mass")),
                label = "Mass (Da)",
                value = 238.0547,
                width = 200
              )
            ),
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::textInput(
                ns(ns2("eics_rt")),
                label = "Retention Time (Seconds)",
                value = 1157,
                width = 200
              )
            ),
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::textInput(
                ns(ns2("eics_ppm")),
                label = "Mass Deviation (ppm)",
                value = 20,
                width = 200
              )
            ),
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::textInput(
                ns(ns2("eics_sec")),
                label = "Time deviation (Seconds)",
                value = 60,
                width = 200
              )
            ),
            htmltools::div(
              style = "margin-left: 20px;",
              shiny::actionButton(
                ns(ns2("eics_extract")),
                label = "Extract EIC",
                width = 200
              )
            )
          ),
          htmltools::div(htmltools::br()),
          htmltools::div(style = "margin-left: 20px;", shiny::uiOutput(ns(ns2("eics_targets")))),
          htmltools::div(htmltools::br()),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns(ns2("eics_plotly")), height = "500px"), color = "black"
          ),
          htmltools::div(htmltools::br()),
          shiny::column(12, shiny::uiOutput(ns(ns2("eics_plot_controls")))),
          htmltools::div(style = "margin-bottom: 20px;")
        )
      } else {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No spectra found!"))
      }
    })

    # event EICs extract -----
    shiny::observeEvent(input$eics_extract, {
      anas <- input$eics_analyses
      if (length(anas) == 0) {
        msg <- "No analyses selected!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      mass <- as.numeric(input$eics_mass)
      rt <- as.numeric(input$eics_rt)
      ppm <- as.numeric(input$eics_ppm)
      sec <- as.numeric(input$eics_sec)
      if (is.na(mass) || is.na(rt) || is.na(ppm) || is.na(sec)) {
        msg <- "Invalid input!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      pols <- reactive_analyses()@spectra_polarity[anas]
      tar <- MassSpecTargets(
        mz = data.frame(mass = mass, rt = rt),
        mobility = 0,
        ppm = ppm,
        sec = sec,
        millisec = 0,
        analyses = anas,
        polarities = pols
      )
      tar <- tar@targets
      if (is.null(targets())) {
        targets(tar)
      } else {
        unified <- targets()
        unified <- rbind(unified, tar)
        unified <- unique(unified)
        targets(unified)
      }
    })

    # out EICs targets -----
    output$eics_targets <- shiny::renderUI({
      if (is.null(targets())) {
        return()
      }
      if (nrow(targets()) == 0) {
        return()
      }
      lapply(1:nrow(targets()), function(i) {
        shiny::observeEvent(input[[paste0("eics_del_", i)]],
          {
            unified <- targets()
            unified <- unified[-i, ]
            targets(unified)
          },
          ignoreInit = TRUE
        )

        htmltools::div(
          shiny::actionButton(
            ns(ns2(paste0("eics_del_", i))),
            label = NULL,
            icon = shiny::icon("trash"),
            width = "40px"
          ),
          htmltools::tags$b("  Analysis/Mass/RT/Mobility: "),
          paste0(targets()[i, "analysis"], " ", targets()[i, "id"]),
          htmltools::br()
        )
      })
    })

    # out EICs controls -----
    output$eics_plot_controls <- shiny::renderUI({
      if (is.null(targets())) {
        return()
      }
      if (nrow(targets()) == 0) {
        return()
      }
      htmltools::div(
        style = "display: flex; align-items: center;",
        htmltools::div(
          style = "margin-left: 20px;",
          shiny::selectInput(
            ns(ns2("eics_chrom_colorby")),
            label = "Color by",
            choices = colorby_targets,
            selected = "targets+analyses",
            width = 200
          )
        ),
        htmltools::div(
          style = "margin-left: 20px;",
          shiny::checkboxInput(
            ns(ns2("eics_chrom_interactive")),
            label = "Interactive",
            value = FALSE,
            width = 100
          )
        ),
        htmltools::div(
          style = "margin-left: 20px;",
          shinyFiles::shinySaveButton(
            ns(ns2("eics_plot_save")),
            "Save Plot Data (.csv)",
            "Save Plot Data (.csv)",
            filename = "eics_data",
            filetype = list(csv = "csv")
          )
        ),
        htmltools::div(style = "margin-bottom: 20px;")
      )
    })

    # out EICs plotly -----
    output$eics_plotly <- plotly::renderPlotly({
      if (is.null(targets())) {
        return()
      }
      if (is.null(input$eics_chrom_colorby)) {
        return()
      }
      if (nrow(targets()) == 0) {
        return()
      }
      anas <- unique(targets()[["analysis"]])
      plot_spectra_eic(
        reactive_analyses(),
        mz = targets(),
        colorBy = input$eics_chrom_colorby,
        interactive = TRUE
      )
    })

    # out EICs plot -----
    output$eics_plot <- plotly::renderPlotly({
      if (is.null(targets())) {
        return()
      }
      if (is.null(input$eics_chrom_colorby)) {
        return()
      }
      if (nrow(targets()) == 0) {
        return()
      }
      anas <- unique(targets()[["analysis"]])
      plot_spectra_eic(
        reactive_analyses(),
        mz = targets(),
        colorBy = input$eics_chrom_colorby,
        interactive = TRUE
      )
    })
  })
}
