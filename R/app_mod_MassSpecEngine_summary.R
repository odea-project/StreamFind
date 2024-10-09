#' @title .mod_MassSpecEngine_summary_UI
#' 
#' @description Shiny module UI for MassSpecEngine summary in explorer tab.
#' 
#' @param engine A **MassSpecEngine** object.
#' 
#' @noRd
#' 
.mod_MassSpecEngine_summary_UI <- function(id, engine) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabBox(width = 12, height = "1080px",
    shiny::tabPanel("Spectra",
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns("summary_plot_controls"))),
        shiny::column(12, shiny::uiOutput(ns("summary_plot_ui"))),
        shiny::column(12, DT::dataTableOutput(ns("spectraAnalysesTable")))
      )
    ),
    shiny::tabPanel("Chromatograms",
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns("chrom_plot_controls"))),
        shiny::column(12, shiny::uiOutput(ns("chrom_plot_ui"))),
        shiny::column(12, DT::dataTableOutput(ns("chromAnalysesTable")))
      )
    ),
    shiny::tabPanel("Extract Ion Chromatograms",
      shiny::uiOutput(ns("eics_interface"))
    )
  )
}

#' @title .mod_MassSpecEngine_summary_Server
#' 
#' @description Shiny module server for MassSpecEngine summary in explorer tab.
#' 
#' @param engine A **MassSpecEngine** object.
#' @param analyses A reactive `data.frame` of analyses overview.
#' @param volumes A list of volumes for saving destinations.
#' 
#' @noRd
#' 
.mod_MassSpecEngine_summary_Server <- function(id, engine, analyses, volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyFiles::shinyFileSave(input, "summary_plot_save", roots = volumes, defaultRoot = "wd", session = session)
    shinyFiles::shinyFileSave(input, "chrom_plot_save", roots = volumes, defaultRoot = "wd", session = session)
    
    has_spectra <- shiny::reactiveVal(nrow(engine$get_spectra_headers()) > 0)
    has_chromatograms <- shiny::reactiveVal(nrow(engine$get_chromatograms_headers()) > 0)
    
    levels <- unique(engine$get_spectra_level())
    rt_end <- round(max(engine$get_spectra_highest_rt()), digits = 0)
    rt_start <- round(min(engine$get_spectra_lowest_rt()), digits = 0)
    colorby_spectra <- c("analyses", "replicates", "polarities", "analyses+polarities", "replicates+polarities")
    colorby_chromatograms <- c(
      "targets", "analyses",
      "replicates", "polarities",
      "targets+polarities",
      "analyses+polarities",
      "replicates+polarities",
      "targets+analyses",
      "targets+replicates"
    )
    
    output$spectraAnalysesTable <- DT::renderDT({
      DT::datatable(
        analyses()[, c("analysis", "replicate", "blank", "polarity"), with = FALSE],
        selection = list(mode = 'multiple', selected = 1, target = 'row'),
        options = list(pageLength = 10)
      )
    })
    
    # out Summary plot UI -----
    output$summary_plot_ui <- shiny::renderUI({
      if (nrow(analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_spectra()) {
        if (!is.null(input$summary_plot_interactive)) {
          if (input$summary_plot_interactive) {
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("summary_plotly"), height = "600px"), color = "black")
          } else {
            shinycssloaders::withSpinner(shiny::plotOutput(ns("summary_plot"), height = "600px"), color = "black")
          }
        }
      } else {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No spectra found!"))
      }
    })
    
    # out Summary controls -----
    output$summary_plot_controls <- shiny::renderUI({
      if (nrow(analyses()) == 0) return()
      if (has_spectra()) {
        htmltools::div(style = "display: flex; align-items: center;",
          htmltools::div(style = "margin-left: 20px;", shiny::checkboxInput(ns("summary_plot_interactive"), label = "Interactive", value = TRUE, width = 100)),
          htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("summary_plot_type"), label = "Type", choices = c("TIC", "BPC"), selected = "TIC", width = 100)),
          htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("summary_plot_colorby"), label = "Color by", choices = colorby_spectra, selected = "analyses", width = 200)),
          htmltools::div(style = "margin-left: 20px;", shiny::sliderInput(ns("summary_plot_level"), label = "MS levels", min = min(levels), max = max(levels), value = levels, step = 1, width = 200)),
          htmltools::div(style = "margin-left: 20px;", shiny::sliderInput(ns("summary_plot_rt"), label = "Retention Time (Seconds)", min = rt_start, max = rt_end, value = c(rt_start, rt_end), step = 1, width = 200)),
          htmltools::div(style = "margin-left: 20px;", shiny::numericInput(ns("summary_plot_downsize"), label = "Downsize Integer", min = 1, max = 100, value = 1, step = 1, width = 140)),
          htmltools::div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("summary_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "spectra_summary_data", filetype = list(csv = "csv"))),
          htmltools::div(style =  "margin-bottom: 20px;")
        )
      }
    })
    
    # out Summary plotly -----
    output$summary_plotly <- plotly::renderPlotly({
      if (nrow(analyses()) == 0) return()
      if (!is.null(input$summary_plot_type) && input$summary_plot_interactive) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) return()
        if (input$summary_plot_type %in% "TIC") {
          engine$plot_spectra_tic(analyses = selected, colorBy = input$summary_plot_colorby, level = input$summary_plot_level, rt = input$summary_plot_rt, downsize = input$summary_plot_downsize, interactive = input$summary_plot_interactive)
        } else if (input$summary_plot_type %in% "BPC") {
          engine$plot_spectra_bpc(analyses = selected, colorBy = input$summary_plot_colorby, level = input$summary_plot_level, rt = input$summary_plot_rt, interactive = input$summary_plot_interactive)
        }
      }
    })
    
    # out Summary plot -----
    output$summary_plot <- shiny::renderPlot({
      if (nrow(analyses()) == 0) return()
      if (!is.null(input$summary_plot_type) && !input$summary_plot_interactive) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) return()
        if (input$summary_plot_type %in% "TIC") {
          engine$plot_spectra_tic(analyses = selected, colorBy = input$summary_plot_colorby, level = input$summary_plot_level, rt = input$summary_plot_rt, downsize = input$summary_plot_downsize, interactive = input$summary_plot_interactive)
        } else if (input$summary_plot_type %in% "BPC") {
          engine$plot_spectra_bpc(analyses = selected, colorBy = input$summary_plot_colorby, level = input$summary_plot_level, rt = input$summary_plot_rt, interactive = input$summary_plot_interactive)
        }
      }
    })
    
    # event Summary plot export -----
    shiny::observeEvent(input$summary_plot_save, {
      if (nrow(analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      if (!is.null(input$summary_plot_type)) {
        selected <- input$spectraAnalysesTable_rows_selected
        if (length(selected) == 0) return()
        if (input$summary_plot_type %in% "TIC") {
          csv <- engine$get_spectra_tic(analyses = selected, level = input$summary_plot_level)
        } else if (input$summary_plot_type %in% "BPC") {
          csv <- engine$get_spectra_bpc(analyses = selected, level = input$summary_plot_level)
        }
        fileinfo <- shinyFiles::parseSavePath(volumes, input$summary_plot_save)
        if (nrow(fileinfo) > 0) {
          utils::write.csv(csv, fileinfo$datapath, row.names = FALSE)
          shiny::showNotification("File saved successfully!", duration = 5, type = "message")
        }
      }
    })
    
    output$chromAnalysesTable <- DT::renderDT({
      DT::datatable(
        analyses()[, c("analysis", "replicate", "blank", "polarity"), with = FALSE],
        selection = list(mode = 'multiple', selected = 1, target = 'row'),
        options = list(pageLength = 10)
      )
    })
    
    # out Chromatograms plot UI -----
    output$chrom_plot_ui <- shiny::renderUI({
      if (nrow(analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_chromatograms()) {
        if (!is.null(input$summary_chrom_interactive)) {
          if (input$summary_chrom_interactive) {
            plotly::plotlyOutput(ns("chrom_plotly"), height = "600px")
          } else {
            shiny::plotOutput(ns("chrom_plot"), height = "600px")
          }
        }
      } else {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No chromatograms found!"))
      }
    })
    
    # out Chromatograms controls -----
    output$chrom_plot_controls <- shiny::renderUI({
      if (nrow(analyses()) == 0) return()
      if (has_chromatograms()) {
        htmltools::div(style = "display: flex; align-items: center;",
          htmltools::div(style = "margin-left: 20px;", shiny::checkboxInput(ns("summary_chrom_interactive"), label = "Interactive", value = TRUE, width = 100)),
          htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("summary_chrom_colorby"), label = "Color by", choices = colorby_chromatograms, selected = "targets", width = 200)),
          htmltools::div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("chrom_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "chrom_data", filetype = list(csv = "csv"))),
          htmltools::div(style =  "margin-bottom: 20px;")
        )
      }
    })
    
    # out Chromatograms plotly -----
    output$chrom_plotly <- plotly::renderPlotly({
      if (nrow(analyses()) == 0) return()
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) return()
      engine$plot_chromatograms(analyses = selected, colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
    })
    
    # out Chromatograms plot -----
    output$chrom_plot <- shiny::renderPlot({
      if (nrow(analyses()) == 0) return()
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) return()
      engine$plot_chromatograms(analyses = selected, colorBy = input$summary_chrom_colorby, interactive = input$summary_chrom_interactive)
    })
    
    # event Chromatograms plot export
    shiny::observeEvent(input$chrom_plot_save, {
      if (nrow(analyses()) == 0) {
        msg <- "No analyses found!"
        shiny::showNotification(msg, duration = 5, type = "warning")
        return()
      }
      selected <- input$chromAnalysesTable_rows_selected
      if (length(selected) == 0) return()
      csv <- engine$get_chromatograms(analyses = selected)
      fileinfo <- shinyFiles::parseSavePath(volumes, input$chrom_plot_save)
      if (nrow(fileinfo) > 0) {
        utils::write.csv(csv, fileinfo$datapath, row.names = FALSE)
        shiny::showNotification("File saved successfully!", duration = 5, type = "message")
      }
    })
    
    targets <- shiny::reactiveVal(NULL)
    colorby_targets <- c("targets", "analyses", "replicates", "polarities", "targets+analyses", "targets+replicates", "targets+polarities", "analyses+polarities", "replicates+polarities")
    
    # out EICs plot UI -----
    output$eics_interface <- shiny::renderUI({
      if (nrow(analyses()) == 0) {
        htmltools::div(style = "margin-top: 20px;", htmltools::h4("No analyses found!"))
      } else if (has_spectra()) {
        
        htmltools::div(
          htmltools::div(style = "display: flex; align-items: center;",
            htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("eics_analyses"), label = "Analyses", multiple = TRUE, choices = analyses()[["analysis"]], width = 200)),
            htmltools::div(style = "margin-left: 20px;", shiny::textInput(ns("eics_mass"), label = "Mass (Da)", value = 238.0547, width = 200)),
            htmltools::div(style = "margin-left: 20px;", shiny::textInput(ns("eics_rt"), label = "Retention Time (Seconds)", value = 1157, width = 200)),
            htmltools::div(style = "margin-left: 20px;", shiny::textInput(ns("eics_ppm"), label = "Mass Deviation (ppm)", value = 20, width = 200)),
            htmltools::div(style = "margin-left: 20px;", shiny::textInput(ns("eics_sec"), label = "Time deviation (Seconds)", value = 60, width = 200)),
            htmltools::div(style = "margin-left: 20px;", shiny::actionButton(ns("eics_extract"), label = "Extract EIC", width = 200)),
          ),
          htmltools::div(htmltools::br()),
          htmltools::div(style = "margin-left: 20px;", shiny::uiOutput(ns("eics_targets"))),
          htmltools::div(htmltools::br()),
          shinycssloaders::withSpinner(plotly::plotlyOutput(ns("eics_plotly"), height = "500px"), color = "black"),
          htmltools::div(htmltools::br()),
          shiny::column(12, shiny::uiOutput(ns("eics_plot_controls"))),
          htmltools::div(style =  "margin-bottom: 20px;")
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
      tar <- make_ms_targets(mz = mass, rt = rt, drift = 0, ppm = ppm, sec = sec, millisec = 0)
      data.table::setnames(tar, c("mz", "mzmin", "mzmax"), c("mass", "min", "max"))
      
      for (i in 1:length(anas)) {
        tar$analysis <- anas[i]
        if (is.null(targets())) {
          targets(tar)
        } else {
          unified <- targets()
          unified <- rbind(unified, tar)
          unified <- unique(unified)
          targets(unified)
        }
      }
    })
    
    # out EICs targets -----
    output$eics_targets <- shiny::renderUI({
      if (is.null(targets())) return()
      if (nrow(targets()) == 0) return()
      lapply(1:nrow(targets()), function(i) {
        
        shiny::observeEvent(input[[paste0("eics_del_", i)]], {
          unified <- targets()
          unified <- unified[-i, ]
          targets(unified)
        }, ignoreInit = TRUE)
        
        htmltools::div(
          shiny::actionButton(ns(paste0("eics_del_", i)), label = NULL, icon = shiny::icon("trash"), width = '40px'),
          htmltools::tags$b("  Analysis/Mass/RT/Mobility: "), paste0(targets()[i, "analysis"], " ", targets()[i, "id"]), htmltools::br()
        )
      })
    })
    
    # out EICs controls -----
    output$eics_plot_controls <- shiny::renderUI({
      if (is.null(targets())) return()
      if (nrow(targets()) == 0) return()
      htmltools::div(style = "display: flex; align-items: center;",
        htmltools::div(style = "margin-left: 20px;", shiny::selectInput(ns("eics_chrom_colorby"), label = "Color by", choices = colorby_targets, selected = "targets+analyses", width = 200)),
        htmltools::div(style = "margin-left: 20px;", shiny::checkboxInput(ns("eics_chrom_interactive"), label = "Interactive", value = FALSE, width = 100)),
        htmltools::div(style = "margin-left: 20px;", shinyFiles::shinySaveButton(ns("eics_plot_save"), "Save Plot Data (.csv)", "Save Plot Data (.csv)", filename = "eics_data", filetype = list(csv = "csv"))),
        htmltools::div(style =  "margin-bottom: 20px;")
      )
    })
    
    # out EICs plotly -----
    output$eics_plotly <- plotly::renderPlotly({
      if (is.null(targets())) return()
      if (is.null(input$eics_chrom_colorby)) return()
      if (nrow(targets()) == 0) return()
      anas <- unique(targets()[["analysis"]])
      mass <- targets()
      mass$analysis <- NULL
      mass <- unique(mass)
      engine$plot_spectra_eic(analyses = anas, mass = mass, colorBy = input$eics_chrom_colorby, interactive = TRUE)
    })
    
    # out EICs plot -----
    output$eics_plot <- plotly::renderPlotly({
      if (is.null(targets())) return()
      if (is.null(input$eics_chrom_colorby)) return()
      if (nrow(targets()) == 0) return()
      anas <- unique(targets()[["analysis"]])
      mass <- targets()
      mass$analysis <- NULL
      mass <- unique(mass)
      engine$plot_spectra_eic(analyses = anas, mass = mass, colorBy = input$eics_chrom_colorby, interactive = TRUE)
    })
    
  })
}