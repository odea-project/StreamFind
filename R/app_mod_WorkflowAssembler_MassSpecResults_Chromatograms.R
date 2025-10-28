#' @export
#' @noRd
.mod_WorkflowAssembler_Result_UI.MassSpecResults_Chromatograms <- function(
  x,
  id,
  ns
) {
  ns_full <- shiny::NS(paste0("WorkflowAssembler-", id))

  # MARK: Custom CSS
  # Custom CSS ----
  custom_css <- shiny::tags$style(
    shiny::HTML(
      "
    .status-panel {
      background-color: white;
      border-radius: 8px;
      padding: 20px;
      height: 100%;
    }
    .status-item {
      display: flex;
      justify-content: space-between;
      padding: 8px 0;
      border-bottom: 1px solid rgba(0,0,0,0.05);
    }
    .status-item:last-child {
      border-bottom: none;
    }
    .status-label {
      font-weight: 400;
    }
    .status-value {
      font-weight: 600;
    }
    .status-yes {
      color: #28a745;
    }
    .status-no {
      color: #dc3545;
    }
    .tab-content {
      padding: 0;
    }
    .nav-tabs {
      border-bottom: 2px solid #e3e6f0;
    }
    .nav-tabs .nav-link.active {
      border-color: transparent;
      border-bottom: 3px solid #4e73df;
      font-weight: 600;
    }
    .nav-tabs .nav-link {
      border: none;
      color: #5a5c69;
      padding: 10px 15px;
    }
    .nav-tabs .nav-link:hover {
      border-color: transparent;
      border-bottom: 3px solid #a2aecf;
    }
    .plot-container {
      border-radius: 8px;
      background-color: white;
      padding: 15px;
      box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);
    }

    /* Compact form controls */
    .form-check-label {
      font-size: 13px;
      color: #5a5c69;
      margin-left: 5px;
      white-space: nowrap;
    }

    .form-check {
      margin-bottom: 0;
    }

    /* Plot container adjustments for single page */
    .plot-container {
      margin-bottom: 0;
    }

    /* Remove padding from tabBox container */
    .tabbox-container .col-sm-12 {
      padding-left: 0 !important;
      padding-right: 0 !important;
    }

    .nav-tabs-custom {
      margin-bottom: 0px;
    }

  "
    )
  )

  # MARK: Plot Maximize Functions
  # Plot Maximize Functions ----
  source_functions <- list(
    .app_util_create_maximize_button = .app_util_create_maximize_button,
    .app_util_create_plot_modal = .app_util_create_plot_modal,
    .app_util_plot_maximize_js = .app_util_plot_maximize_js
  )

  # MARK: Main UI
  # Main UI ----
  shiny::tagList(
    custom_css,
    .app_util_plot_maximize_js(), # JavaScript functions
    .app_util_create_plot_modal(ns_full), # Modal container

    # MARK: Main Content
    ## Main Content ----
    shiny::div(
      class = "tabbox-container",
      shinydashboard::tabBox(
        id = ns_full("main_tabs"),
        width = 12,
        height = "calc(100vh - 50px - 30px - 45px - 10px)",

        # MARK: Overview Tab
        ### Overview Tab ----
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-line", class = "mr-2"), "Overview"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",
            bslib::layout_sidebar(
              # MARK: Sidebar - Status Panel
              sidebar = bslib::sidebar(
                shiny::div(
                  class = "status-panel",
                  # Total Chromatograms
                  shiny::div(
                    class = "status-item",
                    shiny::span(class = "status-label", "Total Chromatograms:"),
                    shiny::span(class = "status-value", shiny::textOutput(ns_full("total_chromatograms"), inline = TRUE))
                  ),
                  # Is Averaged
                  shiny::div(
                    class = "status-item",
                    shiny::span(class = "status-label", "Averaged:"),
                    shiny::span(class = "status-value", shiny::uiOutput(ns_full("is_averaged_ui"), inline = TRUE))
                  ),
                  # Has Peaks
                  shiny::div(
                    class = "status-item",
                    shiny::span(class = "status-label", "Has Peaks:"),
                    shiny::span(class = "status-value", shiny::uiOutput(ns_full("has_peaks_ui"), inline = TRUE))
                  ),
                  # Total Peaks
                  shiny::div(
                    class = "status-item",
                    shiny::span(class = "status-label", "Total Peaks:"),
                    shiny::span(class = "status-value", shiny::textOutput(ns_full("total_peaks"), inline = TRUE))
                  ),
                  # Has Calibration
                  shiny::div(
                    class = "status-item",
                    shiny::span(class = "status-label", "Has Calibration:"),
                    shiny::span(class = "status-value", shiny::uiOutput(ns_full("has_calibration_ui"), inline = TRUE))
                  )
                )
              ),

              # Main Content - Summary Table
              shiny::div(
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 45px); display: flex; flex-direction: column;",
                # Top Controls Bar
                shiny::div(
                  class = "d-flex justify-content-center align-items-center",
                  style = "height: 60px; background-color: white; padding: 10px;",
                  shiny::h4("MassSpecResults_Chromatograms Summary", style = "margin: 0; color: #5a5c69;")
                ),
                # Table Container
                shiny::column(
                  width = 12,
                  style = "flex: 1; background-color: white; padding: 5px;",
                  DT::dataTableOutput(
                    ns_full("chromatograms_summary_table"),
                    height = "100%",
                    width = "100%"
                  )
                )
              )
            )
          )
        ),

        # MARK: Chromatograms Tab
        ### Chromatograms Tab ----
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-area", class = "mr-2"), "Chromatograms"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",

            # MARK: Top Controls Section
            shiny::div(
              class = "chromatograms-controls-bar",
              style = "
                background-color: white;
                padding: 10px 15px;
                height: 60px;
                display: flex;
                align-items: center;
                justify-content: space-between;
              ",
              # Left: Control inputs
              shiny::div(
                style = "display: flex; align-items: center; gap: 15px;",
                shiny::div(
                  class = "form-check",
                  style = "display: flex; align-items: center;",
                  shiny::tags$input(
                    type = "checkbox",
                    class = "form-check-input",
                    id = ns_full("chromatograms_plot_interactive"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("chromatograms_plot_interactive"),
                    "Interactive",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  )
                ),
                shiny::div(
                  style = "display: flex; align-items: center; gap: 5px;",
                  shiny::tags$label(
                    "Color by:",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  ),
                  shiny::selectInput(
                    ns_full("chromatograms_plot_colorby"),
                    label = NULL,
                    choices = c("analyses", "replicates"),
                    selected = "analyses",
                    width = "120px"
                  )
                ),
                shiny::div(
                  style = "display: flex; align-items: center; gap: 5px;",
                  shiny::tags$label(
                    "Normalized:",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  ),
                  shiny::selectInput(
                    ns_full("chromatograms_normalized"),
                    label = NULL,
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = TRUE,
                    width = "100px"
                  )
                )
              ),
              # Right: Export and Layout controls
              shiny::div(
                class = "proportion-controls",
                style = "display: flex; align-items: center; gap: 10px;",
                shiny::downloadButton(
                  ns_full("chromatograms_plot_save"),
                  "Export Data (.csv)",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm"
                ),
                shiny::span(
                  "Layout:",
                  style = "font-weight: 500; margin-right: 10px;"
                ),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionButton(
                    ns_full("prop_20_80"),
                    "20:80",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("prop_30_70"),
                    "30:70",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("prop_40_60"),
                    "40:60",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("prop_50_50"),
                    "50:50",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("prop_60_40"),
                    "60:40",
                    class = "btn btn-outline-primary btn-sm active"
                  ),
                  shiny::actionButton(
                    ns_full("prop_70_30"),
                    "70:30",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("prop_80_20"),
                    "80:20",
                    class = "btn btn-outline-primary btn-sm"
                  )
                )
              )
            ),

            # MARK: Main Content
            ### Main Content -----
            shiny::div(
              id = ns_full("main_content_container"),
              style = "display: flex; height: calc(100vh - 50px - 30px - 60px - 45px - 80px);",

              # Left Side - Analyses Table (Dynamic width)
              shiny::div(
                id = ns_full("chromatograms_table_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: auto;",
                DT::dataTableOutput(ns_full("chromatogramsAnalysesTable"), height = "auto", width = "98%")
              ),

              # Right Side - Chromatograms Plot (Dynamic width)
              shiny::div(
                id = ns_full("chromatograms_plots_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: hidden;",
                shiny::div(
                  style = "height: 30px; position: relative;",
                  .app_util_create_maximize_button("chromatograms_plot", ns_full),
                ),
                shiny::uiOutput(ns_full("chromatograms_plot_ui"))
              )
            )
          )
        ),

        # MARK: Peaks Tab
        ### Peaks Tab ----
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("mountain", class = "mr-2"), "Peaks"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",

            # MARK: Top Controls Section
            shiny::div(
              class = "peaks-controls-bar",
              style = "
                background-color: white;
                padding: 10px 15px;
                height: 60px;
                display: flex;
                align-items: center;
                justify-content: space-between;
              ",
              # Left: Control inputs
              shiny::div(
                style = "display: flex; align-items: center; gap: 15px;",
                shiny::div(
                  class = "form-check",
                  style = "display: flex; align-items: center;",
                  shiny::tags$input(
                    type = "checkbox",
                    class = "form-check-input",
                    id = ns_full("peaks_plot_interactive"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("peaks_plot_interactive"),
                    "Interactive",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  )
                ),
                shiny::div(
                  style = "display: flex; align-items: center; gap: 5px;",
                  shiny::tags$label(
                    "Color by:",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  ),
                  shiny::selectInput(
                    ns_full("peaks_plot_colorby"),
                    label = NULL,
                    choices = c("analyses", "replicates"),
                    selected = "analyses",
                    width = "120px"
                  )
                )
              ),
              # Right: Export and Layout controls
              shiny::div(
                class = "proportion-controls",
                style = "display: flex; align-items: center; gap: 10px;",
                shiny::downloadButton(
                  ns_full("peaks_plot_save"),
                  "Export Peaks (.csv)",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm"
                ),
                shiny::span(
                  "Layout:",
                  style = "font-weight: 500; margin-right: 10px;"
                ),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionButton(
                    ns_full("peaks_prop_20_80"),
                    "20:80",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_30_70"),
                    "30:70",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_40_60"),
                    "40:60",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_50_50"),
                    "50:50",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_60_40"),
                    "60:40",
                    class = "btn btn-outline-primary btn-sm active"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_70_30"),
                    "70:30",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("peaks_prop_80_20"),
                    "80:20",
                    class = "btn btn-outline-primary btn-sm"
                  )
                )
              )
            ),

            # MARK: Main Content - Peaks
            ### Main Content - Peaks -----
            shiny::div(
              id = ns_full("peaks_main_content_container"),
              style = "display: flex; height: calc(100vh - 50px - 30px - 60px - 45px - 80px);",

              # Left Side - Peaks Table (Dynamic width)
              shiny::div(
                id = ns_full("peaks_table_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: auto;",
                DT::dataTableOutput(ns_full("peaks_table"), height = "auto", width = "98%")
              ),

              # Right Side - Peaks Plot (Dynamic width)
              shiny::div(
                id = ns_full("peaks_plots_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: hidden;",
                shiny::div(
                  style = "height: 30px; position: relative;",
                  .app_util_create_maximize_button("peaks_plot", ns_full),
                ),
                shiny::uiOutput(ns_full("peaks_plot_ui"))
              )
            )
          )
        )
      )
    )
  )
}

#' @export
#' @noRd
.mod_WorkflowAssembler_Result_Server.MassSpecResults_Chromatograms <- function(
  x,
  id,
  ns,
  reactive_analyses,
  reactive_volumes,
  reactive_config
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive value to store the MassSpecResults_Chromatograms object
    chromatograms_results <- shiny::reactiveVal()

    # Initialize with the MassSpecResults_Chromatograms object from input
    shiny::observe({
      shiny::validate(shiny::need(!is.null(x), "Chromatograms data is not available"))
      chromatograms_results(x)
    })

    # MARK: Overview Metrics
    # Overview Metrics -----
    output$total_chromatograms <- shiny::renderText({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) return("0")
      as.character(length(chromatograms_obj$chromatograms))
    })

    output$is_averaged_ui <- shiny::renderUI({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj) || length(chromatograms_obj$is_averaged) == 0) {
        value <- FALSE
      } else {
        value <- chromatograms_obj$is_averaged
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_peaks_ui <- shiny::renderUI({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) {
        value <- FALSE
      } else {
        value <- length(chromatograms_obj$peaks) > 0
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$total_peaks <- shiny::renderText({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj) || length(chromatograms_obj$peaks) == 0) return("0")
      total <- sum(vapply(chromatograms_obj$peaks, nrow, 0))
      as.character(total)
    })

    output$has_calibration_ui <- shiny::renderUI({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) {
        value <- FALSE
      } else {
        value <- length(chromatograms_obj$calibration_model) > 0
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    # MARK: Overview Summary Table
    # Overview Summary Table -----
    output$chromatograms_summary_table <- DT::renderDT({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) {
        return(DT::datatable(
          data.frame(Message = "No MassSpecResults_Chromatograms object available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      # Create summary table from the chromatograms object
      analyses_names <- names(chromatograms_obj$chromatograms)
      if (length(analyses_names) == 0) {
        return(DT::datatable(
          data.frame(Message = "No chromatograms data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          rownames = FALSE
        ))
      }

      summary_data <- data.frame(
        Analysis = analyses_names,
        Chromatograms_Count = vapply(chromatograms_obj$chromatograms, nrow, 0),
        Has_Peaks = vapply(analyses_names, function(x) {
          x %in% names(chromatograms_obj$peaks) && nrow(chromatograms_obj$peaks[[x]]) > 0
        }, FALSE),
        Peak_Count = vapply(analyses_names, function(x) {
          if (x %in% names(chromatograms_obj$peaks)) nrow(chromatograms_obj$peaks[[x]]) else 0
        }, 0),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        summary_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          processing = TRUE,
          dom = 'rt<"bottom"lip>',
          lengthMenu = c(10, 15, 25, 50),
          ordering = TRUE,
          searching = TRUE
        ),
        style = "bootstrap",
        class = "table table-striped table-hover",
        rownames = FALSE,
        filter = "top"
      )
    })

    # MARK: Analyses Table for Selection
    # Analyses Table for Selection -----
    output$chromatogramsAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      chromatograms_obj <- chromatograms_results()

      if (is.null(analyses) || is.null(chromatograms_obj)) {
        return(DT::datatable(
          data.frame(Message = "No analyses or chromatograms data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      info <- info(analyses)[, c("analysis", "replicate", "blank"), with = FALSE]

      # Filter to analyses that have chromatograms data
      analyses_with_chromatograms <- names(chromatograms_obj$chromatograms)[
        vapply(chromatograms_obj$chromatograms, function(z) nrow(z) > 0, FALSE)
      ]

      if (length(chromatograms_obj$is_averaged) > 0 && chromatograms_obj$is_averaged) {
        info <- info[info$replicate %in% analyses_with_chromatograms, ]
      } else {
        info <- info[info$analysis %in% analyses_with_chromatograms, ]
      }

      if (nrow(info) == 0) {
        return(DT::datatable(
          data.frame(Message = "No analyses with chromatograms data found"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          rownames = FALSE
        ))
      }

      DT::datatable(
        info,
        escape = FALSE,
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          scrollX = TRUE,
          processing = TRUE,
          scrollY = TRUE,
          scrollCollapse = TRUE,
          paging = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(1))
          ),
          selection = list(mode = "multiple", selected = 1, target = "row"),
          lengthMenu = c(10, 25, 50),
          ordering = TRUE,
          searching = TRUE,
          searchHighlight = TRUE
        ),
        style = "bootstrap",
        class = "table table-striped table-hover",
        rownames = FALSE,
        filter = "top",
        selection = "multiple"
      )
    })

    # MARK: Helper functions to get selected analyses for each tab
    # Helper function to get selected analyses for Chromatograms tab
    get_selected_analyses_chromatograms <- shiny::reactive({
      selected_rows <- input$chromatogramsAnalysesTable_rows_selected
      analyses <- reactive_analyses()
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_rows) || length(selected_rows) == 0 || is.null(analyses) || is.null(chromatograms_obj)) {
        return(NULL)
      }

      info <- info(analyses)[, c("analysis", "replicate", "blank"), with = FALSE]
      analyses_with_chromatograms <- names(chromatograms_obj$chromatograms)[
        vapply(chromatograms_obj$chromatograms, function(z) nrow(z) > 0, FALSE)
      ]

      if (length(chromatograms_obj$is_averaged) > 0 && chromatograms_obj$is_averaged) {
        info <- info[info$replicate %in% analyses_with_chromatograms, ]
        return(info$replicate[selected_rows])
      } else {
        info <- info[info$analysis %in% analyses_with_chromatograms, ]
        return(info$analysis[selected_rows])
      }
    })

    # Helper function to get selected analyses for Peaks tab (uses peaks table selection)
    get_selected_analyses_peaks <- shiny::reactive({
      selected_rows <- input$peaks_table_rows_selected
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_rows) || length(selected_rows) == 0 || is.null(chromatograms_obj)) {
        return(NULL)
      }

      # Get peaks data to extract analysis/replicate info from selected rows
      tryCatch({
        peaks_data <- get_chromatograms_peaks(chromatograms_obj)
        if (is.null(peaks_data) || nrow(peaks_data) == 0) {
          return(NULL)
        }

        # Extract unique analysis/replicate values from selected rows
        selected_data <- peaks_data[selected_rows, ]
        if ("replicate" %in% names(selected_data)) {
          return(unique(selected_data$replicate))
        } else if ("analysis" %in% names(selected_data)) {
          return(unique(selected_data$analysis))
        }
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    })

    # MARK: Chromatograms Plot UI
    # Chromatograms Plot UI -----
    output$chromatograms_plot_ui <- shiny::renderUI({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) {
        htmltools::div(
          style = "margin-top: 20px;",
          htmltools::h4("No MassSpecResults_Chromatograms object available!")
        )
      } else if (!is.null(input$chromatograms_plot_interactive)) {
        if (input$chromatograms_plot_interactive) {
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              session$ns("chromatograms_plotly"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        } else {
          shinycssloaders::withSpinner(
            shiny::plotOutput(
              session$ns("chromatograms_plot"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        }
      }
    })

    # MARK: Chromatograms Plotly
    # Chromatograms Plotly -----
    output$chromatograms_plotly <- plotly::renderPlotly({
      selected_analyses <- get_selected_analyses_chromatograms()
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_analyses) || is.null(chromatograms_obj)) {
        plotly::plot_ly() %>%
          plotly::add_text(
            x = 0.5,
            y = 0.5,
            text = "Please select analyses from the table to display chromatograms",
            textfont = list(size = 16, color = "#666")
          ) %>%
          plotly::layout(
            title = "No Selection",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE)
          )
      } else {
        tryCatch({
          p <- plot_chromatograms(
            chromatograms_obj,
            analyses = selected_analyses,
            colorBy = input$chromatograms_plot_colorby,
            normalized = as.logical(input$chromatograms_normalized),
            interactive = TRUE,
            title = "Chromatograms Plot"
          )

          if (!is.null(p)) {
            # Enhance the plotly object
            p <- plotly::layout(
              p,
              title = list(text = "Chromatograms Plot", font = list(size = 16)),
              xaxis = list(title = "Retention Time (s)"),
              yaxis = list(title = "Intensity"),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              showlegend = TRUE,
              hovermode = "closest"
            )

            # Add interactive features
            p <- plotly::config(
              p,
              displayModeBar = TRUE,
              toImageButtonOptions = list(
                format = 'png',
                filename = 'chromatograms_plot',
                height = 600,
                width = 800,
                scale = 1
              ),
              modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
            )
          }

          return(p)
        }, error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading chromatograms:", e$message),
              textfont = list(size = 14, color = "red")
            ) %>%
            plotly::layout(
              title = "Error",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        })
      }
    })

    # MARK: Chromatograms Plot (Static)
    # Chromatograms Plot (Static) -----
    output$chromatograms_plot <- shiny::renderPlot({
      selected_analyses <- get_selected_analyses_chromatograms()
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_analyses) || is.null(chromatograms_obj)) {
        return(NULL)
      }

      tryCatch({
        plot_chromatograms(
          chromatograms_obj,
          analyses = selected_analyses,
          colorBy = input$chromatograms_plot_colorby,
          normalized = as.logical(input$chromatograms_normalized),
          interactive = FALSE,
          title = "Chromatograms Plot"
        )
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
        text(1, 1, paste("Error loading chromatograms:", e$message), col = "red")
      })
    })

    # MARK: Export Chromatograms Data
    # Export Chromatograms Data -----
    output$chromatograms_plot_save <- shiny::downloadHandler(
      filename = function() {
        paste0("chromatograms_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        selected_analyses <- get_selected_analyses_chromatograms()
        chromatograms_obj <- chromatograms_results()

        if (is.null(selected_analyses) || is.null(chromatograms_obj)) {
          write.csv(
            data.frame(Error = "No data selected or available"),
            file, row.names = FALSE
          )
          return()
        }

        tryCatch({
          csv_data <- get_chromatograms(chromatograms_obj, analyses = selected_analyses)
          csv_data <- data.table::rbindlist(csv_data, fill = TRUE)
          write.csv(csv_data, file, row.names = FALSE)
        }, error = function(e) {
          write.csv(
            data.frame(Error = paste("Failed to export data:", e$message)),
            file, row.names = FALSE
          )
        })
      }
    )

    # MARK: Peaks Table and Plot
    # Peaks Table and Plot -----
    output$peaks_table <- DT::renderDT({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj) || length(chromatograms_obj$peaks) == 0) {
        return(DT::datatable(
          data.frame(Message = "No peaks data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      tryCatch({
        peaks_data <- get_chromatograms_peaks(chromatograms_obj)

        if (!is.null(peaks_data) && nrow(peaks_data) > 0) {
          DT::datatable(
            peaks_data,
            options = list(
              scrollX = TRUE,
              scrollY = "300px",
              pageLength = 10,
              lengthMenu = c(10, 25, 50, 100),
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            ),
            extensions = 'Buttons',
            rownames = FALSE
          )
        } else {
          DT::datatable(
            data.frame(Message = "No peaks data available"),
            options = list(dom = 't'),
            rownames = FALSE
          )
        }
      }, error = function(e) {
        DT::datatable(
          data.frame(Error = paste("Error loading peaks data:", e$message)),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })

    output$peaks_plot_ui <- shiny::renderUI({
      chromatograms_obj <- chromatograms_results()
      if (is.null(chromatograms_obj)) {
        htmltools::div(
          style = "margin-top: 20px;",
          htmltools::h4("No MassSpecResults_Chromatograms object available!")
        )
      } else if (!is.null(input$peaks_plot_interactive)) {
        if (input$peaks_plot_interactive) {
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              session$ns("peaks_plotly"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        } else {
          shinycssloaders::withSpinner(
            shiny::plotOutput(
              session$ns("peaks_plot"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        }
      }
    })

    output$peaks_plotly <- plotly::renderPlotly({
      selected_analyses <- get_selected_analyses_peaks()
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_analyses) || is.null(chromatograms_obj)) {
        plotly::plot_ly() %>%
          plotly::add_text(
            x = 0.5,
            y = 0.5,
            text = "Please select analyses from the table to display peaks",
            textfont = list(size = 16, color = "#666")
          ) %>%
          plotly::layout(
            title = "No Selection",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE)
          )
      } else {
        tryCatch({
          p <- plot_chromatograms_peaks(
            chromatograms_obj,
            analyses = selected_analyses,
            colorBy = input$peaks_plot_colorby,
            interactive = TRUE,
            title = "Chromatograms Peaks Plot"
          )

          if (!is.null(p)) {
            # Enhance the plotly object
            p <- plotly::layout(
              p,
              title = list(text = "Chromatograms Peaks Plot", font = list(size = 16)),
              xaxis = list(title = "Retention Time (s)"),
              yaxis = list(title = "Intensity"),
              margin = list(l = 50, r = 50, t = 50, b = 50),
              showlegend = TRUE,
              hovermode = "closest"
            )

            # Add interactive features
            p <- plotly::config(
              p,
              displayModeBar = TRUE,
              toImageButtonOptions = list(
                format = 'png',
                filename = 'peaks_plot',
                height = 600,
                width = 800,
                scale = 1
              ),
              modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
            )
          }

          return(p)
        }, error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading peaks:", e$message),
              textfont = list(size = 14, color = "red")
            ) %>%
            plotly::layout(
              title = "Error",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        })
      }
    })

    output$peaks_plot <- shiny::renderPlot({
      selected_analyses <- get_selected_analyses_peaks()
      chromatograms_obj <- chromatograms_results()

      if (is.null(selected_analyses) || is.null(chromatograms_obj)) {
        return(NULL)
      }

      tryCatch({
        plot_chromatograms_peaks(
          chromatograms_obj,
          analyses = selected_analyses,
          colorBy = input$peaks_plot_colorby,
          interactive = FALSE,
          title = "Chromatograms Peaks Plot"
        )
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
        text(1, 1, paste("Error loading peaks:", e$message), col = "red")
      })
    })

    # MARK: Export Peaks Data
    # Export Peaks Data -----
    output$peaks_plot_save <- shiny::downloadHandler(
      filename = function() {
        paste0("chromatograms_peaks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        chromatograms_obj <- chromatograms_results()

        if (is.null(chromatograms_obj)) {
          write.csv(
            data.frame(Error = "No chromatograms data available"),
            file, row.names = FALSE
          )
          return()
        }

        tryCatch({
          peaks_data <- get_chromatograms_peaks(chromatograms_obj)
          write.csv(peaks_data, file, row.names = FALSE)
        }, error = function(e) {
          write.csv(
            data.frame(Error = paste("Failed to export peaks data:", e$message)),
            file, row.names = FALSE
          )
        })
      }
    )

    # Reactive value to store current layout proportions
    layout_proportions <- shiny::reactiveVal(c(60, 40)) # Default 60:40

    # Handle proportion button clicks
    shiny::observeEvent(input$prop_20_80, {
      layout_proportions(c(20, 80))
    })
    shiny::observeEvent(input$prop_30_70, {
      layout_proportions(c(30, 70))
    })
    shiny::observeEvent(input$prop_40_60, {
      layout_proportions(c(40, 60))
    })
    shiny::observeEvent(input$prop_50_50, {
      layout_proportions(c(50, 50))
    })
    shiny::observeEvent(input$prop_60_40, {
      layout_proportions(c(60, 40))
    })
    shiny::observeEvent(input$prop_70_30, {
      layout_proportions(c(70, 30))
    })
    shiny::observeEvent(input$prop_80_20, {
      layout_proportions(c(80, 20))
    })

    # Update layout when proportions change
    shiny::observe({
      props <- layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]

      # Create namespace prefix for server context
      ns_prefix <- paste0("WorkflowAssembler-", id)

      # Update the CSS of the panels
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$style(shiny::HTML(paste0(
          "#", ns_prefix, "-chromatograms_table_panel { width: ", table_width, "% !important; }",
          "#", ns_prefix, "-chromatograms_plots_panel { width: ", plots_width, "% !important; }"
        )))
      )

      # Update button active states
      current_prop <- paste0(table_width, "_", plots_width)
      button_id <- paste0("prop_", current_prop)

      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(shiny::HTML(paste0(
          "$('.proportion-controls .btn').removeClass('active');",
          "$('#", ns_prefix, "-", button_id, "').addClass('active');"
        )))
      )
    })

    # Peaks layout proportions
    peaks_layout_proportions <- shiny::reactiveVal(c(60, 40))

    # Handle peaks proportion button clicks
    shiny::observeEvent(input$peaks_prop_20_80, {
      peaks_layout_proportions(c(20, 80))
    })
    shiny::observeEvent(input$peaks_prop_30_70, {
      peaks_layout_proportions(c(30, 70))
    })
    shiny::observeEvent(input$peaks_prop_40_60, {
      peaks_layout_proportions(c(40, 60))
    })
    shiny::observeEvent(input$peaks_prop_50_50, {
      peaks_layout_proportions(c(50, 50))
    })
    shiny::observeEvent(input$peaks_prop_60_40, {
      peaks_layout_proportions(c(60, 40))
    })
    shiny::observeEvent(input$peaks_prop_70_30, {
      peaks_layout_proportions(c(70, 30))
    })
    shiny::observeEvent(input$peaks_prop_80_20, {
      peaks_layout_proportions(c(80, 20))
    })

    # Update peaks layout when proportions change
    shiny::observe({
      props <- peaks_layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]

      # Create namespace prefix for server context
      ns_prefix <- paste0("WorkflowAssembler-", id)

      # Update the CSS of the peaks panels
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$style(shiny::HTML(paste0(
          "#", ns_prefix, "-peaks_table_panel { width: ", table_width, "% !important; }",
          "#", ns_prefix, "-peaks_plots_panel { width: ", plots_width, "% !important; }"
        )))
      )

      # Update button active states
      current_prop <- paste0(table_width, "_", plots_width)
      button_id <- paste0("peaks_prop_", current_prop)

      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(shiny::HTML(paste0(
          "$('.proportion-controls .btn').removeClass('active');",
          "$('#", ns_prefix, "-", button_id, "').addClass('active');"
        )))
      )
    })

    # MARK: JavaScript for UI interactions
    # JavaScript for UI interactions -----
    shiny::observeEvent(
      1,
      {
        ns_prefix <- paste0("WorkflowAssembler-", id)

        shiny::insertUI(
          selector = "head",
          where = "beforeEnd",
          ui = shiny::tags$script(shiny::HTML(paste0(
            "$(document).ready(function(){",
            "  $('[data-toggle=\"tooltip\"]').tooltip();",
            "  $('.btn-group .btn').click(function(){",
            "    $(this).siblings().removeClass('active');",
            "    $(this).addClass('active');",
            "  });",
            "});"
          )))
        )
      },
      once = TRUE
    )
  })
}
