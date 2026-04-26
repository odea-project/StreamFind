#' @export
#' @noRd
.mod_WorkflowAssembler_Result_UI.RamanResults_Spectra <- function(
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
          title = shiny::tagList(shiny::icon("chart-pie", class = "mr-2"), "Overview"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",
            bslib::layout_sidebar(
              # MARK: Sidebar - Status Panel
              sidebar = bslib::sidebar(
                shiny::div(
                  class = "status-panel",
                  # Total Spectra
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("chart-line", class = "mr-2"),
                      "Total Spectra"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::textOutput(ns_full("total_spectra"), inline = TRUE)
                    )
                  ),
                  # Is Averaged
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("chart-bar", class = "mr-2"),
                      "Is Averaged?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("is_averaged_ui"),
                        inline = TRUE
                      )
                    )
                  ),
                  # Is Neutralized
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("clock", class = "mr-2"),
                      "Has Chrom Peaks?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_chrom_peaks_ui"),
                        inline = TRUE
                      )
                    )
                  ),
                  # Has Peaks
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("mountain", class = "mr-2"),
                      "Has Peaks?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_peaks_ui"),
                        inline = TRUE
                      )
                    )
                  ),
                  # Has Charges
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("clock", class = "mr-2"),
                      "Total Chrom Peaks"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::textOutput(ns_full("total_chrom_peaks"), inline = TRUE)
                    )
                  ),
                  # Total Peaks
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("mountain", class = "mr-2"),
                      "Total Peaks"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::textOutput(ns_full("total_peaks"), inline = TRUE)
                    )
                  ),
                  # Total Charges
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("mountain", class = "mr-2"),
                      "Total Peaks"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::textOutput(ns_full("total_peaks"), inline = TRUE)
                    )
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
                  shiny::h4("RamanResults_Spectra Summary", style = "margin: 0; color: #5a5c69;")
                ),
                # Table Container
                shiny::column(
                  width = 12,
                  style = "flex: 1; background-color: white; padding: 5px;",
                  DT::dataTableOutput(
                    ns_full("spectra_summary_table"),
                    height = "100%"
                  )
                )
              )
            )
          )
        ),

        # MARK: Spectra Tab
        ### Spectra Tab ----
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-line", class = "mr-2"), "Spectra"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",

            # MARK: Top Controls Section
            shiny::div(
              class = "spectra-controls-bar",
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
                    id = ns_full("spectra_plot_interactive"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("spectra_plot_interactive"),
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
                    ns_full("spectra_plot_colorby"),
                    label = NULL,
                    choices = c("analyses", "replicates", "analyses+targets", "replicates+targets"),
                    selected = "analyses",
                    width = "150px"
                  )
                )
              ),
              # Right: Export and Layout controls
              shiny::div(
                class = "proportion-controls",
                style = "display: flex; align-items: center; gap: 10px;",
                shiny::downloadButton(
                  ns_full("spectra_plot_save"),
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
                id = ns_full("spectra_table_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: auto;",
                DT::dataTableOutput(ns_full("spectraAnalysesTable"), height = "auto", width = "98%")
              ),

              # Right Side - Spectra Plot (Dynamic width)
              shiny::div(
                id = ns_full("spectra_plots_panel"),
                style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px; overflow: hidden;",
                shiny::div(
                  style = "height: 30px; position: relative;",
                  .app_util_create_maximize_button("spectra_plot", ns_full),
                ),
                shiny::uiOutput(ns_full("spectra_plot_ui"))
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
                    choices = c("analyses", "replicates", "analyses+targets", "replicates+targets"),
                    selected = "analyses",
                    width = "150px"
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
        ),

        # MARK: Chrom Peaks Tab
        ### Chrom Peaks Tab ----
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("clock", class = "mr-2"), "Chrom Peaks"),

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 60px - 45px - 10px); overflow-y: auto; padding: 0;",

            # MARK: Top Controls Section
            shiny::div(
              class = "chrom-peaks-controls-bar",
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
                    id = ns_full("chrom_peaks_plot_interactive"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("chrom_peaks_plot_interactive"),
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
                    ns_full("chrom_peaks_plot_colorby"),
                    label = NULL,
                    choices = c("analyses", "replicates", "analyses+targets", "replicates+targets"),
                    selected = "analyses",
                    width = "150px"
                  )
                )
              ),
              # Right: Export controls
              shiny::div(
                style = "display: flex; align-items: center; gap: 10px;",
                shiny::downloadButton(
                  ns_full("chrom_peaks_plot_save"),
                  "Export Chrom Peaks (.csv)",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm"
                )
              )
            ),

            # MARK: Main Content - Chrom Peaks
            ### Main Content - Chrom Peaks -----
            shiny::div(
              style = "height: calc(100vh - 50px - 30px - 60px - 45px - 80px); padding: 10px;",
              shiny::div(
                style = "height: 30px; position: relative;",
                .app_util_create_maximize_button("chrom_peaks_plot", ns_full),
              ),
              shiny::uiOutput(ns_full("chrom_peaks_plot_ui"))
            )
          )
        )
      )
    )
  )
}

#' @export
#' @noRd
.mod_WorkflowAssembler_Result_Server.RamanResults_Spectra <- function(
  x,
  id,
  ns,
  reactive_analyses,
  reactive_volumes,
  reactive_config
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive value to store the RamanResults_Spectra object
    spectra_results <- shiny::reactiveVal()

    # Initialize with the RamanResults_Spectra object from analyses$results
    shiny::observe({
      analyses <- reactive_analyses()
      if (!is.null(analyses) && !is.null(analyses$results$RamanResults_Spectra)) {
        spectra_obj <- analyses$results$RamanResults_Spectra
        # Validate the spectra object structure
        if (is.list(spectra_obj) && "spectra" %in% names(spectra_obj)) {
          # Ensure required fields exist with default values if missing
          if (is.null(spectra_obj$is_averaged)) spectra_obj$is_averaged <- FALSE
          if (is.null(spectra_obj$peaks)) spectra_obj$peaks <- list()
          if (is.null(spectra_obj$chrom_peaks)) spectra_obj$chrom_peaks <- list()
          spectra_results(spectra_obj)
        } else {
          spectra_results(NULL)
        }
      } else {
        spectra_results(NULL)
      }
    })

    # MARK: Overview Metrics
    # Overview Metrics -----
    output$total_spectra <- shiny::renderText({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj)) return("0")
      as.character(length(spectra_obj$spectra))
    })

    output$is_averaged_ui <- shiny::renderUI({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj) || length(spectra_obj$is_averaged) == 0) {
        value <- FALSE
      } else {
        value <- spectra_obj$is_averaged
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_peaks_ui <- shiny::renderUI({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj)) {
        value <- FALSE
      } else {
        value <- length(spectra_obj$peaks) > 0
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_chrom_peaks_ui <- shiny::renderUI({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj)) {
        value <- FALSE
      } else {
        value <- length(spectra_obj$chrom_peaks) > 0
      }
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$total_peaks <- shiny::renderText({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj) || length(spectra_obj$peaks) == 0) return("0")
      total <- sum(vapply(spectra_obj$peaks, nrow, 0))
      as.character(total)
    })

    output$total_chrom_peaks <- shiny::renderText({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj) || length(spectra_obj$chrom_peaks) == 0) return("0")
      total <- sum(vapply(spectra_obj$chrom_peaks, nrow, 0))
      as.character(total)
    })

    # MARK: Overview Summary Table
    # Overview Summary Table -----
    output$spectra_summary_table <- DT::renderDT({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj)) {
        return(DT::datatable(
          data.frame(Message = "No RamanResults_Spectra object available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      # Create summary table from the spectra object
      analyses_names <- names(spectra_obj$spectra)
      if (length(analyses_names) == 0) {
        return(DT::datatable(
          data.frame(Message = "No spectra data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          rownames = FALSE
        ))
      }

      summary_data <- data.frame(
        Analysis = analyses_names,
        Spectra_Count = vapply(spectra_obj$spectra, nrow, 0),
        Has_Peaks = vapply(analyses_names, function(x) {
          x %in% names(spectra_obj$peaks) && nrow(spectra_obj$peaks[[x]]) > 0
        }, FALSE),
        Peak_Count = vapply(analyses_names, function(x) {
          if (x %in% names(spectra_obj$peaks)) nrow(spectra_obj$peaks[[x]]) else 0
        }, 0),
        Has_Chrom_Peaks = vapply(analyses_names, function(x) {
          x %in% names(spectra_obj$chrom_peaks) && nrow(spectra_obj$chrom_peaks[[x]]) > 0
        }, FALSE),
        Chrom_Peak_Count = vapply(analyses_names, function(x) {
          if (x %in% names(spectra_obj$chrom_peaks)) nrow(spectra_obj$chrom_peaks[[x]]) else 0
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
    output$spectraAnalysesTable <- DT::renderDT({
      analyses <- reactive_analyses()
      spectra_obj <- spectra_results()

      if (is.null(analyses) || is.null(spectra_obj)) {
        return(DT::datatable(
          data.frame(Message = "No analyses or spectra data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      info <- info(analyses)[, c("analysis", "replicate", "blank"), with = FALSE]

      # Filter to analyses that have spectra data
      analyses_with_spectra <- names(spectra_obj$spectra)[
        vapply(spectra_obj$spectra, function(z) nrow(z) > 0, FALSE)
      ]

      if (length(spectra_obj$is_averaged) > 0 && spectra_obj$is_averaged) {
        info <- info[info$replicate %in% analyses_with_spectra, ]
      } else {
        info <- info[info$analysis %in% analyses_with_spectra, ]
      }

      if (nrow(info) == 0) {
        return(DT::datatable(
          data.frame(Message = "No analyses with spectra data found"),
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
    # Helper function to get selected analyses for Spectra tab
    get_selected_analyses_spectra <- shiny::reactive({
      selected_rows <- input$spectraAnalysesTable_rows_selected
      analyses <- reactive_analyses()
      spectra_obj <- spectra_results()

      if (is.null(selected_rows) || length(selected_rows) == 0 || is.null(analyses) || is.null(spectra_obj)) {
        return(NULL)
      }

      info <- info(analyses)[, c("analysis", "replicate", "blank"), with = FALSE]
      analyses_with_spectra <- names(spectra_obj$spectra)[
        vapply(spectra_obj$spectra, function(z) nrow(z) > 0, FALSE)
      ]

      if (length(spectra_obj$is_averaged) > 0 && spectra_obj$is_averaged) {
        info <- info[info$replicate %in% analyses_with_spectra, ]
        return(info$replicate[selected_rows])
      } else {
        info <- info[info$analysis %in% analyses_with_spectra, ]
        return(info$analysis[selected_rows])
      }
    })

    # Helper function to get selected analyses for Peaks tab (uses peaks table selection)
    get_selected_analyses_peaks <- shiny::reactive({
      selected_rows <- input$peaks_table_rows_selected
      spectra_obj <- spectra_results()

      if (is.null(selected_rows) || length(selected_rows) == 0 || is.null(spectra_obj)) {
        return(NULL)
      }

      # Since we can't use get_spectra_peaks (doesn't exist for RamanResults_Spectra),
      # we'll extract from the table directly
      tryCatch({
        # Get available analyses that have peaks
        analyses_with_peaks <- names(spectra_obj$peaks)[
          vapply(spectra_obj$peaks, function(z) nrow(z) > 0, FALSE)
        ]
        
        # Return a subset based on selection (simplified approach)
        if (length(analyses_with_peaks) > 0) {
          max_idx <- min(length(selected_rows), length(analyses_with_peaks))
          return(analyses_with_peaks[1:max_idx])
        }
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    })

    # MARK: Spectra Plot UI
    # Spectra Plot UI -----
    output$spectra_plot_ui <- shiny::renderUI({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj)) {
        htmltools::div(
          style = "margin-top: 20px;",
          htmltools::h4("No RamanResults_Spectra object available!")
        )
      } else if (!is.null(input$spectra_plot_interactive)) {
        if (input$spectra_plot_interactive) {
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              shiny::NS(paste0("WorkflowAssembler-", id))("spectra_plotly"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        } else {
          shinycssloaders::withSpinner(
            shiny::plotOutput(
              shiny::NS(paste0("WorkflowAssembler-", id))("spectra_plot"),
              height = "calc(100vh - 50px - 30px - 60px - 45px - 80px - 45px - 20px - 30px - 50px)"
            ),
            color = "black"
          )
        }
      }
    })

    # MARK: Spectra Plotly
    # Spectra Plotly -----
    output$spectra_plotly <- plotly::renderPlotly({
      selected_analyses <- get_selected_analyses_spectra()
      spectra_obj <- spectra_results()
      analyses <- reactive_analyses()

      if (is.null(selected_analyses) || is.null(spectra_obj) || is.null(analyses)) {
        plotly::plot_ly() %>%
          plotly::add_text(
            x = 0.5,
            y = 0.5,
            text = "Please select analyses from the table to display spectra",
            textfont = list(size = 16, color = "#666")
          ) %>%
          plotly::layout(
            title = "No Selection",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE)
          )
      } else {
        tryCatch({
          p <- plot_spectra(
            analyses,
            analyses = selected_analyses,
            colorBy = input$spectra_plot_colorby,
            interactive = TRUE
          )

          if (!is.null(p)) {
            # Enhance the plotly object
            p <- plotly::layout(
              p,
              width = NULL,
              autosize = TRUE,
              margin = list(l = 50, r = 30, t = 30, b = 50),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)",
              hoverlabel = list(
                bgcolor = "white",
                bordercolor = "#333",
                font = list(size = 12, color = "#333")
              )
            )

            # Add interactive features
            p <- plotly::config(
              p,
              displayModeBar = TRUE,
              modeBarButtonsToRemove = c(
                "sendDataToCloud",
                "autoScale2d",
                "hoverClosestCartesian",
                "hoverCompareCartesian",
                "lasso2d",
                "select2d"
              ),
              displaylogo = FALSE,
              responsive = TRUE
            )
          }

          return(p)
        }, error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading spectra:", e$message),
              textfont = list(size = 14, color = "red")
            ) %>%
            plotly::layout(
              title = "Error Loading Spectra",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        })
      }
    })

    # MARK: Spectra Plot (Static)
    # Spectra Plot (Static) -----
    output$spectra_plot <- shiny::renderPlot({
      selected_analyses <- get_selected_analyses_spectra()
      spectra_obj <- spectra_results()
      analyses <- reactive_analyses()

      if (is.null(selected_analyses) || is.null(spectra_obj) || is.null(analyses)) {
        return(NULL)
      }

      tryCatch({
        plot_spectra(
          analyses,
          analyses = selected_analyses,
          colorBy = input$spectra_plot_colorby,
          interactive = FALSE
        )
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
        text(1, 1, paste("Error loading spectra:", e$message), col = "red")
      })
    })

    # MARK: Export Spectra Data
    # Export Spectra Data -----
    output$spectra_plot_save <- shiny::downloadHandler(
      filename = function() {
        paste0("spectra_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        selected_analyses <- get_selected_analyses_spectra()
        spectra_obj <- spectra_results()
        analyses <- reactive_analyses()

        if (is.null(selected_analyses) || is.null(spectra_obj) || is.null(analyses)) {
          write.csv(
            data.frame(Message = "No analyses selected or spectra object available"),
            file,
            row.names = FALSE
          )
          return()
        }

        tryCatch({
          csv_data <- get_spectra(analyses, analyses = selected_analyses)
          csv_data <- data.table::rbindlist(csv_data, fill = TRUE)
          write.csv(csv_data, file, row.names = FALSE)
        }, error = function(e) {
          write.csv(
            data.frame(Error = paste("Error exporting data:", e$message)),
            file,
            row.names = FALSE
          )
        })
      }
    )

    # MARK: Peaks Table and Plot
    # Peaks Table and Plot -----
    output$peaks_table <- DT::renderDT({
      spectra_obj <- spectra_results()
      if (is.null(spectra_obj) || length(spectra_obj$peaks) == 0) {
        return(DT::datatable(
          data.frame(Message = "No peaks data available"),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      tryCatch({
        # Convert peaks list to data.frame
        peaks_data <- data.table::rbindlist(spectra_obj$peaks, idcol = "analysis", fill = TRUE)
        if (nrow(peaks_data) == 0) {
          return(DT::datatable(
            data.frame(Message = "No peaks found"),
            options = list(dom = "t", ordering = FALSE, paging = FALSE),
            style = "bootstrap",
            rownames = FALSE
          ))
        }

        # Round numeric columns for readability
        numeric_cols <- names(peaks_data)[sapply(peaks_data, is.numeric)]
        for (col in numeric_cols) {
          peaks_data[[col]] <- round(peaks_data[[col]], 4)
        }

        DT::datatable(
          peaks_data,
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
            selection = list(mode = "multiple", selected = NULL, target = "row"),
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
      }, error = function(e) {
        DT::datatable(
          data.frame(Error = paste("Error loading peaks:", e$message)),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          rownames = FALSE
        )
      })
    })

    output$peaks_plot_ui <- shiny::renderUI({
      htmltools::div(
        style = "margin-top: 20px; text-align: center; color: #666;",
        htmltools::h4("Peak plotting not yet implemented for RamanResults_Spectra"),
        htmltools::p("Peak data is available in the table above.")
      )
    })

    output$peaks_plotly <- plotly::renderPlotly({
      selected_analyses <- get_selected_analyses_peaks()
      spectra_obj <- spectra_results()

      if (is.null(selected_analyses) || is.null(spectra_obj) || length(spectra_obj$peaks) == 0) {
        plotly::plot_ly() %>%
          plotly::add_text(
            x = 0.5,
            y = 0.5,
            text = "No peaks data available or no analyses selected",
            textfont = list(size = 16, color = "#666")
          ) %>%
          plotly::layout(
            title = "No Peaks Data",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE)
          )
      } else {
        tryCatch({
          p <- plot_spectra_peaks(
            spectra_obj,
            analyses = selected_analyses,
            colorBy = input$peaks_plot_colorby,
            xVal = input$peaks_xval,
            interactive = TRUE
          )

          if (!is.null(p)) {
            p <- plotly::config(
              p,
              displayModeBar = TRUE,
              modeBarButtonsToRemove = c(
                "sendDataToCloud",
                "autoScale2d",
                "hoverClosestCartesian",
                "hoverCompareCartesian",
                "lasso2d",
                "select2d"
              ),
              displaylogo = FALSE,
              responsive = TRUE
            )
          }

          return(p)
        }, error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading peaks plot:", e$message),
              textfont = list(size = 14, color = "red")
            ) %>%
            plotly::layout(
              title = "Error Loading Peaks",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        })
      }
    })

    # MARK: Chrom Peaks Plot and Data
    # Chrom Peaks Plot and Data -----
    output$chrom_peaks_plot_ui <- shiny::renderUI({
      htmltools::div(
        style = "margin-top: 20px; text-align: center; color: #666;",
        htmltools::h4("Chromatographic peaks data"),
        htmltools::p("Chromatographic peak data is available for export below.")
      )
    })

    # MARK: Export Chrom Peaks Data
    output$chrom_peaks_plot_save <- shiny::downloadHandler(
      filename = function() {
        paste0("chrom_peaks_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        spectra_obj <- spectra_results()
        if (is.null(spectra_obj) || length(spectra_obj$chrom_peaks) == 0) {
          write.csv(
            data.frame(Message = "No chromatographic peaks data available"),
            file,
            row.names = FALSE
          )
          return()
        }

        tryCatch({
          # Convert chrom_peaks list to data.frame
          chrom_peaks_data <- data.table::rbindlist(spectra_obj$chrom_peaks, idcol = "analysis", fill = TRUE)
          write.csv(chrom_peaks_data, file, row.names = FALSE)
        }, error = function(e) {
          write.csv(
            data.frame(Error = paste("Error exporting chromatographic peaks:", e$message)),
            file,
            row.names = FALSE
          )
        })
      }
    )

    # MARK: Export Peaks Data
    output$peaks_plot_save <- shiny::downloadHandler(
      filename = function() {
        paste0("peaks_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        spectra_obj <- spectra_results()
        if (is.null(spectra_obj) || length(spectra_obj$peaks) == 0) {
          write.csv(
            data.frame(Message = "No peaks data available"),
            file,
            row.names = FALSE
          )
          return()
        }

        tryCatch({
          # Convert peaks list to data.frame
          peaks_data <- data.table::rbindlist(spectra_obj$peaks, idcol = "analysis", fill = TRUE)
          write.csv(peaks_data, file, row.names = FALSE)
        }, error = function(e) {
          write.csv(
            data.frame(Error = paste("Error exporting peaks:", e$message)),
            file,
            row.names = FALSE
          )
        })
      }
    )

    # MARK: Layout Proportion Controls
    # Layout Proportion Controls -----

    # Reactive value to store current layout proportions
    layout_proportions <- shiny::reactiveVal(c(60, 40)) # Default 60:40
    peaks_layout_proportions <- shiny::reactiveVal(c(60, 40)) # Default 60:40

    # Handle spectra proportion button clicks
    shiny::observeEvent(input$prop_20_80, { layout_proportions(c(20, 80)) })
    shiny::observeEvent(input$prop_30_70, { layout_proportions(c(30, 70)) })
    shiny::observeEvent(input$prop_40_60, { layout_proportions(c(40, 60)) })
    shiny::observeEvent(input$prop_50_50, { layout_proportions(c(50, 50)) })
    shiny::observeEvent(input$prop_60_40, { layout_proportions(c(60, 40)) })
    shiny::observeEvent(input$prop_70_30, { layout_proportions(c(70, 30)) })
    shiny::observeEvent(input$prop_80_20, { layout_proportions(c(80, 20)) })

    # Handle peaks proportion button clicks
    shiny::observeEvent(input$peaks_prop_20_80, { peaks_layout_proportions(c(20, 80)) })
    shiny::observeEvent(input$peaks_prop_30_70, { peaks_layout_proportions(c(30, 70)) })
    shiny::observeEvent(input$peaks_prop_40_60, { peaks_layout_proportions(c(40, 60)) })
    shiny::observeEvent(input$peaks_prop_50_50, { peaks_layout_proportions(c(50, 50)) })
    shiny::observeEvent(input$peaks_prop_60_40, { peaks_layout_proportions(c(60, 40)) })
    shiny::observeEvent(input$peaks_prop_70_30, { peaks_layout_proportions(c(70, 30)) })
    shiny::observeEvent(input$peaks_prop_80_20, { peaks_layout_proportions(c(80, 20)) })

    # Update layout when proportions change
    shiny::observe({
      props <- layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]
      ns_prefix <- paste0("WorkflowAssembler-", id)

      # Update the CSS of the panels
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$style(shiny::HTML(paste0(
          "#", ns_prefix, "-spectra_table_panel { width: ", table_width, "% !important; }",
          "#", ns_prefix, "-spectra_plots_panel { width: ", plots_width, "% !important; }"
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

    # Update peaks layout when proportions change
    shiny::observe({
      props <- peaks_layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]
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
