#' @export
#' @noRd
.mod_WorkflowAssembler_Result_UI.MassSpecResults_NonTargetAnalysis <- function(
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
    .metric-card {
      border-radius: 8px;
      padding: 20px;
      height: 140px;
      transition: transform 0.2s, box-shadow 0.2s;
      display: flex;
      flex-direction: column;
      justify-content: center;
      position: relative;
      overflow: hidden;
    }
    .metric-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 20px rgba(0,0,0,0.1);
    }
    .metric-icon {
      position: absolute;
      top: 15px;
      left: 15px;
      opacity: 0.8;
    }
    .metric-value {
      font-size: 42px;
      font-weight: 700;
      text-align: center;
      margin-top: 10px;
      line-height: 1;
    }
    .metric-label {
      text-align: center;
      font-size: 16px;
      margin-top: 10px;
      opacity: 0.8;
    }
    .status-panel {
      background-color: #f8f9fa;
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
    .section-title {
      font-size: 18px;
      font-weight: 600;
      margin: 15px 0;
      padding-left: 15px;
      border-left: 4px solid #4e73df;
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
    .card-header {
      background-color: #f8f9fa;
      padding: 15px 20px;
      border-bottom: 1px solid rgba(0,0,0,0.05);
      font-weight: 600;
    }
    .card-body {
      padding: 20px;
    }
    .empty-state {
      text-align: center;
      padding: 40px 20px;
      color: #6c757d;
    }
    .empty-state-icon {
      font-size: 48px;
      margin-bottom: 20px;
      opacity: 0.5;
    }

    .results-wrapper .nav-tabs {
      background-color: #f8f9fa;
      border-radius: 8px 8px 0 0;
      padding: 0 15px;
      margin-bottom: 0;
      border-bottom: 2px solid #e3e6f0;
    }

    .results-wrapper .nav-tabs .nav-link {
      border: none;
      color: #5a5c69;
      padding: 12px 20px;
      font-weight: 500;
      margin-right: 5px;
      border-radius: 0;
    }

    .results-wrapper .nav-tabs .nav-link:hover {
      border-color: transparent;
      background-color: rgba(78, 115, 223, 0.1);
      color: #4e73df;
    }

    .results-wrapper .nav-tabs .nav-link.active {
      color: #4e73df;
      background-color: white;
      border-color: transparent transparent #4e73df transparent;
      border-bottom: 3px solid #4e73df;
      font-weight: 600;
    }

    .results-wrapper .tab-content {
      background: white;
      border-radius: 0 0 8px 8px;
      box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15);
      padding: 0;
      margin-top: -1px;
    }

    .results-wrapper .tab-pane {
      padding: 0;
    }

    .results-wrapper .tab-content .nav-tabs {
      margin-bottom: 0;
      background: transparent;
      border-bottom: 2px solid #e3e6f0;
      padding: 15px 20px 0 20px;
    }

    .results-wrapper .tab-content .tab-content {
      padding: 20px;
    }

    /* Chromatogram icon styling */
    .chromatogram-icon {
      cursor: pointer;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 30px;
      height: 30px;
      border-radius: 4px;
      background-color: #4e73df;
      color: white;
      transition: all 0.2s ease;
      font-size: 14px;
    }

    .chromatogram-icon:hover {
      background-color: #2e59d9;
      transform: scale(1.1);
      box-shadow: 0 2px 8px rgba(78, 115, 223, 0.3);
    }

    .chromatogram-modal .modal-dialog {
      max-width: 90vw;
      width: 90vw;
    }

    .chromatogram-modal .modal-body {
      padding: 20px;
      min-height: 500px;
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

    .features-table-side {
      height: 100%;
      overflow-y: auto;
      border-right: 2px solid #e3e6f0;
      padding-right: 15px;
    }

    .features-plots-side {
      height: 100%;
      overflow-y: auto;
      padding-left: 15px;
    }

    /* Ensure tables fit properly in constrained space */
    .dataTables_wrapper .dataTables_scrollBody {
      max-height: calc(100vh - 400px) !important;
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
      padding: 0 !important;
      margin: 0 !important;
    }
    
    .nav-tabs-custom .tab-content {
      padding: 0 !important;
      margin: 0 !important;
    }

  "
    )
  )

  # MARK: Color Palette Metrics
  # Color Palette Metrics ----
  metric_colors <- list(
    analysis = "#4e73df",
    features = "#1cc88a",
    filtered = "#36b9cc",
    groups = "#f6c23e"
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

    # MARK: Chromatogram Modal
    ## Chromatogram Modal ----
    shiny::div(
      id = ns_full("chromatogram_modal"),
      class = "modal fade chromatogram-modal",
      tabindex = "-1",
      role = "dialog",
      shiny::div(
        class = "modal-dialog modal-lg",
        role = "document",
        shiny::div(
          class = "modal-content",
          shiny::div(
            class = "modal-header",
            shiny::h4(class = "modal-title", "Group Chromatogram"),
            shiny::tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              shiny::span("×")
            )
          ),
          shiny::div(
            class = "modal-body",
            plotly::plotlyOutput(ns_full("chromatogram_plot"), height = "500px")
          )
        )
      )
    ),

    # MARK: Main Content
    ## Main Content ----
    shiny::div(
      class = "tabbox-container",
      style = "margin-top: -20px;",
      shinydashboard::tabBox(
        id = ns_full("main_tabs"),
        width = 12,
        height = "calc(100vh - 50px - 30px - 10px - 60px)",

        # MARK: Overview Tab
        ### Overview Tab ----
        shiny::tabPanel(
          title = shiny::tagList(
            shiny::icon("chart-pie", class = "mr-2"),
            "Overview"
          ),
          height = "100%",

          shiny::div(
            class = "tab-content",
            style = "max-height: calc(100vh - 50px - 30px - 10px - 60px - 45px); overflow-y: auto; padding: 0;",
            # Metrics Row
            shiny::fluidRow(
              # Total Analysis
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0(
                    "background-color: ",
                    metric_colors$analysis,
                    "; color: white;"
                  ),
                  shiny::div(
                    class = "metric-icon",
                    shiny::icon("chart-line", class = "fa-2x")
                  ),
                  shiny::div(
                    class = "metric-value",
                    shiny::textOutput(ns_full("total_analyses"))
                  ),
                  shiny::div(class = "metric-label", "Total Analysis")
                )
              ),
              # Total Features
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0(
                    "background-color: ",
                    metric_colors$features,
                    "; color: white;"
                  ),
                  shiny::div(
                    class = "metric-icon",
                    shiny::icon("gears", class = "fa-2x")
                  ),
                  shiny::div(
                    class = "metric-value",
                    shiny::textOutput(ns_full("total_features"))
                  ),
                  shiny::div(class = "metric-label", "Total Features")
                )
              ),
              # Filtered Features
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0(
                    "background-color: ",
                    metric_colors$filtered,
                    "; color: white;"
                  ),
                  shiny::div(
                    class = "metric-icon",
                    shiny::icon("filter", class = "fa-2x")
                  ),
                  shiny::div(
                    class = "metric-value",
                    shiny::textOutput(ns_full("filtered_features_count"))
                  ),
                  shiny::div(class = "metric-label", "Filtered Features")
                )
              ),
              # Total Groups
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0(
                    "background-color: ",
                    metric_colors$groups,
                    "; color: white;"
                  ),
                  shiny::div(
                    class = "metric-icon",
                    shiny::icon("network-wired", class = "fa-2x")
                  ),
                  shiny::div(
                    class = "metric-value",
                    shiny::textOutput(ns_full("total_groups"))
                  ),
                  shiny::div(class = "metric-label", "Total Groups")
                )
              )
            ),

            # Status Panel and Chart Row
            shiny::fluidRow(
              # Status Panel
              shiny::column(
                width = 4,
                shiny::div(
                  class = "status-panel",
                  shiny::h4("Analysis Status", class = "mb-4"),

                  # Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("check-circle", class = "mr-2"),
                      "Has Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(ns_full("has_features_ui"), inline = TRUE)
                    )
                  ),

                  # Filtered Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("filter", class = "mr-2"),
                      "Has Filtered Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_filtered_features_ui"),
                        inline = TRUE
                      )
                    )
                  ),

                  # Groups Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("object-group", class = "mr-2"),
                      "Has Groups?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(ns_full("has_groups_ui"), inline = TRUE)
                    )
                  ),

                  # EIC Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("wave-square", class = "mr-2"),
                      "Has EIC Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_features_eic_ui"),
                        inline = TRUE
                      )
                    )
                  ),

                  # MS1 Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("chart-bar", class = "mr-2"),
                      "Has MS1 Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_features_ms1_ui"),
                        inline = TRUE
                      )
                    )
                  ),

                  # MS2 Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("chart-area", class = "mr-2"),
                      "Has MS2 Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_features_ms2_ui"),
                        inline = TRUE
                      )
                    )
                  ),

                  # Suspect Features Status
                  shiny::div(
                    class = "status-item",
                    shiny::span(
                      class = "status-label",
                      shiny::icon("search", class = "mr-2"),
                      "Has Suspect Features?"
                    ),
                    shiny::span(
                      class = "status-value",
                      shiny::uiOutput(
                        ns_full("has_features_suspects_ui"),
                        inline = TRUE
                      )
                    )
                  )
                )
              ),

              # Features Chart
              shiny::column(
                width = 8,
                shiny::div(
                  class = "plot-container",
                  shiny::div(
                    class = "card-header d-flex justify-content-between align-items-center",
                    shiny::span(
                      shiny::icon("chart-column", class = "mr-2"),
                      "Features Distribution"
                    ),
                    shiny::div(
                      class = "btn-group btn-group-sm",
                      shiny::tags$button(
                        class = "btn btn-outline-primary active",
                        `data-value` = "replicates",
                        `data-toggle` = "button",
                        onclick = paste0(
                          "Shiny.setInputValue('",
                          ns_full("chart_color_by"),
                          "', 'replicates')"
                        ),
                        "By Replicates"
                      ),
                      shiny::tags$button(
                        class = "btn btn-outline-primary",
                        `data-value` = "analysis",
                        `data-toggle` = "button",
                        onclick = paste0(
                          "Shiny.setInputValue('",
                          ns_full("chart_color_by"),
                          "', 'analysis')"
                        ),
                        "By Analysis"
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-body p-0 position-relative",
                    # maximize button
                    .app_util_create_maximize_button("features_chart", ns_full),
                    plotly::plotlyOutput(
                      ns_full("features_chart"),
                      height = "auto"
                    )
                  )
                )
              )
            )
          )
        ),

        # MARK: Features Tab
        ### Features Tab ----
        # Single Page Layout with Adjustable Proportions
        shiny::tabPanel(
          title = shiny::tagList(
            shiny::icon("table", class = "mr-2"),
            "Features"
          ),

          shiny::div(
            class = "tab-content",
            style = "padding: 0; height: 90vh;",

            # MARK: Top Controls Section
            # Fixed Height
            shiny::div(
              class = "features-controls-bar",
              style = "
                background-color: #f8f9fa; 
                border-bottom: 1px solid #e3e6f0; 
                padding: 10px 15px; 
                height: 60px; 
                display: flex;
                align-items: center; 
                justify-content: space-between;
              ",
              # Left: Button group
              shiny::div(
                class = "btn-group",
                shiny::actionButton(
                  ns_full("deselect_all_features"),
                  "Deselect All",
                  icon = shiny::icon("times-circle"),
                  class = "btn btn-outline-secondary btn-sm"
                ),
                shiny::downloadButton(
                  ns_full("export_features_csv"),
                  "Export to CSV",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm ml-2"
                ),
                shiny::downloadButton(
                  ns_full("export_selected_features_csv"),
                  "Export Selected to CSV",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm ml-2"
                ),
                shiny::actionButton(
                  ns_full("remove_selected_features"),
                  "Remove Selected Features",
                  icon = shiny::icon("trash-alt"),
                  class = "btn btn-outline-danger btn-sm ml-2"
                )
              ),
              # Right: Proportion controls
              shiny::div(
                class = "proportion-controls",
                style = "display: flex; align-items: center; gap: 10px;",
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

            # MARK: Main Content Row
            ### Main Content Row -----
            # Fill remaining height
            shiny::div(
              id = ns_full("main_content_container"),
              style = "display: flex; height: calc(90vh - 60px);",

              # Left Side - Features Table (Dynamic width)
              shiny::div(
                id = ns_full("table_panel"),
                style = "border-right: 2px solid #e3e6f0; padding: 15px; overflow: hidden;",
                shiny::div(
                  class = "features-table-container",
                  style = "height: 100%; overflow: hidden;",
                  DT::dataTableOutput(ns_full("features_table"), height = "100%")
                )
              ),

              # Right Side - Feature Details Tabs (Dynamic width)
              shiny::div(
                id = ns_full("plots_panel"),
                style = "border-right: 2px solid #e3e6f0; padding: 2px; overflow: hidden;",
                shiny::tabsetPanel(
                  id = ns_full("feature_details_tabs"),
                  type = "tabs",

                  # Feature Peaks Tab
                  shiny::tabPanel(
                    title = "EIC",
                    height = "100%",
                    shiny::div(
                      style = "height: 30px; position: relative;",
                      .app_util_create_maximize_button("feature_peaks_plot", ns_full),
                    ),
                    plotly::plotlyOutput(
                      ns_full("feature_peaks_plot"),
                      height = "calc(100% - 30px)"
                    )
                  ),

                  # MS1 Tab
                  shiny::tabPanel(
                    title = "MS1",
                    height = "100%",
                    shiny::div(
                      style = "height: 30px; position: relative;",
                      .app_util_create_maximize_button("feature_peaks_plot", ns_full),
                    ),
                    plotly::plotlyOutput(
                      ns_full("ms1_plot"),
                      height = "calc(100% - 30px)"
                    )
                  ),

                  # MS2 Tab
                  shiny::tabPanel(
                    title = "MS2",
                    height = "100%",
                    shiny::div(
                      style = "height: 30px; position: relative;",
                      .app_util_create_maximize_button("feature_ms2_plot", ns_full),
                    ),
                    plotly::plotlyOutput(
                      ns_full("ms2_plot"),
                      height = "calc(100% - 30px)"
                    )
                  ),

                  # Quality Tab
                  shiny::tabPanel(
                    title = "Quality",
                    shiny::div(
                      class = "p-3",
                      style = "height: calc(100% - 50px); overflow: auto;",
                      DT::dataTableOutput(ns_full("quality_table"))
                    )
                  )
                )
              )
            )
          )
        ),

        # MARK: Groups Tab
        shiny::tabPanel(
          title = shiny::tagList(
            shiny::icon("object-group", class = "mr-2"),
            "Groups"
          ),

          shiny::div(
            class = "tab-content",
            style = "padding: 0; height: 100vh;",
            # Top Controls Section
            shiny::div(
              class = "features-controls-bar",
              style = "background-color: #f8f9fa; border-bottom: 1px solid #e3e6f0; padding: 10px 15px; height: 60px; display: flex; align-items: center; justify-content: space-between;",

              # Left side - Checkboxes only
              shiny::div(
                style = "display: flex; align-items: center; gap: 15px;",
                shiny::div(
                  class = "form-check",
                  style = "display: flex; align-items: center;",
                  shiny::tags$input(
                    type = "checkbox",
                    class = "form-check-input",
                    id = ns_full("show_metadata_groups"),
                    checked = FALSE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("show_metadata_groups"),
                    "Show Metadata",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  )
                ),
                shiny::div(
                  class = "form-check",
                  style = "display: flex; align-items: center;",
                  shiny::tags$input(
                    type = "checkbox",
                    class = "form-check-input",
                    id = ns_full("show_filters_groups"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("show_filters_groups"),
                    "Show Filters",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  )
                )
              ),

              # Right side - Action buttons only
              shiny::div(
                class = "btn-group",
                shiny::actionButton(
                  ns_full("deselect_all_groups"),
                  "Deselect All",
                  icon = shiny::icon("times-circle"),
                  class = "btn btn-outline-secondary btn-sm"
                ),
                shiny::downloadButton(
                  ns_full("export_groups_csv"),
                  "Export to CSV",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm ml-2"
                ),
                shiny::downloadButton(
                  ns_full("export_selected_groups_csv"),
                  "Export Selected to CSV",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm ml-2"
                )
              )
            ),

            # Main Content Row
            shiny::div(
              id = ns_full("groups_main_content_container"),
              style = "display: flex; height: calc(100vh - 200px);",

              # Left Side - Groups Table
              shiny::div(
                id = ns_full("groups_table_panel"),
                style = "border-right: 2px solid #e3e6f0; padding: 15px; overflow: hidden;",
                shiny::div(
                  class = "features-table-container",
                  style = "height: 100%; overflow: hidden;",
                  DT::dataTableOutput(ns_full("groups_table"))
                )
              ),

              # Right Side - Plot Panel
              shiny::div(
                id = ns_full("groups_plots_panel"),
                style = "padding: 15px; overflow: hidden;",
                shiny::div(
                  class = "plot-container p-0",
                  style = "height: 100%;",
                  shiny::tabsetPanel(
                    id = ns_full("group_details_tabs"),
                    type = "tabs",

                    # Group Chromatogram
                    shiny::tabPanel(
                      title = "Group Chromatogram",
                      shiny::div(
                        class = "p-3 position-relative",
                        style = "height: calc(100% - 50px); overflow: auto;",
                        .app_util_create_maximize_button("group_plot", ns_full),
                        plotly::plotlyOutput(
                          ns_full("group_plot"),
                          height = "100%"
                        )
                      )
                    ),
                    # Group Overview Tab
                    shiny::tabPanel(
                      title = "Overview",
                      shiny::div(
                        class = "p-3 position-relative",
                        style = "height: calc(100% - 50px); overflow: auto;",
                        .app_util_create_maximize_button(
                          "group_overview_plot",
                          ns_full
                        ),
                        plotly::plotlyOutput(
                          ns_full("group_overview_plot"),
                          height = "100%"
                        )
                      )
                    ),
                    # MS1 Tab
                    shiny::tabPanel(
                      title = "MS1",
                      shiny::div(
                        class = "p-3 position-relative",
                        style = "height: calc(100% - 50px); overflow: auto;",
                        .app_util_create_maximize_button(
                          "group_ms1_plot",
                          ns_full
                        ),
                        plotly::plotlyOutput(
                          ns_full("group_ms1_plot"),
                          height = "100%"
                        )
                      )
                    ),

                    # MS2 Tab
                    shiny::tabPanel(
                      title = "MS2",
                      shiny::div(
                        class = "p-3 position-relative",
                        style = "height: calc(100% - 50px); overflow: auto;",
                        .app_util_create_maximize_button(
                          "group_ms2_plot",
                          ns_full
                        ),
                        plotly::plotlyOutput(
                          ns_full("group_ms2_plot"),
                          height = "100%"
                        )
                      )
                    ),
                    # Profile Tab
                    shiny::tabPanel(
                      title = "Profile",
                      shiny::div(
                        class = "p-3 position-relative",
                        style = "height: calc(100% - 50px); overflow: auto;",
                        .app_util_create_maximize_button(
                          "group_profile_plot",
                          ns_full
                        ),
                        plotly::plotlyOutput(
                          ns_full("group_profile_plot"),
                          height = "100%"
                        )
                      )
                    )
                  )
                )
              )
            ),

            # Bottom Proportion Controls - EXACTLY like Features
            shiny::div(
              class = "proportion-controls",
              style = "background-color: #f8f9fa; border-top: 1px solid #e3e6f0; padding: 10px 15px; height: 50px; display: flex; align-items: center; justify-content: center;",
              shiny::div(
                style = "display: flex; align-items: center; gap: 10px;",
                shiny::span(
                  "Layout:",
                  style = "font-weight: 500; margin-right: 10px;"
                ),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionButton(
                    ns_full("groups_prop_20_80"),
                    "20:80",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_30_70"),
                    "30:70",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_40_60"),
                    "40:60",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_50_50"),
                    "50:50",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_60_40"),
                    "60:40",
                    class = "btn btn-outline-primary btn-sm active"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_70_30"),
                    "70:30",
                    class = "btn btn-outline-primary btn-sm"
                  ),
                  shiny::actionButton(
                    ns_full("groups_prop_80_20"),
                    "80:20",
                    class = "btn btn-outline-primary btn-sm"
                  )
                )
              )
            )
          )
        ),

        # Internal Standards Tab
        shiny::tabPanel(
          title = shiny::tagList(
            shiny::icon("flask", class = "mr-2"),
            "Internal Standards"
          ),

          shiny::div(
            class = "tab-content",
            style = "padding: 0; height: 100vh;",
            # Top Controls Section
            shiny::div(
              class = "istd-controls-bar",
              style = "background-color: #f8f9fa; border-bottom: 1px solid #e3e6f0; padding: 10px 15px; height: 60px; display: flex; align-items: center; justify-content: space-between;",

              # Left side - View toggle
              shiny::div(
                style = "display: flex; align-items: center; gap: 15px;",
                shiny::div(
                  class = "form-check",
                  style = "display: flex; align-items: center;",
                  shiny::tags$input(
                    type = "checkbox",
                    class = "form-check-input",
                    id = ns_full("show_istd_plot"),
                    checked = TRUE,
                    style = "margin-right: 5px;"
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    `for` = ns_full("show_istd_plot"),
                    "Show Plot View",
                    style = "font-size: 13px; color: #5a5c69; margin: 0;"
                  )
                )
              ),

              # Right side - Export button
              shiny::div(
                class = "btn-group",
                shiny::downloadButton(
                  ns_full("export_istd_csv"),
                  "Export Internal Standards",
                  icon = shiny::icon("file-csv"),
                  class = "btn btn-outline-primary btn-sm"
                )
              )
            ),

            # Main Content
            shiny::div(
              style = "height: calc(100vh - 110px); padding: 20px;",

              # Conditional content based on toggle
              shiny::conditionalPanel(
                condition = paste0("input['", ns_full("show_istd_plot"), "']"),
                shiny::div(
                  class = "plot-container",
                  style = "height: 100%;",
                  shiny::div(
                    class = "card-header d-flex justify-content-between align-items-center",
                    shiny::span(
                      shiny::icon("flask", class = "mr-2"),
                      "Internal Standards Quality Control"
                    ),
                    .app_util_create_maximize_button("istd_plot", ns_full)
                  ),
                  shiny::div(
                    class = "card-body p-3 position-relative",
                    style = "height: calc(100% - 60px);",
                    plotly::plotlyOutput(ns_full("istd_plot"), height = "100%")
                  )
                )
              ),

              # Table view when plot is disabled
              shiny::conditionalPanel(
                condition = paste0("!input['", ns_full("show_istd_plot"), "']"),
                shiny::div(
                  class = "plot-container",
                  style = "height: 100%;",
                  shiny::div(
                    class = "card-header",
                    shiny::span(
                      shiny::icon("table", class = "mr-2"),
                      "Internal Standards Data"
                    )
                  ),
                  shiny::div(
                    class = "card-body p-3",
                    style = "height: calc(100% - 60px); overflow: auto;",
                    DT::dataTableOutput(ns_full("istd_table"))
                  )
                )
              )
            )
          )
        ),
      )
    )
  )
}

# MARK: .mod_WorkflowAssembler_Result_Server
#' @export
#' @noRd
.mod_WorkflowAssembler_Result_Server.MassSpecResults_NonTargetAnalysis <- function(
  x,
  id,
  ns,
  reactive_analyses,
  reactive_volumes,
  reactive_config
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive value to store NTS data
    nts_data <- shiny::reactiveVal()

    # Initialize with input NTS data
    shiny::observe({
      shiny::validate(shiny::need(!is.null(x), "NTS data is not available"))
      nts_data(x)
    })

    # Chart color by input (default: replicates)
    chart_color_by <- shiny::reactiveVal("replicates")

    # Update chart_color_by when buttons are clicked
    shiny::observeEvent(input$chart_color_by, {
      chart_color_by(input$chart_color_by)
    })

    # MARK: Calculate Metrics
    # Calculate Metrics -----
    # Calculate summary metrics for the Overview tab
    output$total_analyses <- shiny::renderText({
      as.character(nrow(nts_data()$info))
    })

    output$total_features <- shiny::renderText({
      features <- vapply(nts_data()$features, nrow, 0)
      total <- sum(features[features > 0])
      as.character(total)
    })

    output$filtered_features_count <- shiny::renderText({
      filtered_features <- get_features_count(nts_data())$filtered
      total <- sum(filtered_features)
      as.character(total)
    })

    output$total_groups <- shiny::renderText({
      groups <- get_features_count(nts_data())$groups
      total <- sum(groups[groups > 0])
      as.character(total)
    })

    # MARK: Status Indicators
    # Status Indicators -----
    # Dynamic status indicators with UI formatting
    output$has_features_ui <- shiny::renderUI({
      value <- any(vapply(nts_data()$features, nrow, 0) > 0)
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_filtered_features_ui <- shiny::renderUI({
      value <- any(get_features_count(nts_data())$filtered > 0)
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_groups_ui <- shiny::renderUI({
      value <- any(get_features_count(nts_data())$groups > 0)
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    output$has_features_eic_ui <- shiny::renderUI({
      value <- any(
        vapply(
          nts_data()$features,
          function(a) {
            sum(vapply(a$eic, nrow, 0))
          },
          0
        ) >
          0
      )
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    # Dynamic MS1 check
    output$has_features_ms1_ui <- shiny::renderUI({
      value <- any(
        vapply(
          nts_data()$features,
          function(a) {
            sum(vapply(a$ms1, nrow, 0))
          },
          0
        ) >
          0
      )
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    # Dynamic MS2 check
    output$has_features_ms2_ui <- shiny::renderUI({
      value <- any(
        vapply(
          nts_data()$features,
          function(a) {
            sum(vapply(a$ms2, nrow, 0))
          },
          0
        ) >
          0
      )
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    # Dynamic Suspects check
    output$has_features_suspects_ui <- shiny::renderUI({
      value <- any(
        vapply(
          nts_data()$features,
          function(a) {
            sum(vapply(a$suspects, nrow, 0))
          },
          0
        ) >
          0
      )
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    })

    # MARK: Features Count Chart
    # Features Count Chart -----
    # Enhanced Features chart using plot_features_count with Plotly
    output$features_chart <- plotly::renderPlotly({
      nts <- nts_data()
      # Get the color by parameter
      color_by <- chart_color_by()
      # Call the base plot function
      p <- plot_features_count(nts, colorBy = color_by, showLegend = FALSE)
      # Enhance the plotly object
      p <- plotly::layout(
        p,
        width = NULL, # Ensure no fixed width
        autosize = TRUE, # Enable dynamic resizing
        margin = list(l = 60, r = 40, t = 40, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(
          title = NULL,
          tickfont = list(size = 12),
          gridcolor = "#eee",
          categoryorder = "total descending" # Ensure bars are ordered for better spacing
        ),
        yaxis = list(
          title = list(
            text = "Number of Features",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),
        # Adjust bargap to stretch bars across the width
        bargap = 0.1,
        # Improved legend
        # legend = list(
        #   orientation = "h",
        #   xanchor = "center",
        #   x = 0.5,
        #   y = -0.15,
        #   bgcolor = "rgba(255,255,255,0.8)",
        #   bordercolor = "rgba(0,0,0,0.1)",
        #   borderwidth = 1
        # ),
        # Hover template
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
      return(p)
    })

    # MARK: Features Table
    # Features Table -----
    output$features_table <- DT::renderDT({
      features <- nts_data()$features
      info <- nts_data()$info
      rpls <- info$replicate
      names(rpls) <- info$analysis
      features <- data.table::rbindlist(features, idcol = "analysis")
      features$replicate <- rpls[features$analysis]
      features$has_eic <- vapply(features$eic, nrow, 0) > 0
      features$eic <- NULL
      features$has_ms1 <- vapply(features$ms1, nrow, 0) > 0
      features$ms1 <- NULL
      features$has_ms2 <- vapply(features$ms2, nrow, 0) > 0
      features$ms2 <- NULL
      features$has_quality <- vapply(features$quality, nrow, 0) > 0
      features$quality <- NULL
      features$has_annotation <- vapply(features$annotation, nrow, 0) > 0
      features$annotation <- NULL
      features$has_istd <- vapply(features$istd, nrow, 0) > 0
      features$istd <- NULL
      features$has_suspects <- vapply(features$suspects, nrow, 0) > 0
      features$suspects <- NULL
      features$has_formulas <- vapply(features$formulas, nrow, 0) > 0
      features$formulas <- NULL
      features$has_compounds <- vapply(features$compounds, nrow, 0) > 0
      features$compounds <- NULL
      for (col in colnames(features)) {
        if (is.numeric(features[[col]][1])) {
          features[[col]] <- round(features[[col]], 4)
        }
      }

      # Add a new column for checkboxes (initially unchecked)
      features$sel <- rep(FALSE, nrow(features))

      data.table::setcolorder(
        features,
        c("sel", "analysis", "feature", "replicate")
      )

      # Determine the index of the 'sel' and 'sel' columns (0-based for JavaScript)
      sel_col_index <- which(names(features) == "sel") - 1

      # Render the DataTable
      DT::datatable(
        features,
        escape = FALSE,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          scrollX = TRUE,
          processing = TRUE,
          scrollY = "100%",
          scrollCollapse = TRUE,
          paging = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(1)),
            list(
              targets = sel_col_index,
              render = DT::JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    var checked = data === true ? 'checked' : '';",
                "    return '<input type=\"checkbox\" ' + checked + ' class=\"sel-checkbox\" data-row=\"' + meta.row + '\" />';",
                "  }",
                "  return data;",
                "}"
              ),
              className = "dt-center"
            )
          ),
          selection = list(mode = "multiple", selected = NULL, target = "row"),
          lengthMenu = c(5, 10, 15, 25, 50, 100),
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

      # DT::datatable(
      #   features,
      #   escape = FALSE,
      #   options = list(
      #     pageLength = 15,
      #     scrollX = TRUE,
      #     processing = TRUE,
      #     scrollY = FALSE,
      #     columnDefs = list(
      #       list(width = "50px", targets = c(1)),
      #       list(width = "200px", targets = c(0)),
      #       list(width = "200px", targets = c(2)),
      #       list(width = "100px", targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11)),
      #       list(width = "80px", targets = c(12)),
      #       list(width = "100px", targets = c(13)),
      #       list(width = "80px", targets = c(14, 15, 16)),
      #       list(width = "150px", targets = c(17)),
      #       list(width = "80px", targets = c(18)),
      #       list(width = "120px", targets = c(19)),
      #       list(
      #         className = "dt-right",
      #         targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17)
      #       ),
      #       list(
      #         className = "dt-left",
      #         targets = c(0, 2, 13, 14, 15, 16, 18, 19)
      #       ),
      #       list(className = "dt-center", targets = c(1)),
      #       list(
      #         targets = sel_col_index,
      #         render = DT::JS(
      #           "function(data, type, row, meta) {",
      #           "  if (type === 'display') {",
      #           "    var checked = data === true ? 'checked' : '';",
      #           "    return '<input type=\"checkbox\" ' + checked + ' class=\"sel-checkbox\" data-row=\"' + meta.row + '\" />';",
      #           "  }",
      #           "  return data;",
      #           "}"
      #         ),
      #         className = "dt-center"
      #       ),
      #       list(
      #         targets = filtered_col_index,
      #         render = DT::JS(
      #           "function(data, type, row, meta) {",
      #           "  if (type === 'display') {",
      #           "    var checked = data === true ? 'checked' : '';",
      #           "    return '<input type=\"checkbox\" ' + checked + ' disabled style=\"pointer-events: none;\" />';",
      #           "  }",
      #           "  return data;",
      #           "}"
      #         ),
      #         className = "dt-center"
      #       )
      #     ),
      #     selection = list(mode = "multiple", selected = NULL, target = "row"),
      #     dom = 'rt<"bottom"lip>',
      #     lengthMenu = c(5, 10, 15, 25, 50, 100),
      #     ordering = TRUE,
      #     searching = FALSE,
      #     searchHighlight = TRUE
      #   ),
      #   style = "bootstrap",
      #   class = "table table-striped table-hover",
      #   rownames = FALSE,
      #   filter = "top",
      #   selection = "multiple"
      # ) %>%
      #   DT::formatStyle(
      #     columns = names(features),
      #     fontSize = "14px",
      #     padding = "8px 12px"
      #   ) %>%
      #   DT::formatStyle(
      #     columns = intersect(numeric_cols, names(features)),
      #     backgroundColor = NULL,
      #     color = NULL
      #   )
    })

    shiny::observeEvent(input$deselect_all_features, {
      DT::selectRows(DT::dataTableProxy("features_table"), NULL)
    })

    # MARK: Export CSV
    # Export CSV -----
    output$export_features_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("features_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        # Get all features data
        features <- get_features(nts_data())

        # Replace list-cols with flags because we flattened them
        features$ms1 <- sapply(features$ms1, function(x) {
          !is.null(x) && length(x) > 0
        })
        features$ms2 <- sapply(features$ms2, function(x) {
          !is.null(x) && length(x) > 0
        })
        features$istd <- sapply(features$istd, function(x) {
          !is.null(x) && length(x) > 0
        })

        # Remove nested columns for CSV export
        nested_cols <- c(
          "eic",
          "quality",
          "annotation",
          "suspects",
          "formulas",
          "compounds"
        )
        export_features <- features[, !nested_cols, with = FALSE]

        # Export to CSV
        write.csv(export_features, file, row.names = FALSE)
      }
    )

    # MARK: Export CSV Selected
    # Export Selected Features CSV -----
    output$export_selected_features_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "selected_features_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        selected_rows <- input$features_table_rows_selected
        features <- get_features(nts_data())

        # Replace list-cols with flags because we flattened them
        features$ms1 <- sapply(features$ms1, function(x) {
          !is.null(x) && length(x) > 0
        })
        features$ms2 <- sapply(features$ms2, function(x) {
          !is.null(x) && length(x) > 0
        })
        features$istd <- sapply(features$istd, function(x) {
          !is.null(x) && length(x) > 0
        })

        # Remove nested columns
        nested_cols <- c(
          "eic",
          "quality",
          "annotation",
          "suspects",
          "formulas",
          "compounds"
        )

        if (!is.null(selected_rows) && length(selected_rows) > 0) {
          selected_features <- features[
            selected_rows,
            !nested_cols,
            with = FALSE
          ]
          write.csv(selected_features, file, row.names = FALSE)
        } else {
          # If no selection, export empty file or a message
          write.csv(
            data.frame(Message = "No features selected"),
            file,
            row.names = FALSE
          )
        }
      }
    )

    # MARK: Reactive Selected Features
    # Reactive Selected Features -----
    selected_features <- shiny::reactive({
      selected_rows <- input$features_table_rows_selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(NULL)
      }
      features <- nts_data()$features
      analyses_vec <- Map(
        function(x, y) rep(y, nrow(x)),
        features,
        names(features)
      )
      analyses_vec <- unlist(analyses_vec, use.names = FALSE)
      features_vec <- unlist(
        lapply(features, function(x) x$feature),
        use.names = FALSE
      )
      return(data.table::data.table(
        "analysis" = analyses_vec[selected_rows],
        "feature" = features_vec[selected_rows]
      ))
    })

    # MARK: Remove Selected Features
    # Remove Selected Features -----
    shiny::observeEvent(input$remove_selected_features, {
      # Get selected features
      selected <- selected_features()

      # Validate selection
      if (is.null(selected) || nrow(selected) == 0) {
        shiny::showNotification(
          "No features selected for removal.",
          type = "warning"
        )
        return()
      }

      # Get current NTS data
      nts <- nts_data()

      # Create a copy to modify
      feature_list <- nts$features

      # Remove selected features
      tryCatch(
        {
          for (i in seq_len(nrow(selected))) {
            analysis_i <- selected$analysis[i]
            feature_i <- selected$feature[i]
            feature_list[[analysis_i]] <- feature_list[[analysis_i]][
              !feature_list[[analysis_i]][["feature"]] %in% feature_i,
            ]
          }

          # Update NTS feature list
          nts$features <- feature_list

          # Update reactive nts_data
          nts_data(nts)

          # Clear table selection
          DT::selectRows(DT::dataTableProxy("features_table"), NULL)

          # Notify user
          shiny::showNotification(
            paste(nrow(selected), "features removed successfully."),
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error removing features:", e$message),
            type = "error"
          )
        }
      )
    })

    # Reactive value to store selected features with mass for MS1/MS2 plots
    selected_features_with_mass <- shiny::reactive({
      selected_rows <- input$features_table_rows_selected

      # Fetch features data (including nested columns)
      features <- get_features(nts_data())
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(NULL)
      }

      # Extract mass for the selected rows
      selected_data <- features[selected_rows, .(mass)]

      # Convert to data frame
      as.data.frame(selected_data)
    })

    # Reactive value to store quality data for selected features
    selected_quality_data <- shiny::reactive({
      selected_rows <- input$features_table_rows_selected

      # Fetch features data (including nested columns)
      features <- get_features(nts_data())
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(NULL)
      }

      # Extract quality data for the selected rows
      quality_list <- features[selected_rows, "quality", with = FALSE]$quality

      # Check if quality_list is empty or NULL
      if (length(quality_list) == 0 || all(sapply(quality_list, is.null))) {
        return(data.frame(message = "No quality data available"))
      }

      # Flatten the quality data
      quality_data <- do.call(
        rbind,
        lapply(seq_along(quality_list), function(i) {
          q <- quality_list[[i]]
          if (is.null(q) || length(q) == 0) {
            return(data.frame(
              feature = features[
                selected_rows[i],
                "feature",
                with = FALSE
              ]$feature,
              message = "No quality data"
            ))
          }
          if (is.list(q) || is.vector(q)) {
            # Convert named vector/list to data frame
            df <- as.data.frame(t(unlist(q)))
            # Add feature column
            df$feature <- features[
              selected_rows[i],
              "feature",
              with = FALSE
            ]$feature
            return(df)
          } else {
            # If it's not a list or vector, return a placeholder
            return(data.frame(
              feature = features[
                selected_rows[i],
                "feature",
                with = FALSE
              ]$feature,
              message = "Invalid quality data"
            ))
          }
        })
      )

      # If the result is empty, return a placeholder
      if (nrow(quality_data) == 0) {
        return(data.frame(message = "No quality data available"))
      }

      # Identify numeric columns to round
      numeric_cols <- c("sn", "gauss_a", "gauss_u", "gauss_s", "gauss_f")
      for (col in numeric_cols) {
        if (col %in% names(quality_data)) {
          quality_data[[col]] <- round(as.numeric(quality_data[[col]]), 3) # Round to 3 decimal places
        }
      }

      # Reorder columns to put feature first
      quality_data <- quality_data[, c(
        "feature",
        setdiff(names(quality_data), "feature")
      )]

      return(quality_data)
    })

    # MARK: Features EIC Plot
    # Features EIC Plot -----
    output$feature_peaks_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_features()),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Generate the plot using plot_features
      nts <- nts_data()
      p <- plot_features(nts, features = selected_features(), filtered = TRUE)

      shiny::validate(
        need(
          !is.null(p),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
      p <- plotly::layout(
        p,
        width = NULL, # Ensure no fixed width
        autosize = TRUE, # Enable dynamic resizing
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",

        # Axis labels
        xaxis = list(
          title = list(
            text = "Retention Time (RT)",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        yaxis = list(
          title = list(
            text = "Intensity",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        # Legend
        legend = list(
          orientation = "v",
          xanchor = "right",
          yanchor = "top",
          x = 0.98,
          y = 0.98,
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "rgba(0,0,0,0.1)",
          borderwidth = 1,
          font = list(size = 12)
        ),

        # Hover template
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

      return(p)
    })

    # MARK: Features MS1 Plot
    # Features MS1 Plot -----
    output$ms1_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_features_with_mass()),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Generate the MS1 plot
      nts <- nts_data()
      p <- plot_features_ms1(nts, features = selected_features(), filtered = TRUE)

      shiny::validate(
        need(
          !is.null(p),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
      p <- plotly::layout(
        p,
        width = NULL, # Ensure no fixed width
        autosize = TRUE, # Enable dynamic resizing
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",

        # Axis labels
        xaxis = list(
          title = list(
            text = "m/z",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        yaxis = list(
          title = list(
            text = "Intensity",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        # Legend
        legend = list(
          orientation = "v",
          xanchor = "right",
          yanchor = "top",
          x = 0.98,
          y = 0.98,
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "rgba(0,0,0,0.1)",
          borderwidth = 1,
          font = list(size = 12)
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

      return(p)
    })

    # MARK: Features MS2 Plot
    # Features MS2 Plot -----
    output$ms2_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_features_with_mass()),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Generate the MS2 plot
      nts <- nts_data()
      p <- plot_features_ms2(nts, features = selected_features(), filtered = TRUE)

      shiny::validate(
        need(
          !is.null(p),
          "Please select one or more features from the table to display the plot."
        )
      )

      # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
      p <- plotly::layout(
        p,
        width = NULL, # Ensure no fixed width
        autosize = TRUE, # Enable dynamic resizing
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",

        # Axis labels
        xaxis = list(
          title = list(
            text = "m/z",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        yaxis = list(
          title = list(
            text = "Intensity / Counts",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),

        # Legend
        legend = list(
          orientation = "v",
          xanchor = "right",
          yanchor = "top",
          x = 0.98,
          y = 0.98,
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "rgba(0,0,0,0.1)",
          borderwidth = 1,
          font = list(size = 12)
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

      return(p)
    })

    # MARK: Features Quality Table
    # Features Quality Table -----
    output$quality_table <- DT::renderDT({
      shiny::validate(
        need(
          !is.null(selected_quality_data()),
          "Please select one or more features from the table to display the quality data."
        )
      )

      # Fetch quality data
      quality_data <- selected_quality_data()

      # Check if the data is a placeholder
      if ("message" %in% names(quality_data) && ncol(quality_data) == 1) {
        return(DT::datatable(
          quality_data,
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE
          ),
          style = "bootstrap",
          class = "table table-bordered table-hover",
          rownames = FALSE
        ))
      }

      # Render the DataTable
      DT::datatable(
        quality_data,
        options = list(
          pageLength = 5,
          processing = TRUE,
          scrollX = TRUE,
          dom = "lfrtp",
          ordering = TRUE,
          lengthMenu = c(5, 10, 25),
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        style = "bootstrap",
        class = "table table-striped table-hover",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = names(quality_data),
          fontSize = "14px",
          padding = "8px 12px"
        )
    })

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
          "
        #",
          ns_prefix,
          "-table_panel { width: ",
          table_width,
          "% !important; }
        #",
          ns_prefix,
          "-plots_panel { width: ",
          plots_width,
          "% !important; }
      "
        )))
      )

      # Update button active states with JavaScript
      current_prop <- paste0(table_width, "_", plots_width)
      button_id <- paste0("prop_", current_prop)

      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(shiny::HTML(paste0(
          "
        $('.proportion-controls .btn').removeClass('active');
        $('#",
          ns_prefix,
          "-",
          button_id,
          "').addClass('active');
      "
        )))
      )
    })

    # MARK: Groups Tab
    # Groups Tab -----

    # Store filter visibility state
    show_filters_groups <- shiny::reactive({
      if (is.null(input$show_filters_groups)) {
        TRUE
      } else {
        input$show_filters_groups
      }
    })

    # Groups table for the Groups tab
    output$groups_table <- DT::renderDT({
      # Get checkbox values
      show_metadata <- if (is.null(input$show_metadata_groups)) {
        FALSE
      } else {
        input$show_metadata_groups
      }
      show_filters <- if (is.null(input$show_filters_groups)) {
        TRUE
      } else {
        input$show_filters_groups
      }

      # Fetch groups data with enhanced parameters
      groups <- get_groups(
        nts_data(),
        metadata = show_metadata,
        intensities = TRUE # Always show intensities
      )

      # Check if groups data is available
      if (is.null(groups) || nrow(groups) == 0) {
        return(DT::datatable(
          data.frame(Message = "No groups data available"),
          options = list(
            dom = "t",
            ordering = FALSE,
            processing = TRUE,
            paging = FALSE
          ),
          style = "bootstrap",
          class = "table table-bordered",
          rownames = FALSE
        ))
      }

      # Round numeric columns to 4 decimal places for readability
      numeric_cols <- names(groups)[sapply(groups, is.numeric)]
      for (col in numeric_cols) {
        groups[[col]] <- round(groups[[col]], 4)
      }

      # DataTable
      DT::datatable(
        groups,
        escape = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          processing = TRUE,
          scrollY = FALSE,
          columnDefs = list(
            list(width = "150px", targets = 0),
            list(className = "dt-center", targets = "_all")
          ),
          selection = list(mode = "multiple", selected = NULL, target = "row"),
          dom = 'rt<"bottom"lip>',
          lengthMenu = c(5, 10, 25, 50, 100),
          ordering = TRUE,
          searching = FALSE,
          searchHighlight = TRUE
        ),
        style = "bootstrap",
        class = "table table-striped table-hover",
        rownames = FALSE,
        filter = if (show_filters) "top" else "none",
        selection = "multiple"
      ) %>%
        DT::formatStyle(
          columns = names(groups),
          fontSize = "14px",
          padding = "8px 12px"
        )
    })

    # Reactive value to store selected groups
    selected_groups <- shiny::reactive({
      selected_rows <- input$groups_table_rows_selected

      # Fetch groups data
      groups <- get_groups(nts_data(), intensities = TRUE)
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(NULL)
      }

      # Extract group IDs for the selected rows
      selected_data <- groups[selected_rows, .(group)]

      # Convert to data frame
      as.data.frame(selected_data)
    })

    # Group action handlers
    shiny::observeEvent(input$deselect_all_groups, {
      DT::selectRows(DT::dataTableProxy("groups_table"), NULL)
    })

    # Export handlers for groups
    output$export_groups_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("groups_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        groups <- get_groups(nts_data(), metadata = TRUE, intensities = TRUE)
        write.csv(groups, file, row.names = FALSE)
      }
    )

    output$export_selected_groups_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("selected_groups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        selected_rows <- input$groups_table_rows_selected
        groups <- get_groups(nts_data(), metadata = TRUE, intensities = TRUE)

        if (!is.null(selected_rows) && length(selected_rows) > 0) {
          selected_groups_data <- groups[selected_rows, ]
          write.csv(selected_groups_data, file, row.names = FALSE)
        } else {
          write.csv(
            data.frame(Message = "No groups selected"),
            file,
            row.names = FALSE
          )
        }
      }
    )

    # plot_groups() function
    output$group_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_groups()),
          "Please select one or more groups from the table to display the plot."
        )
      )

      # Generate the plot
      nts <- nts_data()
      selected_group_data <- selected_groups()

      tryCatch(
        {
          p <- plot_groups(nts, groups = selected_group_data)

          # Enhance the plotly object
          p <- plotly::layout(
            p,
            width = NULL,
            autosize = TRUE,
            margin = list(l = 50, r = 30, t = 30, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",

            xaxis = list(
              title = list(
                text = "Retention Time (RT)",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            yaxis = list(
              title = list(
                text = "Intensity",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            legend = list(
              orientation = "v",
              xanchor = "right",
              yanchor = "top",
              x = 0.98,
              y = 0.98,
              bgcolor = "rgba(255,255,255,0.8)",
              bordercolor = "rgba(0,0,0,0.1)",
              borderwidth = 1,
              font = list(size = 12)
            )
          )

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

          return(p)
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading plot:", e$message),
              textfont = list(size = 16, color = "red")
            ) %>%
            plotly::layout(
              title = "Error loading plot",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        }
      )
    })

    # Group Overview Plot
    output$group_overview_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_groups()),
          "Please select one or more groups from the table to display the overview."
        )
      )

      nts <- nts_data()
      selected_group_data <- selected_groups()

      tryCatch(
        {
          p <- plot_groups_overview(nts, groups = selected_group_data)

          p <- plotly::layout(
            p,
            width = NULL,
            autosize = TRUE,
            margin = list(l = 50, r = 30, t = 30, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          )

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

          return(p)
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading overview:", e$message),
              textfont = list(size = 16, color = "red")
            ) %>%
            plotly::layout(
              title = "Error loading overview",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        }
      )
    })

    # Group MS1 Plot
    output$group_ms1_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_groups()),
          "Please select one or more groups from the table to display the MS1 plot."
        )
      )

      nts <- nts_data()
      selected_group_data <- selected_groups()

      tryCatch(
        {
          # Try with more permissive parameters
          p <- plot_groups_ms1(
            nts,
            groups = selected_group_data,
            useLoadedData = TRUE, # Try loaded data first
            mzClustFeatures = 0.01, # More permissive clustering
            presenceFeatures = 0.5, # Lower presence requirement
            minIntensityFeatures = 100, # Lower intensity threshold
            mzClust = 0.01,
            presence = 0.5,
            minIntensity = 100,
            top = 50, # More peaks
            normalized = TRUE,
            groupBy = "groups"
          )

          # Check if plot is NULL
          if (is.null(p)) {
            stop("No MS1 data available for selected groups")
          }

          p <- plotly::layout(
            p,
            width = NULL,
            autosize = TRUE,
            margin = list(l = 50, r = 30, t = 30, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",

            xaxis = list(
              title = list(
                text = "m/z",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            yaxis = list(
              title = list(
                text = "Intensity",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            )
          )

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

          return(p)
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste(
                "No MS1 data available for selected groups\n",
                "Try selecting different groups or check data processing"
              ),
              textfont = list(size = 14, color = "#666")
            ) %>%
            plotly::layout(
              title = "MS1 Data Not Available",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE),
              margin = list(l = 50, r = 50, t = 50, b = 50)
            )
        }
      )
    })

    # Group MS2 Plot
    output$group_ms2_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_groups()),
          "Please select one or more groups from the table to display the MS2 plot."
        )
      )

      nts <- nts_data()
      selected_group_data <- selected_groups()

      tryCatch(
        {
          # Try with more permissive parameters
          p <- plot_groups_ms2(
            nts,
            groups = selected_group_data,
            useLoadedData = TRUE, # Try loaded data first
            isolationWindow = 2.0, # Wider isolation window
            mzClustFeatures = 0.01, # More permissive clustering
            presenceFeatures = 0.5, # Lower presence requirement
            minIntensityFeatures = 50, # Lower intensity threshold
            mzClust = 0.01,
            presence = 0.5,
            minIntensity = 50,
            top = 50, # More peaks
            normalized = TRUE,
            groupBy = "groups"
          )

          # Check if plot is NULL
          if (is.null(p)) {
            stop("No MS2 data available for selected groups")
          }

          p <- plotly::layout(
            p,
            width = NULL,
            autosize = TRUE,
            margin = list(l = 50, r = 30, t = 30, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",

            xaxis = list(
              title = list(
                text = "m/z",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            yaxis = list(
              title = list(
                text = "Intensity",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            )
          )

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

          return(p)
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste(
                "No MS2 data available for selected groups\n",
                "Try selecting different groups or check data processing"
              ),
              textfont = list(size = 14, color = "#666")
            ) %>%
            plotly::layout(
              title = "MS2 Data Not Available",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE),
              margin = list(l = 50, r = 50, t = 50, b = 50)
            )
        }
      )
    })

    # Group Profile Plot
    output$group_profile_plot <- plotly::renderPlotly({
      shiny::validate(
        need(
          !is.null(selected_groups()),
          "Please select one or more groups from the table to display the profile."
        )
      )

      nts <- nts_data()
      selected_group_data <- selected_groups()

      tryCatch(
        {
          p <- plot_groups_profile(nts, groups = selected_group_data)

          p <- plotly::layout(
            p,
            width = NULL,
            autosize = TRUE,
            margin = list(l = 50, r = 30, t = 30, b = 50),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",

            xaxis = list(
              title = list(
                text = "Analysis",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            yaxis = list(
              title = list(
                text = "Normalized Intensity",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),

            legend = list(
              orientation = "v",
              xanchor = "right",
              yanchor = "top",
              x = 0.98,
              y = 0.98,
              bgcolor = "rgba(255,255,255,0.8)",
              bordercolor = "rgba(0,0,0,0.1)",
              borderwidth = 1,
              font = list(size = 12)
            )
          )

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

          return(p)
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading profile:", e$message),
              textfont = list(size = 16, color = "red")
            ) %>%
            plotly::layout(
              title = "Error loading profile",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        }
      )
    })

    # Groups layout proportion controls
    groups_layout_proportions <- shiny::reactiveVal(c(60, 40))

    # Handle groups proportion button clicks
    shiny::observeEvent(input$groups_prop_20_80, {
      groups_layout_proportions(c(20, 80))
    })
    shiny::observeEvent(input$groups_prop_30_70, {
      groups_layout_proportions(c(30, 70))
    })
    shiny::observeEvent(input$groups_prop_40_60, {
      groups_layout_proportions(c(40, 60))
    })
    shiny::observeEvent(input$groups_prop_50_50, {
      groups_layout_proportions(c(50, 50))
    })
    shiny::observeEvent(input$groups_prop_60_40, {
      groups_layout_proportions(c(60, 40))
    })
    shiny::observeEvent(input$groups_prop_70_30, {
      groups_layout_proportions(c(70, 30))
    })
    shiny::observeEvent(input$groups_prop_80_20, {
      groups_layout_proportions(c(80, 20))
    })

    # Update groups layout when proportions change
    shiny::observe({
      props <- groups_layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]

      ns_prefix <- paste0("WorkflowAssembler-", id)

      # Update the CSS of the groups panels
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$style(shiny::HTML(paste0(
          "
        #",
          ns_prefix,
          "-groups_table_panel { width: ",
          table_width,
          "% !important; }
        #",
          ns_prefix,
          "-groups_plots_panel { width: ",
          plots_width,
          "% !important; }
      "
        )))
      )

      # Update button active states
      current_prop <- paste0(table_width, "_", plots_width)
      button_id <- paste0("groups_prop_", current_prop)

      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(shiny::HTML(paste0(
          "
        $('.proportion-controls .btn').removeClass('active');
        $('#",
          ns_prefix,
          "-",
          button_id,
          "').addClass('active');
      "
        )))
      )
    })

    # Internal Standards Plot
    output$istd_plot <- plotly::renderPlotly({
      nts <- nts_data()

      tryCatch(
        {
          # Check if internal standards data exists
          istd_data <- get_internal_standards(nts, average = TRUE)

          if (nrow(istd_data) == 0) {
            # Create empty plot with message
            plotly::plot_ly() %>%
              plotly::add_text(
                x = 0.5,
                y = 0.5,
                text = "No internal standards found\nRun FindInternalStandards method first",
                textfont = list(size = 16, color = "#666")
              ) %>%
              plotly::layout(
                title = "Internal Standards Not Available",
                xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, showticklabels = FALSE),
                margin = list(l = 50, r = 50, t = 50, b = 50)
              )
          } else {
            # Generate the plot
            p <- plot_internal_standards(nts)

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
          }
        },
        error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5,
              y = 0.5,
              text = paste("Error loading internal standards:", e$message),
              textfont = list(size = 14, color = "red")
            ) %>%
            plotly::layout(
              title = "Error Loading Internal Standards",
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE)
            )
        }
      )
    })

    # Internal Standards Table
    output$istd_table <- DT::renderDT({
      nts <- nts_data()

      tryCatch(
        {
          istd_data <- get_internal_standards(nts, average = TRUE)

          if (nrow(istd_data) == 0) {
            return(DT::datatable(
              data.frame(
                Message = "No internal standards found. Run FindInternalStandards method first."
              ),
              options = list(dom = "t", ordering = FALSE, paging = FALSE),
              style = "bootstrap",
              class = "table table-bordered",
              rownames = FALSE
            ))
          }

          # Round numeric columns
          numeric_cols <- names(istd_data)[sapply(istd_data, is.numeric)]
          for (col in numeric_cols) {
            istd_data[[col]] <- round(istd_data[[col]], 4)
          }

          DT::datatable(
            istd_data,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              processing = TRUE,
              dom = 'rt<"bottom"lip>',
              lengthMenu = c(5, 10, 25, 50),
              ordering = TRUE,
              searching = TRUE
            ),
            style = "bootstrap",
            class = "table table-striped table-hover",
            rownames = FALSE,
            filter = "top"
          ) %>%
            DT::formatStyle(
              columns = names(istd_data),
              fontSize = "14px",
              padding = "8px 12px"
            )
        },
        error = function(e) {
          DT::datatable(
            data.frame(Error = paste("Error loading data:", e$message)),
            options = list(dom = "t", ordering = FALSE, paging = FALSE),
            style = "bootstrap",
            rownames = FALSE
          )
        }
      )
    })

    # Export handler
    output$export_istd_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "internal_standards_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        istd_data <- get_internal_standards(nts_data(), average = TRUE)
        write.csv(istd_data, file, row.names = FALSE)
      }
    )

    # JavaScript for UI interactions
    shiny::observeEvent(
      1,
      {
        # Get the full namespace for use in JavaScript
        ns_prefix <- paste0("WorkflowAssembler-", id)

        # Initialize tooltips and chromatogram functionality
        shiny::insertUI(
          selector = "head",
          where = "beforeEnd",
          ui = shiny::tags$script(shiny::HTML(paste0(
            "
        // Global function to show chromatogram
        function showChromatogram(groupId) {
          Shiny.setInputValue('",
            ns_prefix,
            "-show_chromatogram', {
            group: groupId,
            timestamp: Date.now()
          });
          $('#",
            ns_prefix,
            "-chromatogram_modal').modal('show');
        }
        
        $(document).ready(function(){
          // Initialize tooltips
          $('[data-toggle=\"tooltip\"]').tooltip();
          
          // Make status panel items clickable
          $('.status-item').css('cursor', 'pointer').click(function(){
            $('.status-item').removeClass('active');
            $(this).addClass('active');
          });
          
          // Enhance button group behavior
          $('.btn-group .btn').click(function(){
            $(this).siblings().removeClass('active');
            $(this).addClass('active');
          });
        });
      "
          )))
        )
      },
      once = TRUE
    )
  })
}
