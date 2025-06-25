# MARK: method
#' @noRd
S7::method(.mod_WorkflowAssembler_Result_UI, NonTargetAnalysisResults) <- function(x, id, ns) {
  ns_full <- shiny::NS(paste0("WorkflowAssembler-", id))
  
  # Custom CSS for consistent styling
  custom_css <- shiny::tags$style(shiny::HTML("
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
    .dataTables_wrapper .dataTables_length, 
    .dataTables_wrapper .dataTables_filter {
      margin-bottom: 15px;
    }
    .dataTables_wrapper .dataTables_info, 
    .dataTables_wrapper .dataTables_paginate {
      margin-top: 15px;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background: #4e73df !important;
      color: white !important;
      border: none !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: #a2aecf !important;
      color: white !important;
      border: none !important;
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
    .table-wrapper {
      position: relative;
    }

    .table-controls-wrapper {
      display: flex;
      justify-content: flex-end;
      align-items: center;
      margin-bottom: 10px;
    }

    .features-table-container .dataTables_wrapper .dataTables_filter {
      float: right;
      margin-bottom: 10px;
    }

    .features-table-container .dataTables_wrapper .dataTables_filter input {
      border-radius: 4px;
      border: 1px solid #ced4da;
      padding: 5px 10px;
      font-size: 14px;
      width: 200px;
    }

    .table-controls-wrapper .btn-group {
      display: flex;
      align-items: center;
      margin-right: 15px;
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

    /* Fix for Groups table filter scrolling */
    .dataTables_wrapper .dataTables_scrollHead {
      overflow: visible !important;
      position: relative;
      z-index: 100;
    }

    .dataTables_wrapper .dataTables_scrollHeadInner {
      overflow: visible !important;
      position: relative;
      z-index: 100;
    }

    .dataTables_wrapper thead tr:last-child th {
      position: relative;
      background: white;
      z-index: 50;
    }

    .dataTables_wrapper thead tr:last-child th input,
    .dataTables_wrapper thead tr:last-child th select {
      position: relative !important;
      z-index: 60 !important;
      background: white !important;
      opacity: 1 !important;
      pointer-events: auto !important;
      border: 1px solid #ced4da !important;
    }

    .dataTables_wrapper .dataTables_scrollBody {
      position: relative;
      z-index: 10;
    }

    .dataTables_wrapper .dataTables_scrollHead table {
      position: relative;
      z-index: 55;
    }

    .dataTables_wrapper .dataTables_scrollHead .dataTables_filter {
      position: relative !important;
      z-index: 70 !important;
    }

    /* Synchronized header scrolling */
    .dataTables_wrapper .dataTables_scrollHead {
      overflow-x: hidden !important;
    }

    .dataTables_wrapper .dataTables_scrollBody {
      overflow-x: auto !important;
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

    /* Ensure proper header alignment */
    .dataTables_wrapper .dataTables_scrollHeadInner table {
      width: 100% !important;
    }

  "))
  
  # Color palette for metrics
  metric_colors <- list(
    analysis = "#4e73df",
    features = "#1cc88a", 
    filtered = "#36b9cc",
    groups = "#f6c23e"
  )
  
  # plot maximize functions
  source_functions <- list(
    .app_util_create_maximize_button = .app_util_create_maximize_button,
    .app_util_create_plot_modal = .app_util_create_plot_modal,
    .app_util_plot_maximize_js = .app_util_plot_maximize_js
  )
  
  shiny::tagList(
    custom_css,
    .app_util_plot_maximize_js(),  # JavaScript functions
    .app_util_create_plot_modal(ns_full),  # Modal container

    # Add chromatogram modal
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
    
    shiny::div(style = "margin-top: -20px;",
      shinydashboard::tabBox(
        id = ns_full("main_tabs"),
        width = 12,
        height = "auto",
        
        # Overview Tab
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-pie", class = "mr-2"), "Overview"),
          
          shiny::div(class = "tab-content",
            # Metrics Row
            shiny::fluidRow(
              # Total Analysis
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0("background-color: ", metric_colors$analysis, "; color: white;"),
                  shiny::div(class = "metric-icon", shiny::icon("chart-line", class = "fa-2x")),
                  shiny::div(class = "metric-value", shiny::textOutput(ns_full("total_analyses"))),
                  shiny::div(class = "metric-label", "Total Analysis")
                )
              ),
              # Total Features
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0("background-color: ", metric_colors$features, "; color: white;"),
                  shiny::div(class = "metric-icon", shiny::icon("gears", class = "fa-2x")),
                  shiny::div(class = "metric-value", shiny::textOutput(ns_full("total_features"))),
                  shiny::div(class = "metric-label", "Total Features")
                )
              ),
              # Filtered Features
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0("background-color: ", metric_colors$filtered, "; color: white;"),
                  shiny::div(class = "metric-icon", shiny::icon("filter", class = "fa-2x")),
                  shiny::div(class = "metric-value", shiny::textOutput(ns_full("filtered_features_count"))),
                  shiny::div(class = "metric-label", "Filtered Features")
                )
              ),
              # Total Groups
              shiny::column(
                width = 3,
                shiny::div(
                  class = "metric-card",
                  style = paste0("background-color: ", metric_colors$groups, "; color: white;"),
                  shiny::div(class = "metric-icon", shiny::icon("network-wired", class = "fa-2x")),
                  shiny::div(class = "metric-value", shiny::textOutput(ns_full("total_groups"))),
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
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("check-circle", class = "mr-2"), "Has Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_features_ui"), inline = TRUE)
                    )
                  ),
                  
                  # Filtered Features Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("filter", class = "mr-2"), "Has Filtered Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_filtered_features_ui"), inline = TRUE)
                    )
                  ),
                  
                  # Groups Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("object-group", class = "mr-2"), "Has Groups?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_groups_ui"), inline = TRUE)
                    )
                  ),
                  
                  # EIC Features Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("wave-square", class = "mr-2"), "Has EIC Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_features_eic_ui"), inline = TRUE)
                    )
                  ),
                  
                  # MS1 Features Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("chart-bar", class = "mr-2"), "Has MS1 Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_features_ms1_ui"), inline = TRUE)
                    )
                  ),
                  
                  # MS2 Features Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("chart-area", class = "mr-2"), "Has MS2 Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_features_ms2_ui"), inline = TRUE)
                    )
                  ),
                  
                  # Suspect Features Status
                  shiny::div(class = "status-item",
                    shiny::span(class = "status-label", shiny::icon("search", class = "mr-2"), "Has Suspect Features?"),
                    shiny::span(class = "status-value", 
                      shiny::uiOutput(ns_full("has_features_suspects_ui"), inline = TRUE)
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
                    shiny::span(shiny::icon("chart-column", class = "mr-2"), "Features Distribution"),
                    shiny::div(
                      class = "btn-group btn-group-sm",
                      shiny::tags$button(
                        class = "btn btn-outline-primary active",
                        `data-value` = "replicates",
                        `data-toggle` = "button",
                        onclick = paste0("Shiny.setInputValue('", ns_full("chart_color_by"), "', 'replicates')"),
                        "By Replicates"
                      ),
                      shiny::tags$button(
                        class = "btn btn-outline-primary",
                        `data-value` = "analysis",
                        `data-toggle` = "button",
                        onclick = paste0("Shiny.setInputValue('", ns_full("chart_color_by"), "', 'analysis')"),
                        "By Analysis"
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-body p-0 position-relative",
                    # maximize button
                    .app_util_create_maximize_button("features_chart", ns_full),
                    plotly::plotlyOutput(ns_full("features_chart"), height = "auto")
                  )
                )
              )
            )
          )
        ),
        
        # Features Tab
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("table", class = "mr-2"), "Features"),
          
          shiny::div(class = "tab-content",
            # Features Table
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::div(
                  class = "mb-4",
                  # Custom wrapper for table
                  shiny::div(
                    class = "table-wrapper",
                    # DataTable output with custom class for styling
                    shiny::div(
                      class = "features-table-container",
                      DT::dataTableOutput(ns_full("features_table"))
                    )
                  )
                )
              )
            ),

            #  Buttons
            shiny::fluidRow(
              shiny::column(
                width = 12,
                  shiny::div(
                    class = "card-body d-flex justify-content-center align-items-center",
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
                    )
                )
              )
            ),
            
            # Feature Details Row
            shiny::fluidRow(
              # Feature Peaks Plot
              shiny::column(
                width = 6,
                shiny::div(
                  class = "plot-container",
                  shiny::div(
                    class = "card-header",
                    shiny::icon("wave-square", class = "mr-2"), "Feature Peaks"
                  ),
                  shiny::div(
                    class = "card-body p-0 position-relative",
                    .app_util_create_maximize_button("feature_peaks_plot", ns_full),
                    plotly::plotlyOutput(ns_full("feature_peaks_plot"), height = "auto")
                  )
                )
              ),
              
              # Feature Details Tabs
              shiny::column(
                width = 6,
                shiny::div(
                  class = "plot-container p-0",
                  shiny::tabsetPanel(
                    id = ns_full("feature_details_tabs"),
                    type = "tabs",
                    
                    # MS1 Tab
                    shiny::tabPanel(
                      title = "MS1",
                      shiny::div(
                        class = "p-3 position-relative",
                        .app_util_create_maximize_button("ms1_plot", ns_full),
                        plotly::plotlyOutput(ns_full("ms1_plot"), height = "auto")
                      )
                    ),
                    
                    # MS2 Tab
                    shiny::tabPanel(
                      title = "MS2",
                      shiny::div(
                        class = "p-3 position-relative",
                        .app_util_create_maximize_button("ms2_plot", ns_full),
                        plotly::plotlyOutput(ns_full("ms2_plot"), height = "auto")
                      )
                    ),
                    
                    # Quality Tab
                    shiny::tabPanel(
                      title = "Quality",
                      shiny::div(
                        class = "p-3",
                        DT::dataTableOutput(ns_full("quality_table"))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Groups Tab
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("object-group", class = "mr-2"), "Groups"),
          
          shiny::div(class = "tab-content",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::div(
                  class = "mb-4",
                  
                  # Single line controls - using flexbox
                  shiny::div(
                    style = "display: flex; align-items: center; margin-bottom: 10px; gap: 15px;",
                    # Left side - Metadata checkbox
                    shiny::div(
                      style = "display: flex; align-items: center;",
                      shiny::tags$input(
                        type = "checkbox",
                        id = ns_full("show_metadata_groups"),
                        checked = FALSE,
                        style = "margin-right: 5px;"
                      ),
                      shiny::tags$label(
                        `for` = ns_full("show_metadata_groups"),
                        "Show Metadata",
                        style = "font-size: 13px; color: #5a5c69; margin: 0;"
                      )
                    ),
                    # Right side - Filter toggle
                    shiny::div(
                      style = "display: flex; align-items: center;",
                      shiny::tags$input(
                        type = "checkbox",
                        id = ns_full("show_filters_groups"),
                        checked = TRUE,
                        style = "margin-right: 5px;"
                      ),
                      shiny::tags$label(
                        `for` = ns_full("show_filters_groups"),
                        "Show Filters",
                        style = "font-size: 13px; color: #5a5c69; margin: 0;"
                      )
                    )
                  ),
                  
                  # Table wrapper
                  shiny::div(
                    class = "table-wrapper",
                    shiny::div(
                      class = "features-table-container",
                      DT::dataTableOutput(ns_full("groups_table"))
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Subjects Tab
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("users", class = "mr-2"), "Subjects"),
          
          shiny::div(class = "tab-content",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::div(
                  class = "plot-container empty-state",
                  shiny::div(class = "empty-state-icon", shiny::icon("users")),
                  shiny::h4("Subject data will be displayed here", class = "text-muted")
                )
              )
            )
          )
        ),
        
        # Fold Change Tab
        shiny::tabPanel(
          title = shiny::tagList(shiny::icon("chart-line", class = "mr-2"), "Fold Change"),
          
          shiny::div(class = "tab-content",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::div(
                  class = "plot-container empty-state",
                  shiny::div(class = "empty-state-icon", shiny::icon("chart-line")),
                  shiny::h4("Fold change data will be displayed here", class = "text-muted")
                )
              )
            )
          )
        )
      )
    )
  )
}
#' @noRd
S7::method(.mod_WorkflowAssembler_Result_Server, NonTargetAnalysisResults) <- function(x,
                                                                                       id,
                                                                                       ns,
                                                                                       reactive_analyses,
                                                                                       reactive_volumes,
                                                                                       reactive_config) {
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
    
    # Calculate summary metrics for the Overview tab
    output$total_analyses <- shiny::renderText({
      as.character(nts_data()@number_analyses)
    })
    
    output$total_features <- shiny::renderText({
      features <- nts_data()@number_features
      total <- sum(features[features > 0])
      as.character(total)
    })
    
    output$filtered_features_count <- shiny::renderText({
      filtered_features <- nts_data()@number_filtered_features
      total <- sum(filtered_features)
      as.character(total)
    })
    
    output$total_groups <- shiny::renderText({
      groups <- nts_data()@number_groups
      total <- sum(groups[groups > 0])
      as.character(total)
    })
    
    # Status indicators with UI formatting
    # Dynamic status indicators with UI formatting
    output$has_features_ui <- shiny::renderUI({
      value <- nts_data()@has_features
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    output$has_filtered_features_ui <- shiny::renderUI({
      value <- nts_data()@has_filtered_features
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    output$has_groups_ui <- shiny::renderUI({
      value <- nts_data()@has_groups
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    output$has_features_eic_ui <- shiny::renderUI({
      value <- nts_data()@has_features_eic
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    # Dynamic MS1 check
    output$has_features_ms1_ui <- shiny::renderUI({
      value <- nts_data()@has_features_ms1
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    # Dynamic MS2 check
    output$has_features_ms2_ui <- shiny::renderUI({
      value <- nts_data()@has_features_ms2
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })

    # Dynamic Suspects check
    output$has_features_suspects_ui <- shiny::renderUI({
      value <- nts_data()@has_features_suspects
      shiny::tags$span(class = ifelse(value, "status-yes", "status-no"), ifelse(value, "YES", "NO"))
    })
    
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
      width = NULL,  # Ensure no fixed width
      autosize = TRUE,  # Enable dynamic resizing
      margin = list(l = 60, r = 40, t = 40, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
    
      # Better title and axis labels
      title = list(
        text = paste0("Feature Distribution by ", ifelse(color_by == "replicates", "Replicates", "Analysis")),
        font = list(size = 18, color = "#333")
      ),
    
      xaxis = list(
        title = NULL,
        tickfont = list(size = 12),
        gridcolor = "#eee",
        categoryorder = "total descending"  # Ensure bars are ordered for better spacing
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
    p <- plotly::config(p, 
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "lasso2d", "select2d"
      ),
      displaylogo = FALSE,
      responsive = TRUE
    )
    
    return(p)
  })

  # Features table for the Features tab
  output$features_table <- DT::renderDT({
    # Fetch features data
    features <- get_features(nts_data())
    
    # Debug: Log features data structure
    # cat("Debug: Features data structure\n")
    # print(str(features, max.level = 2))
    
    # Debug: Check if plotting functions have access to ms1/ms2 data
    # if (nrow(features) > 0) {
    #   cat("Debug: Checking MS1/MS2 data availability for first feature\n")
    #   mass_data <- features[1, .(mass)]
    #   ms1_plot <- try(plot_features_ms1(nts_data(), mass = mass_data, legendNames = FALSE), silent = TRUE)
    #   ms2_plot <- try(plot_features_ms2(nts_data(), mass = mass_data, legendNames = FALSE), silent = TRUE)
    #   cat("Debug: MS1 plot data available =", !inherits(ms1_plot, "try-error"), "\n")
    #   cat("Debug: MS2 plot data available =", !inherits(ms2_plot, "try-error"), "\n")
    # } else {
    #   cat("Debug: No features available\n")
    # }
    
    # Enhanced flag logic with fallback to plotting data
    features$ms1_flag <- sapply(features$ms1, function(x) {
      !is.null(x) && length(x) > 0 && !all(is.na(x))
    })
    features$ms2_flag <- sapply(features$ms2, function(x) {
      !is.null(x) && length(x) > 0 && !all(is.na(x))
    })
    features$istd_flag <- sapply(features$istd, function(x) {
      !is.null(x) && length(x) > 0 && !all(is.na(x))
    })
    # Removed suspects_flag calculation since we don't need the column
  
    # Fallback: If flags are all FALSE but plots exist, infer from mass column
    if (nrow(features) > 0 && !any(features$ms1_flag) && !inherits(try(plot_features_ms1(nts_data(), mass = features[1, .(mass)]), silent = TRUE), "try-error")) {
      cat("Debug: Inferring MS1 flags from mass data\n")
      features$ms1_flag <- !is.na(features$mass) & features$mass > 0
    }
    if (nrow(features) > 0 && !any(features$ms2_flag) && !inherits(try(plot_features_ms2(nts_data(), mass = features[1, .(mass)]), silent = TRUE), "try-error")) {
      cat("Debug: Inferring MS2 flags from mass data\n")
      features$ms2_flag <- !is.na(features$mass) & features$mass > 0
    }
  
    # Debug: Log flag counts
    cat("Debug: MS1 flags =", sum(features$ms1_flag), "\n")
    cat("Debug: MS2 flags =", sum(features$ms2_flag), "\n")
  
    # Remove original nested list-columns
    nested_cols <- c("eic", "ms1", "ms2", "quality", "annotation", "istd", "suspects", "formulas", "compounds")
    features <- features[, !nested_cols, with = FALSE]
  
    # Rename flags to final column names (excluding suspects)
    data.table::setnames(features, 
      old = c("ms1_flag", "ms2_flag", "istd_flag"), 
      new = c("ms1", "ms2", "istd")
    )
  
    # Round numeric columns to 4 decimal places for readability
    numeric_cols <- c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "intensity", "area", "mass", "suppression_factor")
    for (col in numeric_cols) {
      if (col %in% names(features)) {
        features[[col]] <- round(features[[col]], 4)
      }
    }
  
    # Add a new column for checkboxes (initially unchecked)
    features$sel <- rep(FALSE, nrow(features))
  
    # Reorder columns to place sel as the second column after analysis
    setcolorder(features, c("sel", "analysis", setdiff(names(features), c("sel", "analysis"))))
  
    # Determine the index of the 'filtered' and 'sel' columns (0-based for JavaScript)
    filtered_col_index <- which(names(features) == "filtered") - 1
    sel_col_index <- which(names(features) == "sel") - 1
  
    # Render the DataTable with enhanced styling and checkbox rendering
    DT::datatable(
      features,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        processing = TRUE,
        scrollY = "400px",
        columnDefs = list(
          list(width = "50px", targets = c(1)),
          list(width = "200px", targets = c(0)),
          list(width = "200px", targets = c(2)),
          list(width = "100px", targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11)),
          list(width = "80px", targets = c(12)),
          list(width = "100px", targets = c(13)),
          list(width = "80px", targets = c(14, 15, 16)),
          list(width = "150px", targets = c(17)),
          list(width = "80px", targets = c(18)),
          list(width = "120px", targets = c(19)),
          list(className = "dt-right", targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17)),
          list(className = "dt-left", targets = c(0, 2, 13, 14, 15, 16, 18, 19)),
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
          ),
          list(
            targets = filtered_col_index,
            render = DT::JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    var checked = data === true ? 'checked' : '';",
              "    return '<input type=\"checkbox\" ' + checked + ' disabled style=\"pointer-events: none;\" />';",
              "  }",
              "  return data;",
              "}"
            ),
            className = "dt-center"
          )
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
      filter = "top",
      selection = "multiple"
    ) %>%
      DT::formatStyle(
        columns = names(features),
        fontSize = "14px",
        padding = "8px 12px"
      ) %>%
      DT::formatStyle(
        columns = intersect(numeric_cols, names(features)),
        backgroundColor = NULL,
        color = NULL
      )
  })

  shiny::observeEvent(input$deselect_all_features, {
    DT::selectRows(DT::dataTableProxy("features_table"), NULL)
  })

  # Handler for export to CSV
  output$export_features_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("features_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Get all features data
      features <- get_features(nts_data())
  
      # Replace list-cols with flags because we flattened them
      features$ms1 <- sapply(features$ms1, function(x) !is.null(x) && length(x) > 0)
      features$ms2 <- sapply(features$ms2, function(x) !is.null(x) && length(x) > 0)
      features$istd <- sapply(features$istd, function(x) !is.null(x) && length(x) > 0)
      
      # Remove nested columns for CSV export
      nested_cols <- c("eic", "quality", "annotation", "suspects", "formulas", "compounds")
      export_features <- features[, !nested_cols, with = FALSE]
      
      # Export to CSV
      write.csv(export_features, file, row.names = FALSE)
    }
  )

  # Handler for export to CSV for selected features
  output$export_selected_features_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("selected_features_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      selected_rows <- input$features_table_rows_selected
      features <- get_features(nts_data())
  
      # Replace list-cols with flags because we flattened them
      features$ms1 <- sapply(features$ms1, function(x) !is.null(x) && length(x) > 0)
      features$ms2 <- sapply(features$ms2, function(x) !is.null(x) && length(x) > 0)
      features$istd <- sapply(features$istd, function(x) !is.null(x) && length(x) > 0)
  
      # Remove nested columns
      nested_cols <- c("eic", "quality", "annotation", "suspects", "formulas", "compounds")
  
      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        selected_features <- features[selected_rows, !nested_cols, with = FALSE]
        write.csv(selected_features, file, row.names = FALSE)
      } else {
        # If no selection, export empty file or a message
        write.csv(data.frame(Message = "No features selected"), file, row.names = FALSE)
      }
    }
  )

  # Reactive value to store selected features
  selected_features <- shiny::reactive({
    selected_rows <- input$features_table_rows_selected
    
    # Fetch features data
    features <- get_features(nts_data())
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      return(NULL)
    }
    
    analysis <- NULL
    feature <- NULL
    
    # Data frame with analysis and feature columns for the selected rows
    selected_data <- features[selected_rows, .(analysis, feature)]
    
    # Convert to data frame
    as.data.frame(selected_data)
  })
  
  # Handler for removing selected features
  shiny::observeEvent(input$remove_selected_features, {
    # Get selected features
    selected <- selected_features()
    
    # Validate selection
    if (is.null(selected) || nrow(selected) == 0) {
      shiny::showNotification("No features selected for removal.", type = "warning")
      return()
    }
    
    # Get current NTS data
    nts <- nts_data()
    
    # Create a copy to modify
    feature_list <- nts$feature_list
    
    # Remove selected features
    tryCatch({
      for (i in seq_len(nrow(selected))) {
        analysis_i <- selected$analysis[i]
        feature_i <- selected$feature[i]
        feature_list[[analysis_i]] <- feature_list[[analysis_i]][
          !feature_list[[analysis_i]][["feature"]] %in% feature_i,
        ]
      }
      
      # Update NTS feature list
      nts$feature_list <- feature_list
      
      # Update reactive nts_data
      nts_data(nts)
      
      # Clear table selection
      DT::selectRows(DT::dataTableProxy("features_table"), NULL)
      
      # Notify user
      shiny::showNotification(
        paste(nrow(selected), "features removed successfully."),
        type = "message"
      )
    }, error = function(e) {
      shiny::showNotification(
        paste("Error removing features:", e$message),
        type = "error"
      )
    })
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
    quality_data <- do.call(rbind, lapply(seq_along(quality_list), function(i) {
      q <- quality_list[[i]]
      if (is.null(q) || length(q) == 0) {
        return(data.frame(feature = features[selected_rows[i], "feature", with = FALSE]$feature, 
                          message = "No quality data"))
      }
      if (is.list(q) || is.vector(q)) {
        # Convert named vector/list to data frame
        df <- as.data.frame(t(unlist(q)))
        # Add feature column
        df$feature <- features[selected_rows[i], "feature", with = FALSE]$feature
        return(df)
      } else {
        # If it's not a list or vector, return a placeholder
        return(data.frame(feature = features[selected_rows[i], "feature", with = FALSE]$feature, 
                          message = "Invalid quality data"))
      }
    }))
    
    # If the result is empty, return a placeholder
    if (nrow(quality_data) == 0) {
      return(data.frame(message = "No quality data available"))
    }
    
    # Identify numeric columns to round
    numeric_cols <- c("sn", "gauss_a", "gauss_u", "gauss_s", "gauss_f")
    for (col in numeric_cols) {
      if (col %in% names(quality_data)) {
        quality_data[[col]] <- round(as.numeric(quality_data[[col]]), 3)  # Round to 3 decimal places
      }
    }
    
    # Reorder columns to put feature first
    quality_data <- quality_data[, c("feature", setdiff(names(quality_data), "feature"))]
    
    return(quality_data)
  })

  # Feature peaks plot with Plotly
  output$feature_peaks_plot <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(selected_features()), "Please select one or more features from the table to display the plot.")
    )
    
    # Generate the plot using plot_features
    nts <- nts_data()
    p <- plot_features(nts, features = selected_features())
    
    # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
    p <- plotly::layout(p,
      width = NULL,  # Ensure no fixed width
      autosize = TRUE,  # Enable dynamic resizing
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
    p <- plotly::config(p, 
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "lasso2d", "select2d"
      ),
      displaylogo = FALSE,
      responsive = TRUE
    )
    
    return(p)
  })

  # MS1 plot with Plotly
  output$ms1_plot <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(selected_features_with_mass()), "Please select one or more features from the table to display the MS1 plot.")
    )
    
    # Generate the MS1 plot
    nts <- nts_data()
    mass_data <- selected_features_with_mass()
    p <- plot_features_ms1(nts, mass = mass_data, legendNames = TRUE)
    
    # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
    p <- plotly::layout(p,
      width = NULL,  # Ensure no fixed width
      autosize = TRUE,  # Enable dynamic resizing
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
    p <- plotly::config(p, 
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "lasso2d", "select2d"
      ),
      displaylogo = FALSE,
      responsive = TRUE
    )
    
    return(p)
  })

  # MS2 plot with Plotly
  output$ms2_plot <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(selected_features_with_mass()), "Please select one or more features from the table to display the MS2 plot.")
    )
    
    # Generate the MS2 plot
    nts <- nts_data()
    mass_data <- selected_features_with_mass()
    p <- plot_features_ms2(nts, mass = mass_data, legendNames = TRUE)
    
    # Ensure the plot is responsive by setting width = NULL and autosize = TRUE
    p <- plotly::layout(p,
      width = NULL,  # Ensure no fixed width
      autosize = TRUE,  # Enable dynamic resizing
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
    p <- plotly::config(p, 
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "lasso2d", "select2d"
      ),
      displaylogo = FALSE,
      responsive = TRUE
    )
    
    return(p)
  })

  # Quality table
  output$quality_table <- DT::renderDT({
    shiny::validate(
      need(!is.null(selected_quality_data()), "Please select one or more features from the table to display the quality data.")
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

  # Store filter visibility state
  show_filters_groups <- shiny::reactive({
    if (is.null(input$show_filters_groups)) TRUE else input$show_filters_groups
  })

  # Groups table for the Groups tab
  output$groups_table <- DT::renderDT({
    # Get checkbox values
    show_metadata <- if (is.null(input$show_metadata_groups)) FALSE else input$show_metadata_groups
    show_filters <- if (is.null(input$show_filters_groups)) TRUE else input$show_filters_groups
    
    # Fetch groups data with enhanced parameters
    groups <- get_groups(
      nts_data(),
      metadata = show_metadata,
      intensities = TRUE  # Always show intensities
    )
    
    # Rest of your existing code remains exactly the same...
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
    
    # Add chromatogram column with clickable icons
    groups$chromatogram <- sapply(1:nrow(groups), function(i) {
      paste0('<div class="chromatogram-icon" onclick="showChromatogram(\'', 
            groups$group[i], '\')" title="View chromatogram for ', groups$group[i], '">',
            '<i class="fas fa-chart-line"></i></div>')
    })
    
    # Reorder columns to put chromatogram as second column
    col_order <- c("group", "chromatogram", setdiff(names(groups), c("group", "chromatogram")))
    groups <- groups[, col_order, with = FALSE]
    
    # Find chromatogram column index (0-based for JavaScript)
    chromatogram_col_index <- which(names(groups) == "chromatogram") - 1
    
    # Render DataTable with fixed header scrolling
    DT::datatable(
      groups,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        processing = TRUE,
        scrollY = "400px",
        fixedHeader = TRUE,
        columnDefs = list(
          list(
            targets = chromatogram_col_index,
            orderable = FALSE,
            width = "80px",
            className = "dt-center"
          ),
          list(width = "150px", targets = 0)
        ),
        dom = 'rt<"bottom"lip>',
        lengthMenu = c(5, 10, 25, 50, 100),
        ordering = TRUE,
        searching = FALSE,
        searchHighlight = TRUE
      ),
      style = "bootstrap",
      class = "table table-striped table-hover",
      rownames = FALSE,
      filter = if (show_filters) "top" else "none"
    ) %>%
      DT::formatStyle(
        columns = names(groups),
        fontSize = "14px",
        padding = "8px 12px"
      ) %>%
      DT::formatStyle(
        columns = "chromatogram",
        textAlign = "center"
      )
  })

  # Reactive value to store selected group for chromatogram
  selected_group <- shiny::reactiveVal(NULL)
  
  # Handle chromatogram click
  shiny::observeEvent(input$show_chromatogram, {
    shiny::req(input$show_chromatogram$group)
    selected_group(input$show_chromatogram$group)
  })
  
  # Chromatogram plot
  output$chromatogram_plot <- plotly::renderPlotly({
    shiny::validate(
      need(!is.null(selected_group()), "Please select a group to display the chromatogram.")
    )
    
    # Generate the chromatogram plot using plot_groups
    nts <- nts_data()
    group_id <- selected_group()
    
    tryCatch({
      # Call plot_groups function with the selected group
      p <- plot_groups(nts, groups = data.frame(group = group_id))
      
      # Enhance the plotly object
      p <- plotly::layout(p,
        title = list(
          text = paste("Chromatogram for Group:", group_id),
          font = list(size = 18, color = "#333")
        ),
        xaxis = list(
          title = list(text = "Retention Time (RT)", font = list(size = 14)),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),
        yaxis = list(
          title = list(text = "Intensity", font = list(size = 14)),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),
        margin = list(l = 60, r = 40, t = 60, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
      
      # Configure plotly
      p <- plotly::config(p, 
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "autoScale2d", "hoverClosestCartesian",
          "hoverCompareCartesian", "lasso2d", "select2d"
        ),
        displaylogo = FALSE,
        responsive = TRUE
      )
      
      return(p)
      
    }, error = function(e) {
      # Handle errors gracefully
      plotly::plot_ly() %>%
        plotly::add_text(x = 0.5, y = 0.5, text = paste("Error loading chromatogram:", e$message),
                        textfont = list(size = 16, color = "red")) %>%
        plotly::layout(
          title = paste("Error: Group", group_id),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE)
        )
    })
  })

  # JavaScript for UI interactions
  shiny::observeEvent(1, {
    # Get the full namespace for use in JavaScript
    ns_prefix <- paste0("WorkflowAssembler-", id)
    
    # Initialize tooltips and chromatogram functionality
    shiny::insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = shiny::tags$script(shiny::HTML(paste0("
        // Global function to show chromatogram
        function showChromatogram(groupId) {
          Shiny.setInputValue('", ns_prefix, "-show_chromatogram', {
            group: groupId,
            timestamp: Date.now()
          });
          $('#", ns_prefix, "-chromatogram_modal').modal('show');
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
      ")))
    )
  }, once = TRUE)
    })
  }