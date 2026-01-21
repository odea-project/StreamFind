# MARK: .mod_WADB_Result_UI.DB_MassSpecResults_NonTargetAnalysis
#' @export
#' @noRd
.mod_WADB_Result_UI.DB_MassSpecResults_NonTargetAnalysis <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  ns_full <- function(name) ns(ns2(name))

  # MARK: Custom CSS
  custom_css <- shiny::tags$style(
    shiny::HTML(
      "
    .status-panel {
      background-color: white;
      border-radius: 8px;
      padding: 16px;
      height: 100%;
    }
    .status-item {
      display: flex;
      justify-content: space-between;
      padding: 6px 0;
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
    .nav-tabs-custom {
      margin-bottom: 0px;
    }
    .features-controls-bar {
      background-color: white;
      padding: 10px 15px;
      height: 60px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .features-controls-bar .shiny-input-container {
      margin-bottom: 0;
      display: flex;
      align-items: center;
    }
    .features-controls-bar .checkbox {
      margin: 0;
      display: flex;
      align-items: center;
    }
    .features-controls-bar .checkbox input {
      margin-right: 4px;
    }
    "
    )
  )

  shiny::tagList(
    custom_css,
    .app_util_plot_maximize_js(),
    .app_util_create_plot_modal(ns_full),
    # MARK: Main TabBox
    shinydashboard::tabBox(
      id = ns_full("main_tabs"),
      width = 12,
      height = "calc(100vh - 60px - 60px)",
      # MARK: Summary Tab
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("chart-pie", class = "mr-2"),
          "Summary"
        ),
        shiny::div(
          class = "tab-content",
          style = "max-height: calc(100vh - 120px); overflow-y: auto; padding: 0;",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::div(
                class = "status-panel",
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("chart-line", class = "mr-2"),
                    "Total Analyses"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::textOutput(ns_full("total_analyses"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("gears", class = "mr-2"),
                    "Total Features"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::textOutput(ns_full("total_features"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("filter", class = "mr-2"),
                    "Filtered Features"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::textOutput(ns_full("filtered_features_count"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("network-wired", class = "mr-2"),
                    "Total Groups"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::textOutput(ns_full("total_groups"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("wave-square", class = "mr-2"),
                    "Has EIC?"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::uiOutput(ns_full("has_features_eic_ui"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("chart-bar", class = "mr-2"),
                    "Has MS1?"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::uiOutput(ns_full("has_features_ms1_ui"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("chart-area", class = "mr-2"),
                    "Has MS2?"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::uiOutput(ns_full("has_features_ms2_ui"), inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "status-item",
                  shiny::span(
                    class = "status-label",
                    shiny::icon("list-check", class = "mr-2"),
                    "Suspects"
                  ),
                  shiny::span(
                    class = "status-value",
                    shiny::uiOutput(ns_full("has_features_suspects_ui"), inline = TRUE)
                  )
                )
              )
            ),
            shiny::div(
              style = "height: calc(100vh - 200px); display: flex; flex-direction: column;",
              shiny::div(
                class = "d-flex justify-content-center align-items-center",
                style = "height: 60px; background-color: white; padding: 10px;",
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::tags$button(
                    class = "btn btn-outline-primary active",
                    style = "margin-right: 10px;",
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
                    style = "margin-right: 10px;",
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
              shiny::column(
                width = 12,
                class = "position-relative",
                style = "flex: 1; background-color: white; padding: 5px;",
                .app_util_create_maximize_button("features_chart", ns_full),
                plotly::plotlyOutput(ns_full("features_chart"), height = "100%")
              )
            )
          )
        )
      ),
      # MARK: Features Scatter Tab
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("braille", class = "mr-2"), "Features"),
        shiny::div(
          class = "tab-content",
          style = "max-height: calc(100vh - 120px); overflow-y: auto; padding: 0;",
          shiny::div(
            class = "features-controls-bar",
            style = "display: flex; align-items: center; justify-content: space-between;",
            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
              shiny::div(
                style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
                shiny::span("Group by:", style = "font-weight: 500;"),
                shiny::checkboxInput(ns_full("scatter_color_analysis"), "Analysis", value = TRUE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_color_replicate"), "Replicate", value = FALSE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_color_feature"), "Feature", value = FALSE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_color_component"), "Component", value = FALSE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_color_group"), "Group", value = FALSE, width = "auto")
              ),
              shiny::div(
                style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
                shiny::span("Select by:", style = "font-weight: 500;"),
                shiny::checkboxInput(ns_full("scatter_select_analysis"), "Analysis", value = TRUE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_select_feature"), "Feature", value = TRUE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_select_component"), "Component", value = FALSE, width = "auto"),
                shiny::checkboxInput(ns_full("scatter_select_group"), "Group", value = FALSE, width = "auto")
              )
            ),
            shiny::div(
              class = "btn-group btn-group-sm",
              shiny::actionButton(ns_full("scatter_prop_20_80"), "20:80", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_30_70"), "30:70", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_40_60"), "40:60", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_50_50"), "50:50", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_60_40"), "60:40", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_70_30"), "70:30", class = "btn btn-outline-primary btn-sm"),
              shiny::actionButton(ns_full("scatter_prop_80_20"), "80:20", class = "btn btn-outline-primary btn-sm")
            )
          ),
          shiny::div(
            id = ns_full("scatter_content_container"),
            style = "display: flex; height: calc(100vh - 250px);",
            shiny::div(
              id = ns_full("features_scatter_panel"),
              style = "height: calc(100vh - 250px); padding: 10px; overflow: auto; width: 55%; display: flex; flex-direction: column; gap: 8px;",
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  open = TRUE,
                  width = "350px",
                  style = "max-height: calc(100vh - 320px); overflow-y: auto; overflow-x: visible; padding: 16px 24px;",
                  shiny::textInput(
                    ns_full("scatter_search"),
                    "Search (regex)",
                    value = "",
                    placeholder = "Filter features (regex)..."
                  ),
                  shiny::uiOutput(ns_full("scatter_numeric_filters"))
                ),
                fill = TRUE,
                shiny::div(
                  style = "flex: 1 1 auto; min-width: 0; width: 100%; position: relative;",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("features_scatter_plot", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("features_scatter_plot"),
                    height = "calc(100vh - 320px)",
                    width = "100%"
                  )
                )
              )
            ),
            shiny::div(
              id = ns_full("features_scatter_details_panel"),
              style = "height: calc(100vh - 250px); padding: 10px; overflow: hidden; width: 45%;",
              shiny::tabsetPanel(
                id = ns_full("feature_scatter_details_tabs"),
                type = "tabs",
                shiny::tabPanel(
                  title = "EIC",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_peaks_plot_scatter", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("feature_peaks_plot_scatter"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "XIC",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_xic_plot_scatter", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("feature_xic_plot_scatter"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "MS1",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_ms1_plot_scatter", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("feature_ms1_plot_scatter"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "MS2",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_ms2_plot_scatter", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("feature_ms2_plot_scatter"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "Details",
                  shiny::div(
                    class = "p-3",
                    style = "height: calc(100vh - 300px); overflow: auto;",
                    DT::dataTableOutput(ns_full("feature_details_table_scatter"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# MARK: .mod_WADB_Result_Server.DB_MassSpecResults_NonTargetAnalysis
#' @export
#' @noRd
.mod_WADB_Result_Server.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    id,
    ns,
    reactive_analyses,
    reactive_volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns_full <- session$ns

    # MARK: Helpers
    status_tag <- function(value) {
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    }

    # MARK: nts_data
    nts_data <- shiny::reactiveVal()

    shiny::observe({
      shiny::validate(shiny::need(!is.null(x), "NTA data is not available"))
      nts_data(x)
    })

    # MARK: features_data
    features_data <- shiny::reactive({
      nts <- nts_data()
      fts <- data.table::as.data.table(get_features(nts, filtered = TRUE))
      if (nrow(fts) == 0) return(fts)
      digits_for_col <- function(col) {
        col_lower <- tolower(col)
        digits <- 4
        if (col_lower %in% c("ppm", "sn")) digits <- 1
        if (col_lower %in% c("gaussian_sigma")) digits <- 1
        if (col_lower %in% c("gaussian_mu", "gaussian_a")) digits <- 0
        if (col_lower == "fwhm_mz") digits <- 4
        mz_in_col <- grepl("^mz", col_lower) || grepl("mzmin|mzmax|mass", col_lower)
        if (mz_in_col) digits <- 4
        no_decimals <- grepl("intensity|area|size|noise|plates", col_lower)
        no_decimals <- no_decimals || grepl("^rt", col_lower)
        no_decimals <- no_decimals || (grepl("width|fwhm", col_lower) && col_lower != "fwhm_mz")
        if (no_decimals) digits <- 0
        if (grepl("gaussian_r2|correction|jaggedness|sharpness|asymmetry", col_lower)) digits <- 2
        digits
      }
      num_cols <- names(fts)[sapply(fts, is.numeric)]
      for (col in num_cols) {
        d <- digits_for_col(col)
        fts[[col]] <- round(fts[[col]], d)
      }
      fts
    })

    # MARK: chart_color_by
    chart_color_by <- shiny::reactiveVal("replicates")

    shiny::observeEvent(input$chart_color_by, {
      chart_color_by(input$chart_color_by)
    })

    # MARK: Layout proportions (features scatter tab)
    scatter_layout_proportions <- shiny::reactiveVal(c(80, 20))
    scatter_numeric_cols <- shiny::reactive({
      fts <- data.table::as.data.table(features_data())
      names(fts)[sapply(fts, is.numeric)]
    })
    shiny::observeEvent(input$scatter_prop_20_80, {
      scatter_layout_proportions(c(20, 80))
    })
    shiny::observeEvent(input$scatter_prop_30_70, {
      scatter_layout_proportions(c(30, 70))
    })
    shiny::observeEvent(input$scatter_prop_40_60, {
      scatter_layout_proportions(c(40, 60))
    })
    shiny::observeEvent(input$scatter_prop_50_50, {
      scatter_layout_proportions(c(50, 50))
    })
    shiny::observeEvent(input$scatter_prop_60_40, {
      scatter_layout_proportions(c(60, 40))
    })
    shiny::observeEvent(input$scatter_prop_70_30, {
      scatter_layout_proportions(c(70, 30))
    })
    shiny::observeEvent(input$scatter_prop_80_20, {
      scatter_layout_proportions(c(80, 20))
    })
    shiny::observe({
      props <- scatter_layout_proportions()
      table_width <- props[1]
      plots_width <- props[2]
      table_id <- session$ns("features_scatter_panel")
      plots_id <- session$ns("features_scatter_details_panel")
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$style(shiny::HTML(paste0(
          "
            #", table_id, " { width: ", table_width, "% !important; }
            #", plots_id, " { width: ", plots_width, "% !important; }
          "
        )))
      )
      current_prop <- paste0(table_width, "_", plots_width)
      button_id <- session$ns(paste0("scatter_prop_", current_prop))
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(shiny::HTML(paste0(
          "
            $('.features-controls-bar .btn').removeClass('active');
            $('#", button_id, "').addClass('active');
          "
        )))
      )
    })

    # MARK: UI scatter_numeric_filters
    output$scatter_numeric_filters <- shiny::renderUI({
      fts <- data.table::as.data.table(features_data())
      if (nrow(fts) == 0) return(NULL)
      num_cols <- names(fts)[sapply(fts, is.numeric)]
      log_cols <- names(fts)[sapply(fts, is.logical)]

      slider_specs <- function(col, rng) {
        digits <- 3
        col_lower <- tolower(col)
        if (col_lower == "fwhm_mz") {
          digits <- 4
        } else if (col_lower == "gaussian_sigma") {
          digits <- 1
        } else if (col_lower %in% c("gaussian_mu", "gaussian_a")) {
          digits <- 0
        }
        four_decimals <- grepl("^mz", col, ignore.case = TRUE)
        four_decimals <- four_decimals || grepl("mzmin|mzmax|mass", col_lower)
        if (four_decimals) {
          digits <- 4
        }
        if (col_lower == "ppm") {
          digits <- 1
        }
        if (col_lower == "sn") {
          digits <- 1
        }
        zero_decimals <- grepl("intensity|area|size|noise|plates", col_lower)
        zero_decimals <- zero_decimals || grepl("^rt", col_lower)
        zero_decimals <- zero_decimals || (grepl("width|fwhm", col_lower) && col_lower != "fwhm_mz")
        if (zero_decimals) {
          digits <- 0
        }
        if (grepl("gaussian_r2|correction|jaggedness|sharpness|asymmetry", col_lower)) {
          digits <- 2
        }
        step <- if (digits == 0) 1 else 10^-digits
        list(
          min = rng[1],
          max = rng[2],
          value = rng,
          step = step
        )
      }

      slider_list <- NULL
      if (length(num_cols) > 0) {
        slider_list <- lapply(num_cols, function(col) {
          vals <- fts[[col]]
          vals <- vals[is.finite(vals)]
          if (length(vals) == 0) return(NULL)
          rng <- range(vals, na.rm = TRUE)
          specs <- slider_specs(col, rng)
          shiny::sliderInput(
            ns_full(paste0("scatter_filter_", col)),
            label = col,
            min = specs$min,
            max = specs$max,
            value = specs$value,
            step = specs$step
          )
        })
        slider_list <- slider_list[!vapply(slider_list, is.null, logical(1))]
      }

      logi_list <- NULL
      if (length(log_cols) > 0) {
        logi_list <- lapply(log_cols, function(col) {
          col_lower <- tolower(col)
          default_sel <- if (col_lower == "filtered") "FALSE" else c("TRUE", "FALSE")
          shiny::checkboxGroupInput(
            ns_full(paste0("scatter_filter_", col)),
            label = paste0(col, " (TRUE/FALSE)"),
            choices = c("TRUE", "FALSE"),
            selected = default_sel,
            inline = TRUE
          )
        })
      }

      ui_elems <- c(logi_list, slider_list)
      if (length(ui_elems) == 0) return(NULL)
      shiny::tagList(ui_elems)
    })

    # MARK: summary_data
    summary_data <- shiny::reactive({
      nts <- nts_data()
      info_analyses <- info(nts$analyses)
      all_fts <- features_data()
      if (nrow(all_fts) == 0) {
        counts <- data.frame(
          analysis = info_analyses$analysis,
          total = 0,
          filtered = 0,
          not_filtered = 0,
          replicate = info_analyses$replicate
        )
        return(list(
          info = info_analyses,
          counts = counts,
          total_analyses = nrow(info_analyses),
          total_features = 0,
          filtered_features = 0,
          total_groups = 0,
          has_eic = FALSE,
          has_ms1 = FALSE,
          has_ms2 = FALSE
        ))
      }
      counts <- all_fts[, .(total = .N, filtered = sum(filtered, na.rm = TRUE)), by = analysis]
      counts$not_filtered <- counts$total - counts$filtered
      counts$replicate <- info_analyses$replicate[match(counts$analysis, info_analyses$analysis)]
      non_filtered_fts <- all_fts[!all_fts$filtered, ]
      total_groups <- 0
      if (nrow(non_filtered_fts) > 0 && "feature_group" %in% colnames(non_filtered_fts)) {
        total_groups <- data.table::uniqueN(non_filtered_fts[feature_group != "" & !is.na(feature_group)]$feature_group)
      }
      has_eic <- FALSE
      has_ms1 <- FALSE
      has_ms2 <- FALSE
      if ("eic_size" %in% colnames(all_fts)) {
        has_eic <- any(all_fts$eic_size > 0, na.rm = TRUE)
      }
      if ("ms1_size" %in% colnames(all_fts)) {
        has_ms1 <- any(all_fts$ms1_size > 0, na.rm = TRUE)
      }
      if ("ms2_size" %in% colnames(all_fts)) {
        has_ms2 <- any(all_fts$ms2_size > 0, na.rm = TRUE)
      }
      list(
        info = info_analyses,
        counts = counts,
        total_analyses = nrow(info_analyses),
        total_features = sum(counts$total, na.rm = TRUE),
        filtered_features = sum(counts$filtered, na.rm = TRUE),
        total_groups = total_groups,
        has_eic = has_eic,
        has_ms1 = has_ms1,
        has_ms2 = has_ms2
      )
    })

    # MARK: summary outputs
    output$total_analyses <- shiny::renderText({
      as.character(summary_data()$total_analyses)
    })
    output$total_features <- shiny::renderText({
      as.character(summary_data()$total_features)
    })
    output$filtered_features_count <- shiny::renderText({
      as.character(summary_data()$filtered_features)
    })
    output$total_groups <- shiny::renderText({
      as.character(summary_data()$total_groups)
    })
    output$has_features_eic_ui <- shiny::renderUI({
      status_tag(summary_data()$has_eic)
    })
    output$has_features_ms1_ui <- shiny::renderUI({
      status_tag(summary_data()$has_ms1)
    })
    output$has_features_ms2_ui <- shiny::renderUI({
      status_tag(summary_data()$has_ms2)
    })
    output$has_features_suspects_ui <- shiny::renderUI({
      # TODO: implement suspect detection once available for DB_MassSpecResults_NonTargetAnalysis
      status_tag(FALSE)
    })

    # MARK: summary_chart
    output$features_chart <- plotly::renderPlotly({
      counts <- data.table::as.data.table(summary_data()$counts)
      shiny::validate(
        shiny::need(nrow(counts) > 0, "No features available to plot.")
      )
      color_by <- chart_color_by()
      if (color_by == "replicates" && "replicate" %in% colnames(counts)) {
        agg <- counts[
          ,
          .(
            mean_features = mean(not_filtered, na.rm = TRUE),
            sd_features = stats::sd(not_filtered, na.rm = TRUE),
            mean_filtered = mean(filtered, na.rm = TRUE)
          ),
          by = replicate
        ]
        agg$sd_features[is.na(agg$sd_features)] <- 0
        pal <- .get_colors(agg$replicate)
        plotly::plot_ly(
          data = agg,
          x = ~replicate,
          y = ~mean_features,
          type = "bar",
          color = ~replicate,
          colors = pal,
          error_y = list(type = "data", array = agg$sd_features, visible = TRUE),
          hovertemplate = paste(
            "Replicate: %{x}<br>",
            "Mean Features: %{y}<br>",
            "SD: %{error_y.array}<br>",
            "Mean Filtered: %{customdata}",
            "<extra></extra>"
          ),
          customdata = agg$mean_filtered
        ) %>%
          plotly::layout(
            margin = list(l = 60, r = 40, t = 40, b = 40),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(
              title = NULL,
              tickfont = list(size = 12),
              gridcolor = "#eee",
              categoryorder = "array",
              categoryarray = agg$replicate
            ),
            yaxis = list(
              title = list(
                text = "Number of Features",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),
            bargap = 0.1,
            legend = list(
              title = list(text = "Replicate")
            )
          ) %>%
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            responsive = TRUE
          )
      } else {
        counts$color_by <- counts$analysis
        pal <- .get_colors(counts$color_by)
        plotly::plot_ly(
          data = counts,
          x = ~analysis,
          y = ~not_filtered,
          type = "bar",
          color = ~color_by,
          colors = pal,
          hovertemplate = paste(
            "Analysis: %{x}<br>",
            "Features: %{y}<br>",
            "Filtered: %{customdata}",
            "<extra></extra>"
          ),
          customdata = counts$filtered
        ) %>%
          plotly::layout(
            margin = list(l = 60, r = 40, t = 40, b = 40),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(
              title = NULL,
              tickfont = list(size = 12),
              gridcolor = "#eee",
              categoryorder = "total descending"
            ),
            yaxis = list(
              title = list(
                text = "Number of Features",
                font = list(size = 14, color = "#555")
              ),
              tickfont = list(size = 12),
              gridcolor = "#eee"
            ),
            bargap = 0.1,
            legend = list(
              title = list(text = "Analysis")
            )
          ) %>%
          plotly::config(
            displayModeBar = TRUE,
            displaylogo = FALSE,
            responsive = TRUE
          )
      }
    })

    # MARK: features_scatter_data
    features_scatter_data <- shiny::reactive({
      fts <- data.table::as.data.table(features_data())
      if (nrow(fts) == 0) return(fts)

      # Make explicit copy to avoid shallow copy warning with :=
      fts <- data.table::copy(fts)
      fts$analysis <- as.character(fts$analysis)
      fts$feature <- as.character(fts$feature)
      fts$replicate <- as.character(fts$replicate)

      # Apply text search (regex) across all columns
      search_term <- input$scatter_search
      if (!is.null(search_term) && nzchar(search_term)) {
        row_txt <- apply(fts, 1, function(r) paste(r, collapse = " "))
        keep_idx <- grepl(search_term, row_txt, perl = TRUE, ignore.case = TRUE)
        fts <- fts[keep_idx]
      }

      # Apply numeric filters
      num_cols <- names(fts)[sapply(fts, is.numeric)]
      for (col in num_cols) {
        rng <- input[[paste0("scatter_filter_", col)]]
        if (!is.null(rng) && length(rng) == 2 && all(is.finite(rng))) {
          fts <- fts[fts[[col]] >= rng[1] & fts[[col]] <= rng[2]]
        }
      }

      # Apply logical filters
      log_cols <- names(fts)[sapply(fts, is.logical)]
      for (col in log_cols) {
        sel <- input[[paste0("scatter_filter_", col)]]
        if (!is.null(sel) && length(sel) > 0) {
          keep_vals <- as.logical(sel)
          fts <- fts[fts[[col]] %in% keep_vals]
        }
      }

      if (nrow(fts) == 0) return(fts)

      fts$rel_intensity <- NA_real_
      if ("intensity" %in% colnames(fts) && "analysis" %in% colnames(fts)) {
        max_intensity_global <- max(fts$intensity, na.rm = TRUE)
        fts$rel_intensity <- fts$intensity / max_intensity_global
        fts$rel_intensity[is.infinite(fts$rel_intensity) | is.na(fts$rel_intensity)] <- 0
      } else {
        fts$rel_intensity <- 0
      }
      fts$dot_size <- 6 + 10 * fts$rel_intensity
      fts
    })

    # MARK: scatter_color_cols & scatter_selection_cols
    scatter_color_cols <- shiny::reactive({
      cols <- character(0)
      if (isTRUE(input$scatter_color_analysis)) cols <- c(cols, "analysis")
      if (isTRUE(input$scatter_color_replicate)) cols <- c(cols, "replicate")
      if (isTRUE(input$scatter_color_feature)) cols <- c(cols, "feature")
      if (isTRUE(input$scatter_color_component)) cols <- c(cols, "feature_component")
      if (isTRUE(input$scatter_color_group)) cols <- c(cols, "feature_group")
      if (length(cols) == 0) cols <- "analysis"
      cols
    })
    scatter_selection_cols <- shiny::reactive({
      cols <- character(0)
      if (isTRUE(input$scatter_select_analysis)) cols <- c(cols, "analysis")
      if (isTRUE(input$scatter_select_feature)) cols <- c(cols, "feature")
      if (isTRUE(input$scatter_select_component)) cols <- c(cols, "feature_component")
      if (isTRUE(input$scatter_select_group)) cols <- c(cols, "feature_group")
      if (length(cols) == 0) cols <- "feature"
      cols
    })

    # MARK: features_scatter_plot
    output$features_scatter_plot <- plotly::renderPlotly({
      fts <- as.data.frame(features_scatter_data())
      shiny::validate(shiny::need(nrow(fts) > 0, "No features available to plot."))

      color_cols <- scatter_color_cols()
      color_cols <- color_cols[color_cols %in% colnames(fts)]
      if (length(color_cols) == 0) color_cols <- "analysis"

      fts[, color_cols] <- lapply(fts[, color_cols, drop = FALSE], as.character)
      for (col in color_cols) fts[[col]][is.na(fts[[col]])] <- ""
      fts$color_var <- do.call(paste, c(fts[, color_cols, drop = FALSE], sep = "_"))

      pal <- .get_colors(unique(fts$color_var))
      hide_legend <- length(unique(fts$color_var)) > 50

      sel_cols <- scatter_selection_cols()
      sel_cols <- sel_cols[sel_cols %in% colnames(fts)]
      if (length(sel_cols) == 0) sel_cols <- intersect(c("analysis", "feature"), colnames(fts))
      fts[, sel_cols] <- lapply(fts[, sel_cols, drop = FALSE], as.character)
      for (col in sel_cols) fts[[col]][is.na(fts[[col]])] <- ""
      fts$scatter_key <- do.call(paste, c(fts[, sel_cols, drop = FALSE], sep = "||"))

      # Ensure size is numeric vector to avoid ordering issues with formula notation
      # size_values <- as.numeric(fts$dot_size)

      p <- plotly::plot_ly(
        data = fts,
        source = "features_scatter",
        x = ~rt,
        y = ~mz,
        type = "scattergl",
        mode = "markers",
        color = ~color_var,
        colors = pal,
        marker = list(
          sizemode = "diameter",
          size = ~dot_size,
          sizemin = 3,
          line = list(width = 0)
        ),
        key = ~scatter_key,
        hoverinfo = "none"
      )

      p <- plotly::layout(
        p,
        margin = list(l = 60, r = 30, t = 30, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(
          title = list(text = "Retention Time"),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),
        yaxis = list(
          title = list(text = "<i>m/z</i>"),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        ),
        legend = list(
          title = list(text = paste(color_cols, collapse = ", ")),
          orientation = "h",
          x = 0,
          y = -0.15
        ),
        showlegend = !hide_legend
      )

      p <- plotly::config(
        p,
        displayModeBar = TRUE,
        displaylogo = FALSE,
        responsive = TRUE
      )

      p <- plotly::event_register(p, "plotly_selected")
      p <- plotly::event_register(p, "plotly_click")
      p
    })

    # MARK: selected_features_scatter
    selected_features_scatter <- shiny::reactive({
      evt <- plotly::event_data("plotly_selected", source = "features_scatter")
      if (is.null(evt) || nrow(evt) == 0) {
        evt <- plotly::event_data("plotly_click", source = "features_scatter")
      }
      if (is.null(evt) || nrow(evt) == 0) return(NULL)
      keys <- evt$key
      if (is.null(keys)) return(NULL)
      keys <- as.character(keys)

      fts <- data.table::copy(features_scatter_data())
      if (!nrow(fts)) return(NULL)

      sel_cols <- scatter_selection_cols()
      sel_cols <- sel_cols[sel_cols %in% colnames(fts)]
      if (length(sel_cols) == 0) sel_cols <- intersect(c("analysis", "feature"), colnames(fts))
      if ("feature_component" %in% sel_cols && "feature_component" %in% colnames(fts)) {
        fts <- fts[feature_component != "" & !is.na(feature_component)]
      }
      if ("feature_group" %in% sel_cols && "feature_group" %in% colnames(fts)) {
        fts <- fts[feature_group != "" & !is.na(feature_group)]
      }
      if (!nrow(fts)) return(NULL)

      key_parts <- strsplit(keys, "||", fixed = TRUE)

      sel <- rep(TRUE, nrow(fts))
      lapply(seq_along(sel_cols), function(i) {
        sel <<- sel & fts[[sel_cols[i]]] %in% key_parts[[1]][i]
        invisible(NULL)
      })

      fts[sel, c("analysis", "feature"), with = FALSE]
    })

    # MARK: feature_peaks_plot_scatter
    output$feature_peaks_plot_scatter <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          nrow(selected_features_scatter()) > 0,
          "Select one or more points to view EIC."
        )
      )
      nts <- nts_data()
      p <- plot_features(
        nts,
        features = selected_features_scatter(),
        filtered = TRUE,
        showDetails = TRUE
      )
      shiny::validate(shiny::need(!is.null(p), "No EIC data for selected features."))
      plotly::layout(
        p,
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })

    # MARK: feature_ms1_plot_scatter
    output$feature_ms1_plot_scatter <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          nrow(selected_features_scatter()) > 0,
          "Select one or more points to view MS1."
        )
      )
      nts <- nts_data()
      p <- plot_features_ms1(nts, features = selected_features_scatter(), filtered = TRUE)
      shiny::validate(shiny::need(!is.null(p), "No MS1 data for selected features."))
      plotly::layout(
        p,
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })

    # MARK: feature_ms2_plot_scatter
    output$feature_ms2_plot_scatter <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          nrow(selected_features_scatter()) > 0,
          "Select one or more points to view MS2."
        )
      )
      nts <- nts_data()
      p <- plot_features_ms2(nts, features = selected_features_scatter(), filtered = TRUE)
      shiny::validate(shiny::need(!is.null(p), "No MS2 data for selected features."))
      plotly::layout(
        p,
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })

    output$feature_xic_plot_scatter <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          nrow(selected_features_scatter()) > 0,
          "Select one or more points to view XIC."
        )
      )
      nts <- nts_data()
      p <- map_features(nts, features = selected_features_scatter(), filtered = TRUE, showDetails = TRUE)
      shiny::validate(shiny::need(!is.null(p), "No XIC data for selected features."))
      plotly::layout(
        p,
        margin = list(l = 50, r = 30, t = 30, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })

    # MARK: feature_details_table_scatter
    output$feature_details_table_scatter <- DT::renderDT({
      sel <- selected_features_scatter()
      shiny::validate(shiny::need(nrow(sel) > 0, "Select one or more points to view details."))

      fts <- data.table::copy(features_data()[analysis %in% sel$analysis & feature %in% sel$feature, ])

      if (nrow(fts) == 0) {
        return(DT::datatable(
          data.frame(Message = "No details available for selected features."),
          options = list(dom = "t", paging = FALSE, ordering = FALSE),
          style = "bootstrap",
          class = "table table-striped table-hover",
          rownames = FALSE
        ))
      }

      keep_cols <- colnames(fts)
      keep_cols <- !keep_cols %in% c(
        "eic_rt",
        "eic_mz",
        "eic_intensity",
        "eic_baseline",
        "eic_smoothed",
        "ms1_mz",
        "ms1_intensity",
        "ms2_mz",
        "ms2_intensity",
        "rel_intensity"
      )
      fts <- fts[, keep_cols, with = FALSE]
      n_sel <- nrow(fts)
      prop_names <- colnames(fts)
      rows <- lapply(prop_names, function(p) {
        vals <- as.character(fts[[p]])
        data.frame(
          Property = p,
          t(as.matrix(vals)),
          stringsAsFactors = FALSE
        )
      })
      details_dt <- data.table::rbindlist(rows, fill = TRUE)
      if (n_sel > 1) {
        setnames(details_dt, c("Property", paste0("Value ", seq_len(n_sel))))
      } else {
        setnames(details_dt, c("Property", "Value"))
      }
      DT::datatable(
        details_dt,
        options = list(
          dom = "tip",
          paging = FALSE,
          ordering = FALSE,
          autoWidth = TRUE
        ),
        style = "bootstrap",
        class = "table table-striped table-hover",
        rownames = FALSE
      )
    })
  })
}
