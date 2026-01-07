#' @export
#' @noRd
.mod_WADB_Result_UI.DB_MassSpecResults_NonTargetAnalysis <- function(x, id, ns) {
  ns2 <- shiny::NS(id)
  ns_full <- function(name) ns(ns2(name))

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
    .tabbox-container .col-sm-12 {
      padding-left: 0 !important;
      padding-right: 0 !important;
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
    "
    )
  )

  shiny::tagList(
    custom_css,
    .app_util_plot_maximize_js(),
    .app_util_create_plot_modal(ns_full),
    shinydashboard::tabBox(
      id = ns_full("main_tabs"),
      width = 12,
      height = "calc(100vh - 60px)",
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
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("table", class = "mr-2"), "Features"),
        shiny::div(
          class = "tab-content",
          style = "max-height: calc(100vh - 120px); overflow-y: auto; padding: 0;",
          shiny::div(
            class = "features-controls-bar",
            shiny::div(
              class = "btn-group",
              shiny::actionButton(
                ns_full("deselect_all_features"),
                "Deselect All",
                icon = shiny::icon("times-circle"),
                class = "btn btn-outline-secondary btn-sm",
                style = "padding-right: 10px;"
              ),
              shiny::downloadButton(
                ns_full("export_features_csv"),
                "Export to CSV",
                icon = shiny::icon("file-csv"),
                class = "btn btn-outline-primary btn-sm ml-2",
                style = "padding-right: 10px;"
              ),
              shiny::downloadButton(
                ns_full("export_selected_features_csv"),
                "Export Selected to CSV",
                icon = shiny::icon("file-csv"),
                class = "btn btn-outline-primary btn-sm ml-2",
                style = "padding-right: 10px;"
              ),
              shiny::actionButton(
                ns_full("remove_selected_features"),
                "Remove Selected Features",
                icon = shiny::icon("trash-alt"),
                class = "btn btn-outline-danger btn-sm ml-2",
                style = "padding-right: 10px;"
              )
            )
          ),
          shiny::div(
            id = ns_full("main_content_container"),
            style = "display: flex; height: calc(100vh - 250px);",
            shiny::div(
              id = ns_full("features_table_panel"),
              style = "height: calc(100vh - 250px); padding: 10px; overflow: auto;",
              DT::dataTableOutput(ns_full("features_table"), height = "auto", width = "98%")
            ),
            shiny::div(
              id = ns_full("features_plots_panel"),
              style = "height: calc(100vh - 250px); padding: 10px; overflow: hidden;",
              shiny::tabsetPanel(
                id = ns_full("feature_details_tabs"),
                type = "tabs",
                shiny::tabPanel(
                  title = "EIC",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_peaks_plot", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("feature_peaks_plot"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "MS1",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_ms1_plot", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("ms1_plot"),
                    height = "calc(100vh - 320px)"
                  )
                ),
                shiny::tabPanel(
                  title = "MS2",
                  height = "100%",
                  shiny::div(
                    style = "height: 30px; position: relative;",
                    .app_util_create_maximize_button("feature_ms2_plot", ns_full)
                  ),
                  plotly::plotlyOutput(
                    ns_full("ms2_plot"),
                    height = "calc(100vh - 320px)"
                  )
                ),
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
      )
    )
  )
}

#' @export
#' @noRd
.mod_WADB_Result_Server.DB_MassSpecResults_NonTargetAnalysis <- function(
    x,
    id,
    ns,
    reactive_analyses,
    reactive_volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    nts_data <- shiny::reactiveVal()

    shiny::observe({
      shiny::validate(shiny::need(!is.null(x), "NTA data is not available"))
      nts_data(x)
    })

    chart_color_by <- shiny::reactiveVal("replicates")

    shiny::observeEvent(input$chart_color_by, {
      chart_color_by(input$chart_color_by)
    })

    status_tag <- function(value) {
      shiny::tags$span(
        class = ifelse(value, "status-yes", "status-no"),
        ifelse(value, "YES", "NO")
      )
    }

    summary_data <- shiny::reactive({
      nts <- nts_data()
      info_analyses <- info(nts$analyses)

      conn <- DBI::dbConnect(duckdb::duckdb(), nts$db)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      counts <- DBI::dbGetQuery(
        conn,
        "
        SELECT analysis,
               COUNT(*) AS total,
               SUM(CASE WHEN filtered THEN 1 ELSE 0 END) AS filtered
        FROM Features
        GROUP BY analysis
      "
      )

      if (nrow(counts) == 0) {
        counts <- data.frame(
          analysis = info_analyses$analysis,
          total = 0,
          filtered = 0
        )
      }

      counts$filtered[is.na(counts$filtered)] <- 0
      counts$not_filtered <- counts$total - counts$filtered
      counts$replicate <- info_analyses$replicate[match(counts$analysis, info_analyses$analysis)]

      total_groups_query <- DBI::dbGetQuery(
        conn,
        "
        SELECT COUNT(DISTINCT feature_group) AS n
        FROM Features
        WHERE NOT filtered AND feature_group != ''
      "
      )

      list(
        info = info_analyses,
        counts = counts,
        total_analyses = nrow(info_analyses),
        total_features = sum(counts$total, na.rm = TRUE),
        filtered_features = sum(counts$filtered, na.rm = TRUE),
        total_groups = ifelse(
          nrow(total_groups_query) == 0 || is.na(total_groups_query$n[1]),
          0,
          total_groups_query$n[1]
        ),
        has_eic = DBI::dbGetQuery(
          conn,
          "SELECT COUNT(*) AS n FROM Features WHERE eic_size > 0"
        )$n > 0,
        has_ms1 = DBI::dbGetQuery(
          conn,
          "SELECT COUNT(*) AS n FROM Features WHERE ms1_size > 0"
        )$n > 0,
        has_ms2 = DBI::dbGetQuery(
          conn,
          "SELECT COUNT(*) AS n FROM Features WHERE ms2_size > 0"
        )$n > 0
      )
    })

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

    output$features_chart <- plotly::renderPlotly({
      counts <- summary_data()$counts

      shiny::validate(
        shiny::need(nrow(counts) > 0, "No features available to plot.")
      )

      color_by <- chart_color_by()
      counts$color_by <- if (color_by == "replicates" && "replicate" %in% colnames(counts)) {
        counts$replicate
      } else {
        counts$analysis
      }

      plotly::plot_ly(
        data = counts,
        x = ~analysis,
        y = ~not_filtered,
        type = "bar",
        color = ~color_by,
        colors = "Set2",
        hovertemplate = paste(
          "Analysis: %{x}<br>",
          "Features: %{y}<br>",
          "Filtered: %{customdata}",
          "<extra></extra>"
        ),
        customdata = counts$filtered
      ) %>%
        plotly::layout(
          margin = list(l = 60, r = 40, t = 40, b = 60),
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
            title = list(text = ifelse(color_by == "replicates", "Replicate", "Analysis"))
          )
        ) %>%
        plotly::config(
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
    })

    features_data <- shiny::reactive({
      nts <- nts_data()
      get_features(nts, filtered = FALSE)
    })

    features_table_data <- shiny::reactive({
      fts <- data.table::as.data.table(features_data())

      if (nrow(fts) == 0) {
        return(fts)
      }

      fts$has_eic <- !is.na(fts$eic_size) & fts$eic_size > 0
      fts$has_ms1 <- !is.na(fts$ms1_size) & fts$ms1_size > 0
      fts$has_ms2 <- !is.na(fts$ms2_size) & fts$ms2_size > 0
      drop_cols <- intersect(
        c(
          "eic_rt",
          "eic_mz",
          "eic_intensity",
          "eic_baseline",
          "eic_smoothed",
          "ms1_mz",
          "ms1_intensity",
          "ms2_mz",
          "ms2_intensity"
        ),
        colnames(fts)
      )
      fts[, (drop_cols) := NULL]

      for (col in colnames(fts)) {
        if (is.numeric(fts[[col]][1])) {
          fts[[col]] <- round(fts[[col]], 4)
        }
      }

      fts$sel <- rep(FALSE, nrow(fts))
      data.table::setcolorder(
        fts,
        unique(c("sel", "analysis", "feature", "replicate", colnames(fts)))
      )
      fts
    })

    output$features_table <- DT::renderDT({
      fts <- features_table_data()

      if (nrow(fts) == 0) {
        return(DT::datatable(
          data.frame(Message = "No features available."),
          options = list(dom = "t", ordering = FALSE, paging = FALSE),
          style = "bootstrap",
          class = "table table-striped table-hover",
          rownames = FALSE
        ))
      }

      sel_col_index <- which(names(fts) == "sel") - 1

      DT::datatable(
        fts,
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
          lengthMenu = c(10, 25, 50, 100),
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

    shiny::observeEvent(input$deselect_all_features, {
      DT::selectRows(DT::dataTableProxy("features_table"), NULL)
    })

    output$export_features_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("features_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        table_data <- data.table::copy(features_table_data())
        if ("sel" %in% colnames(table_data)) table_data$sel <- NULL
        write.csv(table_data, file, row.names = FALSE)
      }
    )

    output$export_selected_features_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "selected_features_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        rows <- input$features_table_rows_selected
        table_data <- data.table::copy(features_table_data())
        if ("sel" %in% colnames(table_data)) table_data$sel <- NULL

        if (!is.null(rows) && length(rows) > 0) {
          write.csv(table_data[rows, ], file, row.names = FALSE)
        } else {
          write.csv(
            data.frame(Message = "No features selected"),
            file,
            row.names = FALSE
          )
        }
      }
    )

    selected_features <- shiny::reactive({
      rows <- input$features_table_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        return(NULL)
      }
      table_data <- features_table_data()
      data.table::data.table(
        analysis = table_data$analysis[rows],
        feature = table_data$feature[rows]
      )
    })

    shiny::observeEvent(input$remove_selected_features, {
      # TODO: implement feature removal against DB_MassSpecResults_NonTargetAnalysis backend
      shiny::showNotification(
        "Feature removal is not yet implemented for database-backed results.",
        type = "warning"
      )
    })

    output$feature_peaks_plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          !is.null(selected_features()),
          "Please select one or more features from the table to display the plot."
        )
      )

      nts <- nts_data()
      p <- plot_features(
        nts,
        features = selected_features(),
        filtered = FALSE,
        showDetails = TRUE
      )

      shiny::validate(
        shiny::need(
          !is.null(p),
          "No EIC data available for the selected features."
        )
      )

      plotly::layout(
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
        )
      ) %>%
        plotly::config(
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
    })

    selected_features_with_mass <- shiny::reactive({
      rows <- input$features_table_rows_selected
      features <- features_data()
      if (is.null(rows) || length(rows) == 0 || nrow(features) == 0) {
        return(NULL)
      }
      data.frame(
        mass = features$mass[rows],
        analysis = features$analysis[rows],
        feature = features$feature[rows]
      )
    })

    output$ms1_plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          !is.null(selected_features_with_mass()),
          "Please select one or more features from the table to display the plot."
        )
      )

      nts <- nts_data()
      p <- plot_features_ms1(nts, features = selected_features(), filtered = FALSE)

      shiny::validate(
        shiny::need(
          !is.null(p),
          "No MS1 data available for the selected features."
        )
      )

      plotly::layout(
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
      ) %>%
        plotly::config(
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
    })

    output$ms2_plot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(
          !is.null(selected_features_with_mass()),
          "Please select one or more features from the table to display the plot."
        )
      )

      nts <- nts_data()
      p <- plot_features_ms2(nts, features = selected_features(), filtered = FALSE)

      shiny::validate(
        shiny::need(
          !is.null(p),
          "No MS2 data available for the selected features."
        )
      )

      plotly::layout(
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
            text = "Intensity / Counts",
            font = list(size = 14, color = "#555")
          ),
          tickfont = list(size = 12),
          gridcolor = "#eee"
        )
      ) %>%
        plotly::config(
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
    })

    output$quality_table <- DT::renderDT({
      # TODO: expose quality metrics when available in DB_MassSpecResults_NonTargetAnalysis schema
      DT::datatable(
        data.frame(
          Message = "Quality metrics are not available for database-backed features yet."
        ),
        options = list(
          dom = "t",
          ordering = FALSE,
          paging = FALSE
        ),
        style = "bootstrap",
        rownames = FALSE
      )
    })
  })
}
