#' @noRd
S7::method(.mod_WorkflowAssembler_Result_UI, NTS) <- function(x, id, ns) {
  ns_full <- shiny::NS(paste0("WorkflowAssembler-", id))
  
  shiny::fluidRow(
    shinydashboard::tabBox(
      width = 12,
      shiny::tabPanel(
        title = "Overview",
        # 50/50 split
        shiny::fluidRow(
          # Left half
          shiny::column(
            width = 6,
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::div(
                  class = "info-box",
                  style = "background-color: #a8d1f0; color: #000; border-radius: 10px; padding: 15px; margin-bottom: 20px; height: 120px; position: relative;",
                  # Icon
                  shiny::div(
                    style = "position: absolute; top: 10px; left: 10px;",
                    shiny::icon("chart-line", style = "font-size: 16px;")
                  ),
                  shiny::div(
                    style = "font-size: 36px; text-align: center; font-weight: bold; margin-top: 17px;",
                    shiny::textOutput(ns_full("total_analyses"))
                  ),
                  shiny::div(
                    style = "text-align: center; font-size: 16px; margin-top: 7px;",
                    "Total Analysis"
                  )
                )
              ),
              shiny::column(
                width = 3,
                shiny::div(
                  class = "info-box",
                  style = "background-color: #a8d1f0; color: #000; border-radius: 10px; padding: 15px; margin-bottom: 20px; height: 120px; position: relative;",
                  # Icon
                  shiny::div(
                    style = "position: absolute; top: 10px; left: 10px;",
                    shiny::icon("gears", style = "font-size: 16px;")
                  ),
                  shiny::div(
                    style = "font-size: 36px; text-align: center; font-weight: bold; margin-top: 17px;",
                    shiny::textOutput(ns_full("total_features"))
                  ),
                  shiny::div(
                    style = "text-align: center; font-size: 16px; margin-top: 7px;",
                    "Total Features"
                  )
                )
              ),
              shiny::column(
                width = 3,
                shiny::div(
                  class = "info-box",
                  style = "background-color: #a8d1f0; color: #000; border-radius: 10px; padding: 15px; margin-bottom: 20px; height: 120px; position: relative;",
                  # Icon
                  shiny::div(
                    style = "position: absolute; top: 10px; left: 10px;",
                    shiny::icon("filter", style = "font-size: 16px;")
                  ),
                  shiny::div(
                    style = "font-size: 36px; text-align: center; font-weight: bold; margin-top: 17px;",
                    shiny::textOutput(ns_full("filtered_features_count"))
                  ),
                  shiny::div(
                    style = "text-align: center; font-size: 16px; margin-top: 7px;",
                    "Filtered Features Count"
                  )
                )
              ),
              shiny::column(
                width = 3,
                shiny::div(
                  class = "info-box",
                  style = "background-color: #a8d1f0; color: #000; border-radius: 10px; padding: 15px; margin-bottom: 20px; height: 120px; position: relative;",
                  # Icon
                  shiny::div(
                    style = "position: absolute; top: 10px; left: 10px;",
                    shiny::icon("network-wired", style = "font-size: 16px;")
                  ),
                  shiny::div(
                    style = "font-size: 36px; text-align: center; font-weight: bold; margin-top: 17px;",
                    shiny::textOutput(ns_full("total_groups"))
                  ),
                  shiny::div(
                    style = "text-align: center; font-size: 16px; margin-top: 7px;",
                    "Total Groups"
                  )
                )
              )
            )
          ),
          # Right half
          shiny::column(
            width = 6,
            shiny::div(
              style = "background-color: #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::tags$div(
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has Filtered Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_filtered_features"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has Groups?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_groups"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has EIC Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features_eic"), inline = TRUE))
                    )
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::tags$div(
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has MS1 Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features_ms1"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has MS2 Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features_ms2"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has Suspect Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features_suspects"), inline = TRUE))
                    )
                  )
                )
              )
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Features Count", style = "margin-left: 15px; margin-bottom: 10px;")
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns_full("features_chart"), height = "600px")
          )
        )
      ),
      
      # Features tab
      shiny::tabPanel(
        title = "Features",
        # Table
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              style = "margin-bottom: 20px;",
              DT::dataTableOutput(ns_full("features_table"))
            )
          )
        ),
        # Plot
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Feature Peaks", style = "margin-left: 15px; margin-top: 0px; margin-bottom: 10px;"),
            plotly::plotlyOutput(ns_full("feature_peaks_plot"), height = "300px", width = "50%")
          )
        )
      ),
      
      # Other tabs
      shiny::tabPanel(
        title = "Groups",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            DT::dataTableOutput(ns_full("groups_table"))
          )
        )
      ),
      shiny::tabPanel(
        title = "Subjects",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Subject data will be displayed here")
          )
        )
      ),
      shiny::tabPanel(
        title = "Fold Change",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Fold change data will be displayed here")
          )
        )
      )
    )
  )
}

#' @noRd
S7::method(.mod_WorkflowAssembler_Result_Server, NTS) <- function(x,
                                                                  id,
                                                                  ns,
                                                                  reactive_analyses,
                                                                  reactive_volumes,
                                                                  reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive values to store NTS data
    nts_data <- shiny::reactive({
      shiny::validate(need(!is.null(x), "NTS data is not available"))
      x
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
    
    # Status indicators
    output$has_features <- shiny::renderText({
      ifelse(nts_data()@has_features, "YES", "NO")
    })
    
    output$has_filtered_features <- shiny::renderText({
      ifelse(nts_data()@has_filtered_features, "YES", "NO")
    })
    
    output$has_groups <- shiny::renderText({
      ifelse(nts_data()@has_groups, "YES", "NO")
    })
    
    output$has_features_eic <- shiny::renderText({
      ifelse(nts_data()@has_features_eic, "YES", "NO")
    })
    
    output$has_features_ms1 <- shiny::renderText({
      ifelse(nts_data()@has_features_ms1, "YES", "NO")
    })
    
    output$has_features_ms2 <- shiny::renderText({
      ifelse(nts_data()@has_features_ms2, "YES", "NO")
    })
    
    output$has_features_suspects <- shiny::renderText({
      ifelse(nts_data()@has_features_suspects, "YES", "NO")
    })
    
    # Features chart using plot_features_count
    output$features_chart <- plotly::renderPlotly({
      nts <- nts_data()
      plot_features_count(nts, colorBy = "replicates")
    })
    
    # Features table for the Features tab
    output$features_table <- DT::renderDT({
      # Fetch features data
      features <- get_features(nts_data())
      
      # Remove nested columns
      nested_cols <- c("eic", "ms1", "ms2", "quality", "annotation", "istd", "suspects", "formulas", "compounds")
      features <- features[, !nested_cols, with = FALSE]
      
      # Round numeric columns to 4 decimal places for readability
      numeric_cols <- c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "intensity", "area", "mass", "suppression_factor")
      for (col in numeric_cols) {
        features[[col]] <- round(features[[col]], 4)
      }
      
      # Render the DataTable
      DT::datatable(
        features,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          scrollY = "250px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(width = "200px", targets = c(0, 1, 2)),
            list(width = "100px", targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11)),
            list(width = "80px", targets = c(12)),
            list(width = "100px", targets = c(13)),
            list(width = "80px", targets = c(14, 15, 16)),
            list(width = "150px", targets = c(17)),
            list(width = "80px", targets = c(18)),
            list(width = "120px", targets = c(19)),
            list(className = "dt-right", targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17)),
            list(className = "dt-left", targets = c(0, 1, 2, 13, 14, 15, 16, 18, 19))
          ),
          selection = list(mode = "multiple", selected = NULL),
          dom = "lfrtip",
          lengthMenu = c(5, 10, 25, 50, 100),
          ordering = TRUE,
          searching = TRUE  # Search bar
        ),
        style = "bootstrap",
        class = "table table-bordered table-hover",
      ) %>%
        # Custom CSS
        DT::formatStyle(
          columns = names(features),
          fontSize = "14px",
          padding = "5px 10px"
        )
    })
    
    # Reactive value to store selected features
    selected_features <- shiny::reactive({
      selected_rows <- input$features_table_rows_selected
      
      # Fetch features data
      features <- get_features(nts_data())
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(NULL)
      }
      
      # data frame with analysis and feature columns for the selected rows
      selected_data <- features[selected_rows, .(analysis, feature)]
      
      # Convert to data frame
      as.data.frame(selected_data)
    })
    
    # feature peaks plot
    output$feature_peaks_plot <- plotly::renderPlotly({
      shiny::validate(
        need(!is.null(selected_features()), "Please select one or more features from the table to display the plot.")
      )
      
      # Generate the plot using plot_features
      nts <- nts_data()
      plot_features(nts, features = selected_features())
    })
  })
}