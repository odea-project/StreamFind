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
          # Right half: Grey box
          shiny::column(
            width = 6,  #  50% of the row
            shiny::div(
              style = "background-color: #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
              shiny::fluidRow(
                # Left column
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
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_features_eic"), inline = TRUE))
                    ),
                    shiny::tags$div(
                      style = "margin-bottom: 10px;",
                      shiny::tags$span("Has EIC Features?", style = "font-weight: normal;"),
                      shiny::tags$span(style = "float: right; font-weight: bold;", shiny::textOutput(ns_full("has_groups"), inline = TRUE))
                    )
                  )
                ),
                # Right column
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

        # chart title
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h2("Features Count", style = "margin-left: 15px; margin-bottom: 10px; font-size: 36px; font-weight: bold;")
          )
        ),
        
        # feature count plot
        shiny::fluidRow(
          shiny::column(
            width = 12,
            plotly::plotlyOutput(ns_full("features_chart"), height = "600px")
          )
        )
      ),
      
      # tabs
      shiny::tabPanel(
        title = "Features",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            DT::dataTableOutput(ns_full("features_table"))
          )
        )
      ),
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
    
    # Calculate summary metrics
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
    
    # Yes/No
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

  })
}