#' @title .mod_analyses_UI
#' 
#' @description Shiny module UI for analyses tab.
#' 
#' @noRd
#'
.mod_analyses_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::column(12,
    shinydashboard::box(title = "Analyses", width = 12, solidHeader = TRUE, shiny::uiOutput(ns("overview_analyses"))),
    shiny::column(width = 12, shiny::uiOutput(ns("analyses_overview_buttons")))
  )
}

#' @title .mod_analyses_Server
#' 
#' @description Shiny module server for analyses tab.
#' 
#' @noRd
#' 
.mod_analyses_Server <- function(id, engine, reactive_files, reactive_overview, reactive_warnings, reactive_history, volumes, file_types) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    .add_notifications <- function(warnings, name_msg, msg) {
      shiny::showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }
    
    .wrap_analyses_ui_in_divs <- function(elements) {
      lapply(elements, function(x) {
        htmltools::div(style = sprintf("min-width: %dpx; height: %dpx; display: flex; align-items: center;", 40, 40),x)
      })
    }
    
    check_overView_state <- shiny::reactiveVal(FALSE)
    
    output$overview_analyses <- shiny::renderUI({
      analyses <- reactive_overview()
      number_analyses <- nrow(analyses)
      if (number_analyses > 0) {
        replicates <- unique(c(analyses$replicate, analyses$blank))
        ui_labels <- list(labels = c("   ", "Analysis", "Replicate", "Blank"))
        ui_elements <- lapply(seq_len(number_analyses), function(i) {
          name <- analyses$analysis[i]
          rpl <- analyses$replicate[i]
          blk <- analyses$blank[i]
          if (is.na(blk)) blk <- "NA"
          button_id <- paste0("del_analysis_", name)
          text_rpl_id <- paste0("set_replicate_name_", name)
          text_blk_id <- paste0("set_blank_name_", name)
          
          ui_element <- list(
            shiny::actionButton(ns(button_id), label = NULL, icon = shiny::icon("trash"), width = '40px'),
            htmltools::tags$b(name),
            shiny::textInput(ns(text_rpl_id), label = NULL, value = analyses$replicate[i], width = "100%"),
            shiny::selectInput(ns(text_blk_id), label = NULL, choices = replicates, selected = analyses$blank[i], width = "100%")
          )
          
          #### event Remove analysis -----
          shiny::observeEvent(input[[button_id]], {
            analyses <- reactive_overview()
            analyses <- analyses[analyses$analysis != name, ]
            reactive_overview(analyses)
          }, ignoreInit = TRUE)
          
          #### event Update analyses -----
          shiny::observeEvent(input[[text_rpl_id]], {
            if (!(input[[text_rpl_id]] %in% rpl) && !("analyses_overview_needs_update" %in% names(reactive_warnings()))) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_overview_needs_update", msg))
            }
          }, ignoreInit = TRUE, once = FALSE)
          
          #### event Update analyses -----
          shiny::observeEvent(input[[text_blk_id]], {
            if (!(input[[text_blk_id]] %in% blk) && !("analyses_overview_needs_update" %in% names(reactive_warnings()))) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_overview_needs_update", msg))
            }
          }, ignoreInit = TRUE, once = FALSE)
          
          ui_element
        })
        ui_elements <- c(ui_labels, ui_elements)
        htmltools::tagList(
          shiny::fluidRow(
            shiny::column(1, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[1]]))),
            shiny::column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[2]]))),
            shiny::column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[3]]))),
            shiny::column(3, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[4]])))
          )
        )
      }
    })
    
    output$analyses_overview_buttons <- shiny::renderUI({
      if ("analyses_overview_needs_update" %in% names(reactive_warnings())) {
        analyses <- reactive_overview()
        analyses$blank[is.na(analyses$blank)] <- "NA"
        number_analyses <- nrow(analyses)
        equal_rpl <- all(vapply(seq_len(number_analyses), function(i) input[[paste0("set_replicate_name_", analyses$analysis[i])]] == analyses$replicate[i], FALSE))
        equal_blk <- all(vapply(seq_len(number_analyses), function(i) input[[paste0("set_blank_name_", analyses$analysis[i])]] == analyses$blank[i], FALSE))
        
        if (equal_rpl && equal_blk) {
          htmltools::div(style = "margin-bottom: 20px;",
            shinyFiles::shinyFilesButton(ns("add_analyses_button"), "Add Analysis Files", "Select Analysis Files", multiple = TRUE)
          )
          lapply(seq_len(number_analyses), function(i) {
            shiny::updateTextInput(session, paste0("set_replicate_name_", analyses$analysis[i]), value = analyses$replicate[i])
            shiny::updateSelectInput(session, paste0("set_blank_name_", analyses$analysis[i]), selected = analyses$blank[i])
          })
          warnings <- reactive_warnings()
          warnings[["analyses_overview_needs_update"]] <- NULL
          reactive_warnings(warnings)
          
        } else {
          htmltools::div(style = "margin-bottom: 20px;",
            shinyFiles::shinyFilesButton(ns("add_analyses_button"), "Add Analysis Files", "Select Analysis Files", multiple = TRUE),
            shiny::actionButton(ns("update_analyses_button"), label = "Update Analyses", width = 200, class = "btn-danger"),
            shiny::actionButton(ns("reset_analyses_button"), label = "Discard Changes", width = 200, class = "btn-danger")
          )
        }
        
      } else {
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton(ns("add_analyses_button"), "Add Analysis Files", "Select Analysis Files", multiple = TRUE)
        )
      }
    })
    
    shinyFiles::shinyFileChoose(input, "add_analyses_button", roots = volumes, defaultRoot = "wd", session = session, filetypes = file_types)
    
    shiny::observeEvent(input$add_analyses_button, {
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$add_analyses_button)
      if (nrow(fileinfo) > 0) {
        files <- fileinfo$datapath
        number_files <- length(files)
        if (number_files > 0) {
          reactive_files(c(unname(reactive_files()), files))
        }
      }
    })
    
    shiny::observeEvent(input$update_analyses_button, {
      replicates <- vapply(reactive_overview()$analysis, function(name) input[[paste0("set_replicate_name_", name)]], NA_character_)
      replicates[replicates == "NA"] <- NA_character_
      blanks <- vapply(reactive_overview()$analysis, function(name) input[[paste0("set_blank_name_", name)]], NA_character_)
      blanks[blanks == "NA"] <- NA_character_
      overview <- reactive_overview()
      if (engine$has_results()) {
        shiny::showModal(shiny::modalDialog(title = "Attention",
          "Modifying the replicate or blank names removes any results in the engine! Do you want to proceed with the operation?",
          footer = htmltools::tagList(shiny::modalButton("No"), shiny::actionButton(ns("confirm_yes"), "Yes"))
        ))
        shiny::observeEvent(input$confirm_yes, {
          overview$replicate <- replicates
          overview$blank <- blanks
          reactive_overview(overview)
          warnings <- reactive_warnings()
          warnings[["analyses_overview_needs_update"]] <- NULL
          reactive_warnings(warnings)
          reactive_history(engine$history)
        })
      } else {
        overview$replicate <- replicates
        overview$blank <- blanks
        reactive_overview(overview)
        warnings <- reactive_warnings()
        warnings[["analyses_overview_needs_update"]] <- NULL
        reactive_warnings(warnings)
        reactive_history(engine$history)
      }
    })
    
    shiny::observeEvent(input$reset_analyses_button, {
      analyses <- reactive_overview()
      analyses$blank[is.na(analyses$blank)] <- "NA"
      number_analyses <- nrow(analyses)
      lapply(seq_len(number_analyses), function(i) {
        shiny::updateTextInput(session, paste0("set_replicate_name_", analyses$analysis[i]), value = analyses$replicate[i])
        shiny::updateSelectInput(session, paste0("set_blank_name_", analyses$analysis[i]), selected = analyses$blank[i])
      })
      warnings <- reactive_warnings()
      warnings[["analyses_overview_needs_update"]] <- NULL
      reactive_warnings(warnings)
    })
  })
}