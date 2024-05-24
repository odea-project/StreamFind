#' .make_app_server
#' 
#' @description Creates a Shiny app server for a given engine type and save file.
#' 
#' @param engine_type Character (length 1) with the engine type.
#' @param engine_save_file Character (length 1) with the engine save file.
#' 
#' @noRd
#'  
.make_app_server <- function(engine_type, engine_save_file) {

  server <- function(input, output, session) {
    
    # _Utility functions -----
    
    .add_notifications <- function(warnings, name_msg, msg) {
      showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }
    
    .remove_notifications <- function(warnings, name_msgs) {
      warnings[name_msgs] <- NULL
      return(warnings)
    }
    
    .wrap_analyses_ui_in_divs <- function(elements) {
      lapply(elements, function(x) {
        div(style = sprintf("min-width: %dpx; height: %dpx; display: flex; align-items: center;", 40, 40),x)
      })
    }
    
    .update_analyses_replicate_blank_names <- function(replicates, blanks) {
      tryCatch({
        engine$add_replicate_names(replicates)
        engine$add_blank_names(blanks)
        reactive_overview_analyses(engine$get_overview())
        warnings <- reactive_warnings()
        warnings[["analyses_needs_update"]] <- NULL
        reactive_warnings(warnings)
      }, warning = function(w) {
        showNotification(conditionMessage(w), duration = 10, type = "warning")
      }, error = function(e) {
        showNotification(conditionMessage(e), duration = 10, type = "error")
      })
    }
    
    # _Constants -----
    engine_call <- get(engine_type, envir = .GlobalEnv)
    engine_call_new <- engine_call[["new"]]
    engine <- suppressMessages(do.call(engine_call_new, list()))
    engine$load(engine_save_file)
    wdir <- getwd()
    save_file <- engine$save_file
    initial_engine_history <- engine$history
    mandatory_header_names <- c("name", "author", "file", "date")
  
    message("Running Shiny app for ", engine_type, "... ")
  
    # _Reactive values -----
    reactive_headers <- reactiveVal(engine$get_headers())
    reactive_overview_analyses <- reactiveVal(engine$get_overview())
    reactive_warnings <- reactiveVal(list())
    reactive_history <- reactiveVal(engine$history)
    reactive_saved_history <- reactiveVal(initial_engine_history)
    
    # _Global observers -----
    
    ## obs Unsaved changes -----
    observe({
      if (!identical(reactive_history(), reactive_saved_history()) && !"unsaved_changes" %in% names(reactive_warnings())) {
        reactive_warnings(.add_notifications(reactive_warnings(), "unsaved_changes", "Unsaved changes in the engine!"))
      }
    })
  
    # _Outputs and Events -----
    
    ## out Warnings menu -----
    output$warningMenu <- renderMenu({
      warnings <- reactive_warnings()
      msgs <- lapply(warnings, function(x) { notificationItem(text = x) })
      dropdownMenu(type = "notifications", .list = msgs)
    })
    
    ## out Save engine -----
    output$save_engine <- renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        div(style = "margin-bottom: 20px;", actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger"))
      }
    })
    
    ### event Save -----
    observeEvent(input$save_engine_button, {
      engine$save(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview_analyses(engine$get_overview())
      reactive_saved_history(engine$history)
      reactive_history(engine$history)
    })
    
    ## out Reset engine -----
    output$reset_engine <- renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        div(style = "margin-bottom: 20px;", actionButton("reset_engine_button", label = "Discard Changes", width = 200, class = "btn-danger"))
      }
    })
    
    ### event Reset -----
    observeEvent(input$reset_engine_button, {
      engine$load(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview_analyses(engine$get_overview())
      reactive_saved_history(engine$history)
      reactive_history(engine$history)
    })
  
    ## _Overview -----
    
    ### out Info -----
    output$wdir <- renderUI({ HTML(paste("<b>Working directory:</b>", wdir)) })
    
    ### out Headers -----
    output$headers <- renderUI({
      headers <- reactive_headers()
      lapply(names(headers), function(name) {
        if (name %in% mandatory_header_names) {
          div(tags$b(name), ": ", headers[[name]], br())
        } else {
          id <- paste0("button_header_del_", name)
          observeEvent(input[[id]], {
            headers <- reactive_headers()
            headers[[name]] <- NULL
            reactive_headers(headers)
          }, ignoreInit = TRUE)
          div(
            actionButton(id, label = NULL, icon = icon("trash"), width = '40px'), #button_ids[[name]]
            tags$b(name), ": ", headers[[name]], br()
          )
        }
      })
    })
    
    #### event Add headers -----
    observeEvent(input$add_header_button, {
      if (input$new_header_name != "" && input$new_header_value != "") {
        headers <- reactive_headers()
        headers[[input$new_header_name]] <- input$new_header_value
        reactive_headers(headers)
        updateTextInput(session, "new_header_name", value = "")
        updateTextInput(session, "new_header_value", value = "")
      }
    })
    
    #### obs Update headers -----
    observe({
      reactive_header_names <- names(reactive_headers())
      engine_header_names <- names(engine$headers)
      if (!identical(reactive_header_names, engine_header_names)) {
        headers_to_remove <- engine_header_names[!engine_header_names %in% reactive_header_names]
        headers_to_add <- reactive_header_names[!reactive_header_names %in% engine_header_names]
        if (length(headers_to_remove) > 0) engine$remove_headers(headers_to_remove)
        if (length(headers_to_add) > 0) engine$add_headers(reactive_headers()[headers_to_add])
        reactive_history(engine$history)
      }
    })
    
    ### out Analyses -----
    output$overview_analyses <- renderUI({
      analyses <- reactive_overview_analyses()
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
          
          #### event Remove analysis -----
          observeEvent(input[[button_id]], {
            analyses <- reactive_overview_analyses()
            analyses <- analyses[analyses$analysis != name, ]
            reactive_overview_analyses(analyses)
          }, ignoreInit = TRUE)
          
          #### event Update analyses -----
          observeEvent(input[[text_rpl_id]], {
            if (!(input[[text_rpl_id]] %in% rpl) && !("analyses_needs_update" %in% names(reactive_warnings()))) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
            }
          })
          
          #### event Update analyses -----
          observeEvent(input[[text_blk_id]], {
            if (!(input[[text_blk_id]] %in% blk) && !("analyses_needs_update" %in% names(reactive_warnings()))) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
            }
          })
          
          list(
            actionButton(button_id, label = NULL, icon = icon("trash"), width = '40px'),
            tags$b(name),
            textInput(text_rpl_id, label = NULL, value = analyses$replicate[i], width = "100%"),
            selectInput(text_blk_id, label = NULL, choices = replicates, selected = analyses$blank[i], width = "100%")
          )
        })
        
        ui_elements <- c(ui_labels, ui_elements)
        
        tagList(
          fluidRow(
            column(1, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[1]]))),
            column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[2]]))),
            column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[3]]))),
            column(3, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[4]])))
          )
        )
      }
    })
    
    #### event Add analyses -----
    observeEvent(input$add_analyses_button, {
      files <- utils::choose.files(default = wdir)
      number_files <- length(files)
      if (number_files > 0) {
        output$loading_spinner <- renderUI({ div(style = "height: 100px; width: 100px;") })
        withProgress(message = 'Loading files...', value = 0, {
          for (i in seq_len(number_files)) {
            tryCatch({
              engine$add_files(files[i])
            }, warning = function(w) {
              msg <- paste("Warning for", files[i], ":", conditionMessage(w))
              showNotification(msg, duration = 10, type = "warning")
            }, error = function(e) {
              msg <- paste("Error for", files[i], ":", conditionMessage(e))
              showNotification(msg, duration = 10, type = "error")
            })
            incProgress(i/number_files)
          }
        })
        output$loading_spinner <- renderUI({ NULL })
        reactive_overview_analyses(engine$get_overview())
        reactive_history(engine$history)
      }
    })
    
    #### event Update analyses -----
    observeEvent(input$update_analyses_button, {
      replicates <- vapply(reactive_overview_analyses()$analysis, function(name) input[[paste0("set_replicate_name_", name)]], NA_character_)
      replicates[replicates == "NA"] <- NA_character_
      blanks <- vapply(reactive_overview_analyses()$analysis, function(name) input[[paste0("set_blank_name_", name)]], NA_character_)
      blanks[blanks == "NA"] <- NA_character_
      if (engine$has_results()) {
        showModal(modalDialog(
          title = "Attention",
          "Modifying the replicate or blank names removes any results in the engine! Do you want to proceed with the operation?",
          footer = tagList(
            modalButton("No"),
            actionButton("confirm_yes", "Yes")
          )
        ))
        observeEvent(input$confirm_yes, {
          removeModal()
          .update_analyses_replicate_blank_names(replicates, blanks)
          reactive_history(engine$history)
        })
      } else {
        .update_analyses_replicate_blank_names(replicates, blanks)
        reactive_history(engine$history)
      }
    })
    
    #### event Reset analyses -----
    observeEvent(input$reset_analyses_button, {
      analyses <- reactive_overview_analyses()
      analyses$blank[is.na(analyses$blank)] <- "NA"
      number_analyses <- nrow(analyses)
      lapply(seq_len(number_analyses), function(i) {
        updateTextInput(session, paste0("set_replicate_name_", analyses$analysis[i]), value = analyses$replicate[i])
        updateSelectInput(session, paste0("set_blank_name_", analyses$analysis[i]), selected = analyses$blank[i])
      })
      warnings <- reactive_warnings()
      warnings[["analyses_needs_update"]] <- NULL
      reactive_warnings(warnings)
    })
    
    ### obs Removes analyses -----
    observe({
      reactive_analyses <- reactive_overview_analyses()$analysis
      engine_analyses <- engine$get_overview()$analysis
      if (!identical(reactive_analyses, engine_analyses)) {
        analyses_to_remove <- engine_analyses[!engine_analyses %in% reactive_analyses]
        if (length(analyses_to_remove) > 0) engine$remove_analyses(analyses_to_remove)
        reactive_history(engine$history)
      }
    })
    
    ## _Explorer -----
    
    ### out Explorer -----
    output$explorer_ui <- renderUI({
      if (engine_type %in% "MassSpecEngine") {
        .mod_MassSpecEngine_summary_Server("summary", engine, reactive_overview_analyses)
        .mod_MassSpecEngine_summary_UI("summary", engine)
        
      } else if (engine_type %in% "RamanEngine") {
        .mod_RamanEngine_summary_Server("summary", engine, reactive_overview_analyses)
        .mod_RamanEngine_summary_UI("summary", engine)
        
      } else {
        div("Explorer not implemented for engine type ", engine_type)
      }
    })
    
    ## _History -----
    output$"history_table" <- renderDataTable({
      h_list <- reactive_history()
      rbindlist(h_list, fill = TRUE)
    })
  }

  return(server)
}