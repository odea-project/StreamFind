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
    
    # Utility functions -----
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
        # warnings <- reactive_warnings()
        # warnings[["analyses_needs_update"]] <- NULL
        # reactive_warnings(warnings)
      }, warning = function(w) {
        showNotification(conditionMessage(w), duration = 10, type = "warning")
      }, error = function(e) {
        showNotification(conditionMessage(e), duration = 10, type = "error")
      })
    }
    
    # Constants -----
    engine_call <- get(engine_type, envir = .GlobalEnv)
    engine_call_new <- engine_call[["new"]]
    engine <- suppressMessages(do.call(engine_call_new, list()))
    engine$load(engine_save_file)
    wdir <- getwd()
    save_file <- engine$save_file
    initial_engine_history <- engine$history
  
    message("Running Shiny app for ", engine_type, "... ")
  
    # Reactive values -----
    
    reactive_headers <- reactiveVal(engine$get_headers())
    reactive_overview_analyses <- reactiveVal(engine$get_overview())
    reactive_warnings <- reactiveVal(list())
    reactive_history <- reactiveVal(engine$history)
    reactive_saved_history <- reactiveVal(initial_engine_history)
    reactive_del_buttons_headers <- reactiveVal(list())
    
    ## Reactive observers -----
    
    ### Update engine headers -----
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
    
    ### Removes engine analyses -----
    observe({
      reactive_analyses <- reactive_overview_analyses()$analysis
      engine_analyses <- engine$get_overview()$analysis
      if (!identical(reactive_analyses, engine_analyses)) {
        analyses_to_remove <- engine_analyses[!engine_analyses %in% reactive_analyses]
        if (length(analyses_to_remove) > 0) engine$remove_analyses(analyses_to_remove)
        reactive_history(engine$history)
      }
    })
    
    ### Check for unsaved changes -----
    observe({
      # browser()
      # if (!identical(reactive_history(), reactive_saved_history()) && !"unsaved_changes" %in% names(reactive_warnings())) {
      #   reactive_warnings(.add_notifications(reactive_warnings(), "unsaved_changes", "Unsaved changes in the engine!"))
      # }
      # browser()
    })
  
    # Outputs -----
    
    ## Menus -----
    output$warningMenu <- renderMenu({
      warnings <- reactive_warnings()
      msgs <- lapply(warnings, function(x) { notificationItem(text = x) })
      dropdownMenu(type = "notifications", .list = msgs)
    })
  
    ## Overview -----
    
    ### Info block -----
    output$wdir <- renderUI({ HTML(paste("<b>Working directory:</b>", wdir)) })
    
    output$save_engine <- renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        div(style = "margin-bottom: 20px;", actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger"))
      }
    })
    
    output$reset_engine <- renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        div(style = "margin-bottom: 20px;", actionButton("reset_engine_button", label = "Discard Changes", width = 200, class = "btn-danger"))
      }
    })
    
    #### Save event -----
    observeEvent(input$save_engine_button, {
      engine$save(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview_analyses(engine$get_overview())
      reactive_saved_history(engine$history)
      reactive_history(engine$history)
    })
    
    #### Reset event -----
    observeEvent(input$reset_engine_button, {
      engine$load(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview_analyses(engine$get_overview())
      reactive_saved_history(engine$history)
      reactive_history(engine$history)
    })
    
    ### Headers -----
    output$headers <- renderUI({
      
      browser()
      
      headers <- reactive_headers()
      time_stamp <- format(Sys.time(), "%Y%m%d%H%M%OS4")
      time_stamp <- gsub("\\.", "", time_stamp)
      button_ids <- lapply(names(headers), function(name) paste0("remove_", name, "_", time_stamp))
      names(button_ids) <- names(headers)
      
      if (!all(names(headers) %in% c("name", "author", "file", "date"))) {
        reactive_del_buttons_headers(button_ids)
      }
      
      browser()
      
      lapply(names(headers), function(name) {
        if (name %in% c("name", "author", "file", "date")) {
          div(tags$b(name), ": ", headers[[name]], br())
        } else {
          div(
            actionButton(button_ids[[name]], label = NULL, icon = icon("trash"), width = '40px'),
            tags$b(name), ": ", headers[[name]], br()
          )
        }
      })
    })
    
    #### Delete event -----
    observeEvent(names(reactive_headers()), {
      browser()
      lapply(names(reactive_headers()), function(name) {
        if (!(name %in% c("name", "author", "file", "date"))) {
          browser()
          observeEvent(input[[reactive_del_buttons_headers()[[name]]]], {
            headers <- reactive_headers()
            headers[[name]] <- NULL
            reactive_headers(headers)
          })
        }
      })
      browser()
    })
    
    #### Add event -----
    observeEvent(input$add_header_button, {
      if (input$new_header_name != "" && input$new_header_value != "") {
        headers <- reactive_headers()
        headers[[input$new_header_name]] <- input$new_header_value
        reactive_headers(headers)
        updateTextInput(session, "new_header_name", value = "")
        updateTextInput(session, "new_header_value", value = "")
      }
    })
    
    ### Analyses -----
    output$overview_analyses <- renderUI({
      analyses <- reactive_overview_analyses()
      number_analyses <- nrow(analyses)
      if (number_analyses > 0) {
        replicates <- unique(c(analyses$replicate, analyses$blank))
        ui_labels <- list(labels = c("   ", "Analysis", "Replicate", "Blank"))
        ui_elements <- lapply(seq_len(number_analyses), function(i) {
          name <- analyses$analysis[i]
          list(
            actionButton(paste0("remove_", name), label = NULL, icon = icon("trash"), width = '40px'),
            tags$b(name),
            textInput(paste0("replicate_", name), label = NULL, value = analyses$replicate[i], width = "100%"),
            selectInput(paste0("blank_", name), label = NULL, choices = replicates, selected = analyses$blank[i], width = "100%")
          )
        })
        ui_elements <- c(ui_labels, ui_elements)
        tagList(
          fluidRow(
            column(1, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[1]]))),  # Remove button
            column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[2]]))),  # Text output
            column(4, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[3]]))),  # Text input
            column(3, .wrap_analyses_ui_in_divs(lapply(ui_elements, function(x) x[[4]])))   # Select input
          )
        )
      }
    })
    
    #### Delete and modified event -----
    # observeEvent(reactive_overview_analyses()$analysis, {
    #   reactive_replicates <- reactive_overview_analyses()$replicate
    #   names(reactive_replicates) <- reactive_overview_analyses()$analysis
    #   reactive_blanks <- reactive_overview_analyses()$blank
    #   reactive_blanks[is.na(reactive_blanks)] <- "NA"
    #   names(reactive_blanks) <- reactive_overview_analyses()$analysis
    #   
    #   lapply(reactive_overview_analyses()$analysis, function(name) {
    #     
    #     observeEvent(input[[paste0("remove_", name)]], {
    #       analyses <- reactive_overview_analyses()
    #       analyses <- analyses[analyses$analysis != name, ]
    #       reactive_overview_analyses(analyses)
    #       # removeUI(selector = paste0("remove_", "name"))
    #     })
    # 
    #     observeEvent(input[[paste0("replicate_", name)]], {
    #       if (!is.null(input[[paste0("replicate_", name)]])) {
    #         if (input[[paste0("replicate_", name)]] != reactive_replicates[name] && !"analyses_needs_update" %in% names(reactive_warnings())) {
    #           msg <- "Engine analyses are not updated!"
    #           reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
    #         }
    #       }
    #     })
    # 
    #     observeEvent(input[[paste0("blank_", name)]], {
    #       if (!is.null(input[[paste0("blank_", name)]])) {
    #         if (!(input[[paste0("blank_", name)]] %in% reactive_blanks[name]) && !("analyses_needs_update" %in% names(reactive_warnings()))) {
    #           msg <- "Engine analyses are not updated!"
    #           reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
    #         }
    #       }
    #     })
    #   })
    # })
    
    observe({
      # Update the reactive_replicates and reactive_blanks whenever reactive_overview_analyses()$analysis changes
      analyses <- reactive_overview_analyses()$analysis
      reactive_replicates <- reactive_overview_analyses()$replicate
      names(reactive_replicates) <- analyses
      reactive_blanks <- reactive_overview_analyses()$blank
      reactive_blanks[is.na(reactive_blanks)] <- "NA"
      names(reactive_blanks) <- analyses
      
      lapply(analyses, function(name) {
        # Observer for the remove button
        observe({
          
          browser()
          
          if (reactive_delete_analysis_bottum_counter[[name]] < input[[paste0("remove_", name)]]) {
            analyses <- reactive_overview_analyses()
            analyses <- analyses[analyses$analysis != name, ]
            reactive_overview_analyses(analyses)
            reactive_delete_analysis_bottum_counter[[name]] <- input[[paste0("remove_", name)]]
          }
          
        }) %>% bindEvent(input[[paste0("remove_", name)]], ignoreNULL = TRUE)
        
        # Observer for the replicate input
        observe({
          if (!is.null(input[[paste0("replicate_", name)]])) {
            if (input[[paste0("replicate_", name)]] != reactive_replicates[name] &&
                !"analyses_needs_update" %in% names(reactive_warnings())) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
            }
          }
        }) %>% bindEvent(input[[paste0("replicate_", name)]], ignoreNULL = TRUE)
        
        # Observer for the blank input
        observe({
          if (!is.null(input[[paste0("blank_", name)]])) {
            if (!(input[[paste0("blank_", name)]] %in% reactive_blanks[name]) &&
                !"analyses_needs_update" %in% names(reactive_warnings())) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_needs_update", msg))
            }
          }
        }) %>% bindEvent(input[[paste0("blank_", name)]], ignoreNULL = TRUE)
      })
    })
    
    #### Add event -----
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
    
    #### Update event -----
    observeEvent(input$update_analyses_button, {
      replicates <- vapply(reactive_overview_analyses()$analysis, function(name) input[[paste0("replicate_", name)]], NA_character_)
      replicates[replicates == "NA"] <- NA_character_
      blanks <- vapply(reactive_overview_analyses()$analysis, function(name) input[[paste0("blank_", name)]], NA_character_)
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
    
    #### Reset event -----
    observeEvent(input$reset_analyses_button, {
      analyses <- reactive_overview_analyses()
      analyses$blank[is.na(analyses$blank)] <- "NA"
      number_analyses <- nrow(analyses)
      lapply(seq_len(number_analyses), function(i) {
        updateTextInput(session, paste0("replicate_", analyses$analysis[i]), value = analyses$replicate[i])
        updateSelectInput(session, paste0("blank_", analyses$analysis[i]), selected = analyses$blank[i])
      })
      warnings <- reactive_warnings()
      warnings[["analyses_needs_update"]] <- NULL
      reactive_warnings(warnings)
    })
  }

  return(server)
}