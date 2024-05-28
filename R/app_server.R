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
      shiny::showNotification(msg, duration = 5, type = "warning")
      warnings[[name_msg]] <- msg
      return(warnings)
    }
    
    .remove_notifications <- function(warnings, name_msgs) {
      warnings[name_msgs] <- NULL
      return(warnings)
    }
    
    .wrap_analyses_ui_in_divs <- function(elements) {
      lapply(elements, function(x) {
        htmltools::div(style = sprintf("min-width: %dpx; height: %dpx; display: flex; align-items: center;", 40, 40),x)
      })
    }
    
    .get_volumes <- function() {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        drives <- system("wmic logicaldisk get name", intern = TRUE)
        drives <- drives[grepl(":", drives)]
        drives <- gsub("\\s+", "", drives)
        names(drives) <- drives
      } else {
        drives <- list.files("/media", full.names = TRUE)
        names(drives) <- basename(drives)
        if (length(drives) == 0) {
          drives <- list.files("/mnt", full.names = TRUE)
          names(drives) <- basename(drives)
        }
      }
      c("wd" = getwd(), drives)
    }
    
    .get_valid_file_types <- function(engine_type) {
      if (engine_type %in% "MassSpecEngine") {
        c("mzML", "mzXML")
      } else if (engine_type %in% "RamanEngine") {
        c("asc")
      } else {
        c("txt", "csv")
      }
    }
    
    # _Constants -----
    
    ## Engine -----
    engine_call <- get(engine_type, envir = .GlobalEnv)
    engine_call_new <- engine_call[["new"]]
    engine <- suppressMessages(do.call(engine_call_new, list()))
    engine$load(engine_save_file)
    
    ## Other -----
    wdir <- getwd()
    save_file <- engine$save_file
    mandatory_header_names <- c("name", "author", "file", "date")
    volumes <- .get_volumes()
    file_types <- .get_valid_file_types(engine_type)
  
    # _Reactive values -----
    reactive_warnings <- shiny::reactiveVal(list())
    reactive_headers <- shiny::reactiveVal(engine$get_headers())
    reactive_overview <- shiny::reactiveVal(engine$get_overview())
    reactive_history <- shiny::reactiveVal(engine$history)
    reactive_saved_history <- shiny::reactiveVal(engine$history)
    reactive_workflow <- shiny::reactiveVal(engine$settings)
    
    # _Warnings -----
    
    ## obs Unsaved engine -----
    shiny::observe({
      has_unsaved_changes <- "unsaved_changes" %in% names(reactive_warnings())
      equal_history <- identical(reactive_history(), reactive_saved_history())
      if (!equal_history && !has_unsaved_changes) {
        reactive_warnings(.add_notifications(reactive_warnings(), "unsaved_changes", "Unsaved changes in the engine!"))
      }
    })
    
    ## out Warnings menu -----
    output$warningMenu <- shinydashboard::renderMenu({
      warnings <- reactive_warnings()
      msgs <- lapply(warnings, function(x) { notificationItem(text = x) })
      shinydashboard::dropdownMenu(type = "notifications", .list = msgs)
    })
    
    ## out Save engine -----
    output$save_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton("save_engine_button", label = "Save Engine", width = 200, class = "btn-danger")
        )
      }
    })
    
    ## event Save -----
    shiny::observeEvent(input$save_engine_button, {
      engine$save(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
    })
    
    ## out Reset engine -----
    output$reset_engine <- shiny::renderUI({
      if ("unsaved_changes" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shiny::actionButton("reset_engine_button", label = "Discard Changes", width = 200, class = "btn-danger")
        )
      }
    })
    
    ## event Reset -----
    shiny::observeEvent(input$reset_engine_button, {
      engine$load(save_file)
      reactive_warnings(.remove_notifications(reactive_warnings(), "unsaved_changes"))
      reactive_headers(engine$get_headers())
      reactive_overview(engine$get_overview())
      reactive_history(engine$history)
      reactive_saved_history(engine$history)
    })
  
    # _Overview -----
    
    ## _Info -----
    output$wdir <- shiny::renderUI({ htmltools::HTML(paste("<b>Working directory:</b>", wdir)) })
    
    ## _Headers -----
    
    ### out Headers -----
    output$headers <- shiny::renderUI({
      headers <- reactive_headers()
      lapply(names(headers), function(name) {
        if (name %in% mandatory_header_names) {
          htmltools::div(htmltools::tags$b(name), ": ", headers[[name]], htmltools::br())
        } else {
          button_id <- paste0("button_header_del_", name)
          shiny::observeEvent(input[[button_id]], {
            headers <- reactive_headers()
            headers[[name]] <- NULL
            reactive_headers(headers)
          }, ignoreInit = TRUE)
          htmltools::div(
            shiny::actionButton(button_id, label = NULL, icon = shiny::icon("trash"), width = '40px'),
            htmltools::tags$b(name), ": ", headers[[name]], htmltools::br()
          )
        }
      })
    })
    
    ### event Add headers -----
    shiny::observeEvent(input$add_header_button, {
      if (input$new_header_name != "" && input$new_header_value != "") {
        headers <- reactive_headers()
        headers[[input$new_header_name]] <- input$new_header_value
        reactive_headers(headers)
        shiny::updateTextInput(session, "new_header_name", value = "")
        shiny::updateTextInput(session, "new_header_value", value = "")
      }
    })
    
    ### obs Update headers -----
    shiny::observe({
      if (!identical(reactive_headers(), engine$headers)) {
        reactive_header_names <- names(reactive_headers())
        engine_header_names <- names(engine$headers)
        headers_to_remove <- engine_header_names[!engine_header_names %in% reactive_header_names]
        if (length(headers_to_remove) > 0) {
          tryCatch({
            engine$remove_headers(headers_to_remove)
            reactive_history(engine$history)
          }, warning = function(w) {
            msg <- paste("Warning for headers:", conditionMessage(w))
            shiny::showNotification(msg, duration = 10, type = "warning")
            reactive_headers(engine$get_headers())
          }, error = function(e) {
            msg <- paste("Error for headers:", conditionMessage(e))
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_headers(engine$get_headers())
          })
        }
        if (length(headers_to_remove) == 0) {
          tryCatch({
            engine$add_headers(reactive_headers())
            reactive_history(engine$history)
          }, warning = function(w) {
            msg <- paste("Warning for headers:", conditionMessage(w))
            shiny::showNotification(msg, duration = 10, type = "warning")
            reactive_headers(engine$get_headers())
          }, error = function(e) {
            msg <- paste("Error for headers:", conditionMessage(e))
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_headers(engine$get_headers())
          })
        }
      }
    })
    
    ## _Analyses -----
    
    ### out Analyses overview -----
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
          })
          
          #### event Update analyses -----
          shiny::observeEvent(input[[text_blk_id]], {
            if (!(input[[text_blk_id]] %in% blk) && !("analyses_overview_needs_update" %in% names(reactive_warnings()))) {
              msg <- "Engine analyses are not updated!"
              reactive_warnings(.add_notifications(reactive_warnings(), "analyses_overview_needs_update", msg))
            }
          })
          
          list(
            shiny::actionButton(button_id, label = NULL, icon = shiny::icon("trash"), width = '40px'),
            htmltools::tags$b(name),
            shiny::textInput(text_rpl_id, label = NULL, value = analyses$replicate[i], width = "100%"),
            shiny::selectInput(text_blk_id, label = NULL, choices = replicates, selected = analyses$blank[i], width = "100%")
          )
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
    
    ### out Analyses overview buttons -----
    output$analyses_overview_buttons <- shiny::renderUI({
      if ("analyses_overview_needs_update" %in% names(reactive_warnings())) {
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton("add_analyses_button", "Add Analysis Files", "Select Analysis Files", multiple = TRUE),
          shiny::actionButton("update_analyses_button", label = "Update Analyses", width = 200, class = "btn-danger"),
          shiny::actionButton("reset_analyses_button", label = "Discard Changes", width = 200, class = "btn-danger")
        )
      } else {
        htmltools::div(style = "margin-bottom: 20px;",
          shinyFiles::shinyFilesButton("add_analyses_button", "Add Analysis Files", "Select Analysis Files", multiple = TRUE)
        )
      }
    })
    
    ### file Add analyses -----
    shinyFiles::shinyFileChoose(input, "add_analyses_button", roots = volumes, defaultRoot = "wd", session = session, filetypes = file_types)
    
    ### event Add analyses -----
    shiny::observeEvent(input$add_analyses_button, {
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$add_analyses_button)
      if (nrow(fileinfo) > 0) {
        files <- fileinfo$datapath
        number_files <- length(files)
        if (number_files > 0) {
          output$loading_spinner <- shiny::renderUI({ htmltools::div(style = "height: 100px; width: 100px;") })
          shiny::withProgress(message = 'Loading files...', value = 0, {
            for (i in seq_len(number_files)) {
              tryCatch({
                engine$add_files(files[i])
              }, warning = function(w) {
                msg <- paste("Warning for", files[i], ":", conditionMessage(w))
                shiny::showNotification(msg, duration = 10, type = "warning")
              }, error = function(e) {
                msg <- paste("Error for", files[i], ":", conditionMessage(e))
                shiny::showNotification(msg, duration = 10, type = "error")
              })
              shiny::incProgress(i/number_files)
            }
          })
          output$loading_spinner <- shiny::renderUI({ NULL })
          reactive_overview(engine$get_overview())
          reactive_history(engine$history)
        }
      }
    })
    
    ### event Update rpl ad blk names -----
    shiny::observeEvent(input$update_analyses_button, {
      replicates <- vapply(reactive_overview()$analysis, function(name) input[[paste0("set_replicate_name_", name)]], NA_character_)
      replicates[replicates == "NA"] <- NA_character_
      blanks <- vapply(reactive_overview()$analysis, function(name) input[[paste0("set_blank_name_", name)]], NA_character_)
      blanks[blanks == "NA"] <- NA_character_
      if (engine$has_results()) {
        shiny::showModal(shiny::modalDialog(title = "Attention",
          "Modifying the replicate or blank names removes any results in the engine! Do you want to proceed with the operation?",
          footer = htmltools::tagList(shiny::modalButton("No"), shiny::actionButton("confirm_yes", "Yes"))
        ))
        shiny::observeEvent(input$confirm_yes, {
          shiny::removeModal()
          tryCatch({
            engine$add_replicate_names(replicates)
            engine$add_blank_names(blanks)
            reactive_overview(engine$get_overview())
            warnings <- reactive_warnings()
            warnings[["analyses_overview_needs_update"]] <- NULL
            reactive_warnings(warnings)
            reactive_history(engine$history)
          }, warning = function(w) {
            shiny::showNotification(conditionMessage(w), duration = 10, type = "warning")
          }, error = function(e) {
            shiny::showNotification(conditionMessage(e), duration = 10, type = "error")
          })
        })
      } else {
        tryCatch({
          engine$add_replicate_names(replicates)
          engine$add_blank_names(blanks)
          reactive_overview(engine$get_overview())
          warnings <- reactive_warnings()
          warnings[["analyses_overview_needs_update"]] <- NULL
          reactive_warnings(warnings)
          reactive_history(engine$history)
        }, warning = function(w) {
          shiny::showNotification(conditionMessage(w), duration = 10, type = "warning")
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), duration = 10, type = "error")
        })
      }
    })
    
    ### event Reset analyses overview -----
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
    
    ### obs Removes analyses -----
    shiny::observe({
      reactive_analyses <- reactive_overview()$analysis
      engine_analyses <- engine$get_overview()$analysis
      if (!identical(reactive_analyses, engine_analyses)) {
        analyses_to_remove <- engine_analyses[!engine_analyses %in% reactive_analyses]
        if (length(analyses_to_remove) > 0) engine$remove_analyses(analyses_to_remove)
        reactive_history(engine$history)
      }
    })
    
    # _Explorer -----
    
    ## out Explorer -----
    output$explorer_ui <- shiny::renderUI({
      if (engine_type %in% "MassSpecEngine") {
        .mod_MassSpecEngine_summary_Server("summary", engine, reactive_overview, volumes)
        .mod_MassSpecEngine_summary_UI("summary", engine)
        
      } else if (engine_type %in% "RamanEngine") {
        .mod_RamanEngine_summary_Server("summary", engine, reactive_overview, volumes)
        .mod_RamanEngine_summary_UI("summary", engine)
        
      } else {
        htmltools::div("Explorer not implemented for engine type ", engine_type)
      }
    })
    
    # _Workflow -----
    output$workflow_ui <- shiny::renderUI({
      .mod_workflow_Server("workflow", engine, reactive_workflow)
      .mod_workflow_UI("workflow")
    })
    
    
    # _History -----
    output$"history_table" <- shiny::renderDataTable({
      h_list <- reactive_history()
      h_dt <- data.table::rbindlist(h_list, fill = TRUE)
      h_dt$time <- format(h_dt$time, "%Y-%m-%d %H:%M:%S")
      h_dt
    })
  }

  return(server)
}