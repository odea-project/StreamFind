#' @noRd
.mod_WorkflowAssembler_workflow_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns(ns2("workflow_settings")))),
      shiny::column(6, shiny::uiOutput(ns(ns2("selected_method_details"))))
    ),
    htmltools::tags$style(htmltools::HTML("
      .custom-button {
        background-color: #3498DB;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
      }
      .custom-buttonred {
        background-color: #F1948A;
        color: white;
        border: none;
        padding: 5px 10px;
        margin: 5px;
        cursor: pointer;
        padding: 2px 6px;
        font-size: 12px;
      }
      .custom-button:hover {
        background-color: #5DADE2;
      }
      .custom-buttonred:hover {
        background-color: #F5B7B1;
      }
      .workflow-item {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .workflow-item span {
        flex-grow: 1;
      }
      .workflow-item button {
        margin-left: auto;
      }
      .workflow-box {
        background-color: white;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .method-details {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 5px;
        padding: 20px;
      }
      .method-details dt {
        font-weight: bold;
        float: left;
        clear: left;
        width: 120px;
      }
      .method-details dd {
        margin-left: 130px;
      }
      .parameters-section {
        margin-top: 20px;
        border-top: 1px solid #e9ecef;
        padding-top: 20px;
      }
    "))
  )
}

#' @noRd
.mod_WorkflowAssembler_workflow_Server <- function(id, ns,
                                                   engine, engine_type,
                                                   reactive_analyses,
                                                   reactive_workflow,
                                                   reactive_saved_workflow,
                                                   reactive_results,
                                                   reactive_warnings,
                                                   reactive_volumes) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)
    
    # var Available Methods -----
    processing_methods <- .get_available_settings(engine_type)
    if (length(processing_methods) == 0) {
      shiny::showNotification("No settings functions found for ", engine_type, " engine!", duration = 5, type = "warning")
      processing_methods_short <- NULL
    } else {
      engine_key <- paste0(gsub("Engine", "", engine_type), "Settings_")
      processing_methods_short <- gsub(engine_key, "", processing_methods)
      names(processing_methods) <- processing_methods_short
    }
    
    # var Reactive variables ----
    reactive_selected_method <- shiny::reactiveVal(NULL)
    reactive_workflow_file <- shiny::reactiveVal(NULL)
    
    # out Load Workflow -----
    output$load_workflow_ui <- shiny::renderUI({
      shinyFiles::shinyFilesButton(
        ns(ns2("load_workflow")),
        "Load Workflow",
        "Select a JSON or RDS file with workflow processing settings",
        multiple = FALSE,
        style = "width: 150px;"
      )
    })
    
    # out Clear Workflow -----
    output$clear_workflow_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      if (length(rw) > 0) {
        shiny::actionButton(ns(ns2("clear_workflow")), "Clear Changes", width = 150)
      }
    })
    
    # out Save Workflow -----
    output$save_workflow_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      rw_file <- reactive_workflow_file()
      
      if (length(rw) > 0) {
        shinyFiles::shinyFileSave(input, "save_workflow", roots = reactive_volumes(), defaultRoot = "wd", session = session)
        if (is.null(rw_file)) rw_file <- "workflow.rds"
        
        if (grepl(".json", rw_file)) {
          extensions <- list(json = "json", rds = "rds")
        } else {
          extensions <- list(rds = "rds", json = "json")
        }
        
        shinyFiles::shinySaveButton(
          ns(ns2("save_workflow")),
          label = "Save Workflow",
          title = "Save the workflow as .json or .rds",
          class = "btn-success",
          filename = gsub(".sqlite|.rds", "", basename(rw_file)),
          filetype = extensions, style = "width: 150px;"
        )
      }
    })
    
    # out Discard Changes -----
    output$discard_changes_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      saved_rw <- reactive_saved_workflow()
      if (!identical(rw, saved_rw)) {
        shiny::actionButton(ns(ns2("discard_changes")), "Discard Changes", class = "btn-danger", width = 150)
      }
    })
    
    # out Run Workflow -----
    output$run_workflow_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      if (length(rw) > 0) {
        shiny::actionButton(ns(ns2("run_workflow")), "Run Workflow", class = "btn-info", width = 150)
      }
    })
    
    # out Show Workflow -----
    output$workflow_settings <- shiny::renderUI({
      rw <- reactive_workflow()

      labels <- lapply(rw@names, function(i) {
        htmltools::tagList(
          htmltools::div(class = "workflow-item",
            shiny::actionButton(ns(ns2(paste0("workflow_del_", i))), "X", class = "custom-buttonred", style = "margin-right: 10px;"),
            shiny::span(i),
            shiny::actionButton(ns(ns2(paste0("workflow_edit_", i))), "Details", class = "custom-button")
          )
        )
      })
      
      ## obs Edit Workflow Method -----
      lapply(rw@names, function(i) {
        shiny::observeEvent(input[[paste0("workflow_edit_", i)]], {
          reactive_selected_method(i)
        }, ignoreInit = TRUE)
      })

      # obs Delete Workflow Method -----
      lapply(rw@names, function(i) {
        shiny::observeEvent(input[[paste0("workflow_del_", i)]], {
          rw <- reactive_workflow()
          rw[[i]] <- NULL
          reactive_workflow(rw)
        }, ignoreInit = TRUE)
      })

      shinydashboard::box(width = 12, title = NULL, solidHeader = TRUE, class = "workflow-box",
        
        shiny::column(12,
          htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns(ns2("load_workflow_ui")))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns(ns2("clear_workflow_ui")))),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        
        shiny::column(12,
          htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns(ns2("save_workflow_ui")))),
            htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns(ns2("discard_changes_ui")))),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        
        shiny::column(12,
          htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;",
              htmltools::div(style = "margin-right: 10px;", shiny::uiOutput(ns(ns2("run_workflow_ui")))),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        
        shiny::column(12, htmltools::p(" ")),
        shiny::column(12, htmltools::p("Select Processing Method")),
        shiny::column(12, shiny::selectInput(ns(ns2("settings_selector")), label = NULL, choices = processing_methods_short, multiple = FALSE)),
        shiny::column(12, htmltools::div(style = "display: flex; align-items: center; margin-bottom: 20px;", shiny::actionButton(ns(ns2("add_workflow_step")), "Add Workflow Step"))),
        shiny::column(12, htmltools::h3("Workflow")),
        
        shiny::column(12,
          sortable::rank_list(
            text = "Drag to order",
            labels = labels,
            input_id = ns(ns2("rank_workflow_names"))
          )
        )
      )
    })
    
    # obs Rank Workflow -----
    shiny::observeEvent(input$rank_workflow_names, {
      rw <- reactive_workflow()
      new_order <- input$rank_workflow_names
      new_order <- unname(vapply(new_order, function(z) strsplit(z, "\n")[[1]][2], NA_character_))
      rw[seq_along(new_order)] <- rw[new_order]
      reactive_workflow(rw)
    })
    
    # obs Load Workflow -----
    shinyFiles::shinyFileChoose(input, "load_workflow", roots = reactive_volumes(), defaultRoot = "wd", session = session, filetypes = list(json = "json", rds = "rds"))
    shiny::observeEvent(input$load_workflow, {
      rw <- reactive_workflow()
      fileinfo <- shinyFiles::parseFilePaths(roots = reactive_volumes(), input$load_workflow)
      if (nrow(fileinfo) > 0) {
        file <- fileinfo$datapath
        if (length(file) == 1) {
          if (file.exists(file)) {
            tryCatch({
              rw <- read(rw, file)
              reactive_workflow(rw)
            }, error = function(e) {
              shiny::showNotification(paste("Error loading workflow:", e$message), duration = 5, type = "error")
            }, warning = function(w) {
              shiny::showNotification(paste("Warning loading workflow:", w$message), duration = 5, type = "warning")
            })
          } else {
            shiny::showNotification("File does not exist!", duration = 5, type = "warning")
          }
        }
      }
    })
    
    # obs Clear Workflow -----
    shiny::observeEvent(input$clear_workflow, {
      reactive_workflow(StreamFind::Workflow())
    })
    
    # obs Save Workflow -----
    shiny::observeEvent(input$save_workflow, {
      shiny::req(input$save_workflow)
      file_info <- shinyFiles::parseSavePath(roots = reactive_volumes(), input$save_workflow)
      if (nrow(file_info) > 0) {
        file_path <- file_info$datapath
        
        tryCatch({
          rw <- reactive_workflow()
          save(
            rw,
            format = tools::file_ext(file_path),
            name = basename(tools::file_path_sans_ext(file_path)),
            path = dirname(file_path)
          )
          reactive_workflow_file(file_path)
          shiny::showNotification(paste("Workflow saved successfully as ", file_path), duration = 5, type = "message")
        }, error = function(e) {
          shiny::showNotification(paste("Error saving workflow:", e$message), duration = 5, type = "error")
        }, warning = function(w) {
          shiny::showNotification(paste("Warning saving workflow:", w$message), duration = 5, type = "warning")
        })
      }
    })
    
    # obs Discard Changes -----
    shiny::observeEvent(input$discard_changes, {
      shiny::req(input$discard_changes)
      reactive_workflow(reactive_saved_workflow())
    })
    
    # obs Run Workflow -----
    shiny::observeEvent(input$run_workflow, {
      shiny::req(input$run_workflow)
      
      shiny::showModal(shiny::modalDialog(
        title = "Processing",
        "The workflow is running. Please wait...",
        footer = NULL # No footer buttons to close the modal manually
      ))
      
      tryCatch({
        engine$workflow <- reactive_workflow()
        engine$run_workflow()
        reactive_analyses(engine$analyses)
        reactive_workflow(engine$workflow)
        reactive_results(engine$results)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste("Error running workflow:", e$message), duration = 5, type = "error")
        shiny::removeModal()
      })
    })
    
    # obs Add Workflow Step -----
    shiny::observeEvent(input$add_workflow_step, {
      rw <- reactive_workflow()
      settings_name <- input$settings_selector
      if (length(settings_name) > 0) {
        settings <- do.call(processing_methods[settings_name], list())
        
        tryCatch({
          rw[[length(rw) + 1]] <- settings
          reactive_workflow(rw)
        }, error = function(e) {
          shiny::showNotification(paste("Error adding settings:", e$message), duration = 5, type = "error")
        }, warning = function(w) {
          shiny::showNotification(paste("Warning adding settings:", w$message), duration = 5, type = "warning")
        })
      }
    })
    
    # out Selected Method Details -----
    output$selected_method_details <- shiny::renderUI({
      shiny::req(reactive_selected_method())
      selected_method <- reactive_selected_method()
      rw <- reactive_workflow()
      if (selected_method %in% rw@names) {
        htmltools::tagList(
          shiny::h3(selected_method, style = "color: #3498DB; margin-bottom: 20px;"),
          shiny::actionButton(ns(ns2("update_method")), "Update Settings", class = "btn-primary", style = "color: white; margin-top: 20px; margin-bottom: 20px"),
          shiny::uiOutput(ns(ns2("method_parameters_ui")))
        )
      }
    })
    
    # obs Update Method -----
    shiny::observeEvent(input$update_method, {
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% rw@names)
      settings <- rw[[selected_method]]
      param_names <- names(settings$parameters)
      
      for (param_name in param_names) {
        param_class <- class(settings$parameters[[param_name]])
        
        if (param_class == "logical") {
          settings$parameters[[param_name]] <- as.logical(input[[param_name]])
          
        } else if (param_class == "numeric") {
          settings$parameters[[param_name]] <- as.numeric(input[[param_name]])
          
        } else if (param_class == "character") {
          settings$parameters[[param_name]] <- as.character(input[[param_name]])
          
        } else if (param_class == "integer") {
          settings$parameters[[param_name]] <- as.integer(input[[param_name]])
          
        } else if (param_class == "data.frame") {
          settings$parameters[[param_name]] <- as.data.frame(input[[param_name]])
          
        } else if (param_class == "NULL") {
          settings$parameters[param_name] <- list(NULL)
          
        } else {
          browser()
          shiny::showNotification(paste("Unsupported parameter type for ", param_name), duration = 5, type = "warning")
        }
      }
      
      rw[[selected_method]] <- settings
      reactive_workflow(rw)
      shiny::showNotification("Settings updated successfully!", type = "message")
    })
    
    # out Method Parameters -----
    output$method_parameters_ui <- shiny::renderUI({
      shiny::req(reactive_selected_method())
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% rw@names)
      short_selected_method <- gsub("\\d+_", "", selected_method)
      method_name <- processing_methods[short_selected_method]
      help_url <- paste0("https://odea-project.github.io/StreamFind/reference/", method_name, ".html")
      settings <- rw[[selected_method]]
      
      ## func Create Parameter UI NoEdit -----
      create_parameter_ui_noedit <- function(param_name, param_value) {
        value_display <- if (is.atomic(param_value) && length(param_value) == 1) {
          as.character(param_value)
        } else if (is.list(param_value)) {
          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            lapply(names(param_value), function(sub_param) {
              shiny::tags$li(
                shiny::tags$span(style = "color: #2980B9; font-weight: bold;", sub_param), ": ",
                if (is.atomic(param_value[[sub_param]])) {
                  as.character(param_value[[sub_param]])
                } else {
                  "NULL"
                }
              )
            })
          )
        } else {
          "NULL"
        }

        shiny::tagList(
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(value_display)
        )
      }
      
      ## func Create Parameter UI -----
      create_parameter_ui <- function(ns2, param_name, param_value) {
        input_element <- NULL
        
        if (is.null(param_value)) {
          input_element <- shiny::textInput(ns(ns2(param_name)), label = NULL, value = "")
          
        } else if (is.logical(param_value)) {
          input_element <- shiny::checkboxInput(ns(ns2(param_name)), label = NULL, value = param_value)
          
        } else if (is.numeric(param_value)) {
          input_element <- shiny::numericInput(ns(ns2(param_name)), label = NULL, value = param_value)
          
        } else if (is.character(param_value)) {
          input_element <- shiny::textInput(ns(ns2(param_name)), label = NULL, value = param_value)
          
        # TODO all method must have character. logical, numeric, integer or data.frame not list
        } else if (is.list(param_value) && all(sapply(param_value, is.character))) {
          input_element <- shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            lapply(seq_along(param_value), function(i) {
              shiny::tags$li(
                shiny::textInput(ns(ns2(paste0(param_name, "_", i))), label = NULL, value = param_value[[i]])
              )
            })
          )
          
        # TODO Add support for data.frame by loading a csv from disk, validation of the csv is done a the settings class
        } else {
          input_element <- shiny::tags$p(paste("Unsupported parameter type: ", class(param_value)))
        }

        shiny::tagList(
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(style = "display: flex; align-items: center;", input_element)
        )
      }

      param_names <- names(settings$parameters)
      other_names <- setdiff(names(settings), c("parameters"))

      shiny::tags$div(
        class = "method-details",
        
        if (!is.null(help_url)) {
          shiny::tags$div(
            style = "margin-top: 20px;margin-bottom: 20px;",
            shiny::tags$a(
              href = help_url,
              target = "_blank",
              "View Help Documentation",
              style = "color: #3498DB; text-decoration: underline; cursor: pointer; font-size: 16px;"
            )
          )
        } else {
          shiny::tags$div(style = "margin-top: 20px;margin-bottom: 20px;", "No help documentation available.")
        },
        
        shiny::tags$dl(
          lapply(other_names, function(param) {
            create_parameter_ui_noedit(param, settings[[param]])
          })
        ),
        
        shiny::tags$div(
          class = "parameters-section",
          shiny::tags$h4("Parameters"),
          shiny::tags$dl(
            lapply(param_names, function(param) {
              create_parameter_ui(ns2, param, settings$parameters[[param]])
            })
          )
        )
      )
    })
  })
}