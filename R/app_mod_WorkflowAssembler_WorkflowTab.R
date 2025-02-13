#' @noRd
.mod_WorkflowAssembler_workflow_UI <- function(id, ns) {
  ns2 <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        class = "workflow-column",
        shiny::uiOutput(ns(ns2("workflow_settings")))
      ),
      shiny::column(
        width = 6,
        class = "method-column",
        shiny::uiOutput(ns(ns2("selected_method_details")))
      )
    ),
    htmltools::tags$style(htmltools::HTML("
      .workflow-column {
        padding-right: 0px;
        margin-right: 0px;
      }
      .method-column {
        padding-left: 0px;
        margin-left: 0px;
      }
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
      .method-details dt {
        font-weight: bold;
        float: left;
        clear: left;
        width: 30%;
        padding-bottom: 1px;
        padding-top: 1px;
        margin-bottom: 0px;
        margin-top: 0px;
      }
      .method-details dd {
        width: 70%;
        padding-bottom: 1px;
        padding-top: 12px;
        margin-bottom: 0px;
        margin-top: 0px;
      }
      .parameters-section {
        margin-top: 2px;
        border-top: 1px solid #ccc;
        padding-top: 2px;
      }
      .modal-content {
        resize: both;
        overflow: auto;
      }
      .modal-dialog {
        position: relative;
        width: auto;
        max-width: 80%;
        min-width: 500px;
      }
    "))
  )
}

#' @noRd
.mod_WorkflowAssembler_workflow_Server <- function(id,
                                                   ns,
                                                   engine, engine_type,
                                                   reactive_analyses,
                                                   reactive_workflow,
                                                   reactive_saved_workflow,
                                                   reactive_results,
                                                   reactive_audit,
                                                   reactive_warnings,
                                                   reactive_volumes,
                                                   reactive_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)
    
    # var Available Methods -----
    processing_methods <- .get_available_processing_methods(engine_type)
    
    if (length(processing_methods) == 0) {
      shiny::showNotification(
        "No settings functions found for ", engine_type, " engine!",
        duration = 5,
        type = "warning"
      )
      processing_methods_short <- NULL
    } else {
      engine_key <- paste0(gsub("Engine", "", engine_type), "Method_")
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
        shiny::actionButton(ns(ns2("clear_workflow")), "Clear Workflow", width = 150)
      }
    })
    
    # out Save Workflow -----
    output$save_workflow_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      rw_file <- reactive_workflow_file()
      
      if (length(rw) > 0) {
        shinyFiles::shinyFileSave(
          input, "save_workflow",
          roots = reactive_volumes(),
          defaultRoot = "wd",
          session = session
        )
        
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
        shiny::actionButton(
          ns(ns2("discard_changes")),
          "Discard Changes",
          class = "btn-danger",
          width = 150
        )
      }
    })
    
    # out Run Workflow -----
    output$run_workflow_ui <- shiny::renderUI({
      rw <- reactive_workflow()
      if (length(rw) > 0) {
        shiny::actionButton(
          ns(ns2("run_workflow")),
          "Run Workflow",
          class = "btn-info",
          width = 150
        )
      }
    })
    
    # out Show Workflow -----
    output$workflow_settings <- shiny::renderUI({
      rw <- reactive_workflow()
      labels <- lapply(names(rw), function(i) {
        htmltools::tagList(
          htmltools::div(
            class = "workflow-item",
            shiny::actionButton(
              ns(ns2(paste0("workflow_del_", i))),
              label = "X",
              # TODO check why an error is thrown when using icon
              # icon = shiny::icon("trash"),
              class = "custom-buttonred",
              style = "margin-right: 10px;"
            ),
            shiny::span(i),
            shiny::actionButton(
              ns(ns2(paste0("workflow_edit_", i))),
              "Details",
              class = "custom-button"
            )
          )
        )
      })
      
      ## obs Edit Workflow Method -----
      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_edit_", i)]], {
          reactive_selected_method(i)
        }, ignoreInit = TRUE)
      })
      
      # obs Delete Workflow Method -----
      lapply(names(rw), function(i) {
        shiny::observeEvent(input[[paste0("workflow_del_", i)]], {
          rw <- reactive_workflow()
          rw[[i]] <- NULL
          reactive_workflow(rw)
        }, ignoreInit = TRUE)
      })
      
      shinydashboard::box(
        width = 12,
        title = NULL,
        solidHeader = TRUE,
        class = "workflow-box",
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(
              style = "margin-right: 10px;",
              shiny::uiOutput(ns(ns2("load_workflow_ui")))
            ),
            htmltools::div(
              style = "margin-right: 10px;",
              shiny::uiOutput(ns(ns2("clear_workflow_ui")))
            ),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(
              style = "margin-right: 10px;",
              shiny::uiOutput(ns(ns2("save_workflow_ui")))
            ),
            htmltools::div(
              style = "margin-right: 10px;",
              shiny::uiOutput(ns(ns2("discard_changes_ui")))
            ),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; margin-bottom: 20px;",
            htmltools::div(
              style = "margin-right: 10px;",
              shiny::uiOutput(ns(ns2("run_workflow_ui")))
            ),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        shiny::column(width = 12, htmltools::p(" ")),
        shiny::column(width = 12, htmltools::p("Select Processing Method")),
        shiny::column(
          width = 12,
          shiny::selectInput(
            ns(ns2("settings_selector")),
            label = NULL,
            choices = processing_methods_short,
            multiple = FALSE
          )
        ),
        shiny::column(
          width = 12,
          htmltools::div(
            style = "display: flex; align-items: center; margin-bottom: 20px;",
            shiny::actionButton(ns(ns2("add_workflow_step")), "Add Workflow Step")
          )
        ),
        shiny::column(width = 12, htmltools::h3("Workflow")),
        shiny::column(
          width = 12,
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
      
      withCallingHandlers({
        rw[seq_along(new_order)] <- rw[new_order]
        reactive_workflow(rw)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error ranking workflow:", e$message),
          duration = 5,
          type = "error"
        )
      }, warning = function(w) {
        shiny::showNotification(
          paste("Warning ranking workflow:", w$message),
          duration = 5,
          type = "warning"
        )
      })
    })

    # obs Load Workflow -----
    shinyFiles::shinyFileChoose(
      input,
      "load_workflow",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session,
      filetypes = list(json = "json", rds = "rds")
    )
    shiny::observeEvent(input$load_workflow, {
      rw <- reactive_workflow()
      fileinfo <- shinyFiles::parseFilePaths(roots = reactive_volumes(), input$load_workflow)
      if (nrow(fileinfo) > 0) {
        file <- fileinfo$datapath
        if (length(file) == 1) {
          if (file.exists(file)) {
            tryCatch(
              {
                rw <- read(rw, file)
                reactive_workflow(rw)
              },
              error = function(e) {
                shiny::showNotification(
                  paste("Error loading workflow:", e$message),
                  duration = 5,
                  type = "error"
                )
              },
              warning = function(w) {
                shiny::showNotification(
                  paste("Warning loading workflow:", w$message),
                  duration = 5,
                  type = "warning"
                )
              }
            )
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
        tryCatch(
          {
            rw <- reactive_workflow()
            save(rw, file_path)
            reactive_workflow_file(file_path)
            shiny::showNotification(
              paste("Workflow saved successfully as ", file_path),
              duration = 5,
              type = "message"
            )
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error saving workflow:", e$message),
              duration = 5,
              type = "error"
            )
          },
          warning = function(w) {
            shiny::showNotification(
              paste("Warning saving workflow:", w$message),
              duration = 5,
              type = "warning"
            )
          }
        )
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
      tryCatch(
        {
          engine$workflow <- reactive_workflow()
          engine$run_workflow()
          reactive_analyses(engine$Analyses)
          reactive_workflow(engine$workflow)
          reactive_results(engine$results)
          reactive_audit(engine$audit_trail)
          shiny::removeModal()
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error running workflow:", e$message),
            duration = 5,
            type = "error"
          )
          shiny::removeModal()
        }
      )
    })

    # obs Add Workflow Step -----
    shiny::observeEvent(input$add_workflow_step, {
      rw <- reactive_workflow()
      settings_name <- input$settings_selector
      if (length(settings_name) > 0) {
        settings <- do.call(processing_methods[settings_name], list())
        tryCatch(
          {
            rw[[length(rw) + 1]] <- settings
            reactive_workflow(rw)
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error adding settings:", e$message),
              duration = 5,
              type = "error"
            )
          },
          warning = function(w) {
            shiny::showNotification(
              paste("Warning adding settings:", w$message),
              duration = 5,
              type = "warning"
            )
          }
        )
      }
    })

    # out Selected Method Details -----
    output$selected_method_details <- shiny::renderUI({
      shiny::req(reactive_selected_method())
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% names(rw))
      short_selected_method <- gsub("\\d+_", "", selected_method)
      idx_selected_method <- gsub("\\D", "", selected_method)
      method_name <- processing_methods[short_selected_method]
      help_url <- paste0(
        "https://odea-project.github.io/StreamFind/reference/",
        method_name,
        ".html"
      )
      settings <- rw[[selected_method]]
      method_editor_title <- paste0(idx_selected_method, ": ", short_selected_method)
      create_parameter_ui <- function(ns2, param_name, param_value) {
        input_element <- NULL
        if (is.null(param_value)) {
          input_element <- shiny::textInput(
            ns(ns2(param_name)),
            label = NULL,
            value = "",
            width = "100%"
          )
        } else if (is.logical(param_value)) {
          input_element <- shiny::checkboxInput(
            ns(ns2(param_name)),
            label = NULL,
            value = param_value
          )
        } else if (is.numeric(param_value)) {
          input_element <- shiny::numericInput(
            ns(ns2(param_name)),
            label = NULL,
            value = param_value,
            width = "100%"
          )
        } else if (is.character(param_value)) {
          input_element <- shiny::textInput(
            ns(ns2(param_name)),
            label = NULL,
            value = param_value,
            width = "100%"
          )
        } else if (is.data.frame(param_value)) {
          pram_load_name <- paste0(selected_method, "_load_", param_name)
          pram_save_name <- paste0(selected_method, "_save_", param_name)

          custom_datatable_str_out <- function(dt, n = 5) {
            output <- paste0(
              "Data Table with ", nrow(dt), " observations of ", ncol(dt), " variables:<br>"
            )
            for (col_name in names(dt)) {
              col_type <- class(dt[[col_name]])
              col_values <- head(dt[[col_name]], n)
              col_values_str <- paste(col_values, collapse = ", ")
              output <- paste0(output, "$ ", col_name, " : ", col_type, "<br>")
            }
            shiny::tags$span(shiny::HTML(output))
          }

          shinyFiles::shinyFileChoose(
            input,
            pram_load_name,
            roots = reactive_volumes(),
            defaultRoot = "wd",
            session = session,
            filetypes = list(csv = "csv")
          )
          
          shinyFiles::shinyFileSave(
            input,
            pram_save_name,
            roots = reactive_volumes(),
            defaultRoot = "wd",
            session = session
          )

          input_element <- shiny::tags$div(
            shinyFiles::shinyFilesButton(
              ns(ns2(pram_load_name)),
              "Load (.csv)",
              paste0("Select a CSV file for parameter ", param_name),
              multiple = FALSE,
              style = "width: 150px;margin-bottom: 12px;"
            ),
            shinyFiles::shinySaveButton(
              ns(ns2(pram_save_name)),
              label = "Save (.csv)",
              title = paste0("Save as CSV the parameter ", param_name),
              filename = param_name,
              filetype = list(csv = "csv"),
              style = "width: 150px;margin-bottom: 12px;"
            ),
            shiny::tags$br(),
            custom_datatable_str_out(param_value, 5)
          )

          shiny::observeEvent(input[[pram_load_name]], {
            fileinfo <- shinyFiles::parseFilePaths(
              roots = reactive_volumes(),
              input[[pram_load_name]]
            )
            if (nrow(fileinfo) > 0) {
              file <- fileinfo$datapath
              if (length(file) == 1) {
                if (file.exists(file)) {
                  tryCatch(
                    {
                      param_value <- read.csv(param_value, file)
                      settings$parameters[[param_name]] <- param_value
                      rw[[selected_method]] <- settings
                      reactive_workflow(rw)
                    },
                    error = function(e) {
                      shiny::showNotification(
                        paste("Error loading csv file:", e$message),
                        duration = 5,
                        type = "error"
                      )
                    },
                    warning = function(w) {
                      shiny::showNotification(
                        paste("Warning loading csv file:", w$message),
                        duration = 5,
                        type = "warning"
                      )
                    }
                  )
                } else {
                  shiny::showNotification(
                    "CSV file does not exist!",
                    duration = 5,
                    type = "warning"
                  )
                }
              }
            }
          })

          shiny::observeEvent(input[[pram_save_name]], {
            shiny::req(input[[pram_save_name]])
            file_info <- shinyFiles::parseSavePath(
              roots = reactive_volumes(),
              input[[pram_save_name]]
            )
            if (nrow(file_info) > 0) {
              file_path <- file_info$datapath
              tryCatch(
                {
                  write.csv(param_value, file_path, row.names = FALSE)
                  shiny::showNotification(
                    paste("Parameter saved successfully as ", file_path),
                    duration = 5,
                    type = "message"
                  )
                },
                error = function(e) {
                  shiny::showNotification(
                    paste("Error saving csv:", e$message),
                    duration = 5,
                    type = "error"
                  )
                },
                warning = function(w) {
                  shiny::showNotification(
                    paste("Warning saving csv:", w$message),
                    duration = 5,
                    type = "warning"
                  )
                }
              )
            }
          })
        } else {
          input_element <- shiny::tags$p(paste("Unsupported parameter type: ", class(param_value)))
        }

        shiny::div(
          style = "display: flex; align-items: center;border-bottom: 1px solid #ccc;",
          shiny::tags$dt(shiny::tags$strong(param_name)),
          shiny::tags$dd(input_element)
        )
      }

      param_names <- names(settings$parameters)

      shinydashboard::box(
        width = 12, title = NULL, solidHeader = TRUE, class = "method-box",
        shiny::tags$div(
          class = "method-details",
          shiny::h3(method_editor_title, style = "margin-bottom: 10px;"),
          shiny::tags$div(
            shiny::tags$b("Software: "),
            shiny::tags$span(settings$software),
            shiny::tags$br(),
            shiny::tags$b("Developer: "),
            shiny::tags$span(settings$developer),
            shiny::tags$br(),
            shiny::tags$b("Contact: "),
            shiny::tags$span(settings$contact),
            shiny::tags$br(),
            shiny::tags$b("Link: "),
            shiny::tags$span(settings$link),
            shiny::tags$br(),
            shiny::tags$b("DOI: "),
            shiny::tags$span(settings$doi)
          ),
          if (!is.null(help_url)) {
            shiny::tags$div(
              style = "margin-top: 5px;margin-bottom: 5px;",
              shiny::tags$a(
                href = help_url,
                target = "_blank",
                "View Online Reference Page",
                style = "color: #3498DB; text-decoration: underline; cursor: pointer; font-size: 14px;"
              )
            )
          } else {
            shiny::tags$div(
              style = "margin-top: 5px;margin-bottom: 5px;",
              "No help documentation available."
            )
          },
          shiny::actionButton(
            ns(ns2("open_help_modal")),
            "Help",
            style = "margin-top: 10px; margin-bottom: 10px"
          ),
          shiny::actionButton(
            ns(ns2("update_method")),
            "Update Parameters",
            class = "btn-primary",
            style = "color: white; margin-top: 10px; margin-bottom: 10px"
          ),
          shiny::actionButton(
            ns(ns2("reset_method")),
            "Reset Parameters",
            class = "btn-primary",
            style = "color: white; margin-top: 10px; margin-bottom: 10px"
          ),
          shiny::tags$div(
            class = "parameters-section",
            shiny::tags$dl(
              lapply(param_names, function(param) {
                create_parameter_ui(ns2, param, settings$parameters[[param]])
              })
            )
          )
        )
      )
    })

    # obs Help Modal -----
    shiny::observeEvent(input$open_help_modal, {
      shiny::req(reactive_selected_method())
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% names(rw))
      short_selected_method <- gsub("\\d+_", "", selected_method)
      idx_selected_method <- gsub("\\D", "", selected_method)
      method_name <- processing_methods[short_selected_method]
      package_name <- "StreamFind"
      package_path <- find.package(package_name, lib.loc = .libPaths())
      tryCatch(
        {
          rd <- tools:::fetchRdDB(paste0(package_path, "/help/StreamFind"), method_name)
          help_page <- capture.output(
            tools::Rd2HTML(rd, out = "", options = list(underline_titles = FALSE))
          )
          shiny::showModal(
            shiny::modalDialog(
              title = NULL,
              size = "l",
              shiny::HTML(help_page),
              easyClose = TRUE,
              footer = NULL
            )
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error getting help file for ", method_name, ":", e$message),
            duration = 5,
            type = "error"
          )
          return()
        },
        warning = function(w) {
          shiny::showNotification(
            paste("Warning getting help file for ", method_name, ":", w$message),
            duration = 5,
            type = "warning"
          )
          return()
        }
      )
    })

    # obs Update Method -----
    shiny::observeEvent(input$update_method, {
      shiny::req(input$update_method)
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% names(rw))
      settings <- rw[[selected_method]]
      param_names <- names(settings$parameters)
      for (param_name in param_names) {
        tryCatch(
          {
            param_class <- class(settings$parameters[[param_name]])
            if ("logical" %in% param_class) {
              value <- as.logical(input[[param_name]])
              if (length(value) == 0) value <- as.logical(NA)
              settings$parameters[[param_name]] <- as.logical(value)
            } else if ("numeric" %in% param_class) {
              value <- as.numeric(input[[param_name]])
              if (length(value) == 0) value <- as.numeric(NA_real_)
              settings$parameters[[param_name]] <- as.numeric(value)
            } else if ("character" %in% param_class) {
              value <- as.character(input[[param_name]])
              if (length(value) == 0) value <- as.character(NA_character_)
              settings$parameters[[param_name]] <- as.character(value)
            } else if ("integer" %in% param_class) {
              value <- as.integer(input[[param_name]])
              if (length(value) == 0) value <- as.integer(NA_integer_)
              settings$parameters[[param_name]] <- as.integer(value)
            } else if ("data.frame" %in% param_class) {
              next
            } else if (param_class == "NULL") {
              shiny::showNotification(
                paste("Parameter ", param_name, " is NULL"),
                duration = 5,
                type = "warning"
              )
            } else {
              shiny::showNotification(
                paste("Unsupported parameter type for ", param_name),
                duration = 5,
                type = "warning"
              )
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error getting parameter ", param_name, " value:", e$message),
              duration = 5,
              type = "error"
            )
          },
          warning = function(w) {
            shiny::showNotification(
              paste("Warning getting parameter ", param_name, " value:", w$message),
              duration = 5,
              type = "warning"
            )
          }
        )
      }
      rw[[selected_method]] <- settings
      reactive_workflow(rw)
      shiny::showNotification("Settings updated successfully!", type = "message")
    })

    # obs Reset Method -----
    shiny::observeEvent(input$reset_method, {
      rw <- reactive_workflow()
      selected_method <- reactive_selected_method()
      shiny::req(selected_method %in% names(rw))
      short_selected_method <- gsub("\\d+_", "", selected_method)
      method_name <- processing_methods[short_selected_method]
      settings <- do.call(method_name, list())
      rw[[selected_method]] <- settings
      reactive_workflow(rw)
      shiny::showNotification("Settings reset successfully!", type = "message")
    })
  })
}
