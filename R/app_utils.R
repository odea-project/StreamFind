#' @noRd
.app_util_add_notifications <- function(warnings, name_msg, msg) {
  shiny::showNotification(msg, duration = 5, type = "warning")
  warnings[[name_msg]] <- msg
  return(warnings)
}

#' @noRd
.app_util_remove_notifications <- function(warnings, name_msgs) {
  warnings[name_msgs] <- NULL
  return(warnings)
}

#' @noRd
.app_util_get_volumes <- function() {
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

#' @noRd
.app_util_use_initial_model <- function(reactive_app_mode,
                                        reactive_engine_type,
                                        reactive_engine_save_file,
                                        reactive_clean_start,
                                        reactive_show_init_modal,
                                        volumes,
                                        input, output, session) {
  
  available_engines <- .get_available_engines()
  
  time_var <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  model_elements <- list()
  
  model_elements[[1]] <- shiny::img(src = "www/logo_StreamFind.png", width = 250, style = "display: block; margin-left: auto; margin-right: auto;")
  
  model_elements[[2]] <- shiny::fluidRow(shiny::p("Select an engine to start a new project: ", style = "text-align: center;margin-top: 40px;"))
  
  model_elements[[3]] <- htmltools::div(lapply(available_engines, function(obj) shiny::actionButton(inputId = paste0(time_var, "_select_", obj), label = obj)), style = "text-align: center;")
  
  model_elements[[4]] <- shiny::fluidRow(shiny::p("Load an existing engine: ", style = "text-align: center;margin-top: 40px;"))
  
  shinyFiles::shinyFileChoose(input, paste0(time_var, "_select_LoadEngine"), roots = volumes, defaultRoot = "wd", session = session, filetypes = list(sqlite = "sqlite", rds = "rds"))
  
  model_elements[[5]] <- htmltools::div(shinyFiles::shinyFilesButton(paste0(time_var, "_select_LoadEngine"), "Load Engine (.sqlite)", "Load Engine from .sqlite file", multiple = FALSE), style = "text-align: center;")
  
  shiny::showModal(shiny::modalDialog(
    title = " ",
    easyClose = TRUE,
    footer = shiny::tagList(shiny::modalButton("Cancel")),
    do.call(tagList, model_elements)
  ))
  
  available_engines <- c(available_engines, "LoadEngine")
  
  lapply(available_engines, function(obj) {
    
    shiny::observeEvent(input[[paste0(time_var, "_select_", obj)]], {
      
      if (paste0(time_var, "_select_LoadEngine") %in% paste0(time_var, "_select_", obj) ) {
        input_name <- paste0(time_var, "_select_LoadEngine")
        req(input[[input_name]])
        fileinfo <- shinyFiles::parseFilePaths(volumes, input[[input_name]])
        if (nrow(fileinfo) > 0) {
          engine_save_file <- fileinfo$datapath
          engine_save_file <- StreamFind::EngineSaveFile(engine_save_file)
          engine_name <- engine_save_file$engine
          
          if (is.na(engine_name)) {
            msg <- paste("The file", engine_save_file, "is not a valid engine file!")
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_engine_save_file(NA_character_)
            shiny::removeModal()
            reactive_show_init_modal(TRUE)
            return()
          }
          
          if (!engine_name %in% available_engines) {
            msg <- paste("The engine", engine_name, "is not valid!")
            shiny::showNotification(msg, duration = 10, type = "error")
            reactive_engine_save_file(NA_character_)
            shiny::removeModal()
            reactive_show_init_modal(TRUE)
            return()
          }
          
          if (engine_name %in% available_engines) {
            reactive_app_mode("WorkflowAssembler")
            reactive_engine_type(engine_name)
            reactive_engine_save_file(engine_save_file$path)
            reactive_clean_start(TRUE)
            reactive_show_init_modal(FALSE)
            shiny::removeModal()
            return()
          }
        }
        
      } else {
        reactive_engine_save_file(NA_character_)
        reactive_engine_type(obj)
        shiny::removeModal()
        if (!obj %in% "CoreEngine") {
          reactive_show_init_modal(FALSE)
          reactive_app_mode("WorkflowAssembler")
          reactive_clean_start(TRUE)
        } else {
          reactive_show_init_modal(TRUE)
        }
        return()
      }
    })
  })
}