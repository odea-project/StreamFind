##' @noRd
##' @export
.mod_Report_UI.Engine <- function(x, id, ns) {
  ns2 <- shiny::NS(id)

  htmltools::div(
    style = "height: calc(100vh - 35px); overflow: hidden;",
    htmltools::div(
      style = "padding: 10px;",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Template"),
          shiny::p("Choose a Quarto (.qmd) template."),
          shinyFiles::shinyFilesButton(
            ns(ns2("select_qmd_file")),
            label = "Select QMD File",
            title = "Choose a QMD template",
            multiple = FALSE,
            style = "width: 200px;"
          ),
          shiny::br(),
          shiny::br(),
          shiny::verbatimTextOutput(ns(ns2("selected_qmd_file_display")))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Output File"),
          shiny::p("Choose output file name or path. If only a file name is used, output is written in Execute Directory."),
          shinyFiles::shinySaveButton(
            ns(ns2("select_output_file")),
            label = "Select Output File",
            title = "Choose report output file",
            filename = "report",
            filetype = list(html = "html", pdf = "pdf", docx = "docx"),
            style = "width: 200px;"
          ),
          shiny::br(),
          shiny::br(),
          shiny::verbatimTextOutput(ns(ns2("selected_output_file_display")))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Execution Directory"),
          shiny::p("Defaults to current working directory."),
          shinyFiles::shinyDirButton(
            ns(ns2("select_execute_dir")),
            label = "Select Execute Directory",
            title = "Choose execution directory",
            style = "width: 200px;"
          ),
          shiny::br(),
          shiny::br(),
          shiny::verbatimTextOutput(ns(ns2("selected_execute_dir_display")))
        )
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::actionButton(
            ns(ns2("generate_report_button")),
            label = "Generate Report",
            style = "width: 200px;"
          )
        )
      )
    )
  )
}

##' @noRd
##' @export
.mod_Report_Server.Engine <- function(x, id, ns, reactive_volumes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns2 <- shiny::NS(id)
    engine <- x

    selected_qmd_file <- shiny::reactiveVal(NULL)
    selected_output_file <- shiny::reactiveVal(NULL)
    selected_execute_dir <- shiny::reactiveVal(getwd())

    normalize_output_file <- function(path) {
      path <- normalizePath(path, mustWork = FALSE)
      path_dir <- dirname(path)
      path_name <- basename(path)
      path_ext <- tools::file_ext(path_name)
      if (nzchar(path_ext)) {
        path_name <- tools::file_path_sans_ext(path_name)
      }
      file.path(path_dir, path_name)
    }

    shinyFiles::shinyFileChoose(
      input,
      "select_qmd_file",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session,
      filetypes = "qmd"
    )

    shinyFiles::shinyFileSave(
      input,
      "select_output_file",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )

    shinyFiles::shinyDirChoose(
      input,
      "select_execute_dir",
      roots = reactive_volumes(),
      defaultRoot = "wd",
      session = session
    )

    shiny::observeEvent(input$select_qmd_file, {
      shiny::req(input$select_qmd_file)
      file_info <- shinyFiles::parseFilePaths(
        roots = reactive_volumes(),
        input$select_qmd_file
      )
      if (nrow(file_info) > 0) {
        qmd_path <- file_info$datapath[1]
        selected_qmd_file(qmd_path)
        if (is.null(selected_output_file())) {
          selected_output_file(tools::file_path_sans_ext(basename(qmd_path)))
        }
      }
    })

    shiny::observeEvent(input$select_output_file, {
      shiny::req(input$select_output_file)
      file_info <- shinyFiles::parseSavePath(
        roots = reactive_volumes(),
        input$select_output_file
      )
      if (nrow(file_info) > 0) {
        selected_output_file(normalize_output_file(file_info$datapath[1]))
      }
    })

    shiny::observeEvent(input$select_execute_dir, {
      shiny::req(input$select_execute_dir)
      dir_info <- shinyFiles::parseDirPath(
        roots = reactive_volumes(),
        input$select_execute_dir
      )
      if (length(dir_info) > 0) {
        selected_execute_dir(dir_info)
      }
    })

    output$selected_qmd_file_display <- shiny::renderText({
      if (is.null(selected_qmd_file())) {
        "QMD file: not selected"
      } else {
        paste("QMD file:", selected_qmd_file())
      }
    })

    output$selected_output_file_display <- shiny::renderText({
      if (is.null(selected_output_file())) {
        "Output file: not selected"
      } else {
        paste("Output file:", selected_output_file())
      }
    })

    output$selected_execute_dir_display <- shiny::renderText({
      paste("Execute dir:", selected_execute_dir())
    })

    shiny::observeEvent(input$generate_report_button, {
      qmd_path <- selected_qmd_file()
      if (is.null(qmd_path) || !file.exists(qmd_path)) {
        shiny::showNotification(
          "Please select a valid QMD file.",
          duration = 5,
          type = "error"
        )
        return()
      }

      output_file <- selected_output_file()
      if (is.null(output_file) || !nzchar(trimws(output_file))) {
        output_file <- tools::file_path_sans_ext(basename(qmd_path))
      }

      execute_dir <- selected_execute_dir()
      if (is.null(execute_dir) || !dir.exists(execute_dir)) {
        shiny::showNotification(
          "Execution directory does not exist.",
          duration = 5,
          type = "error"
        )
        return()
      }

      tryCatch(
        {
          shiny::showNotification(
            "Generating report. This may take a moment.",
            duration = 5,
            type = "message"
          )
          engine$report_quarto(
            template = qmd_path,
            output_file = output_file,
            execute_dir = execute_dir
          )
          output_target <- if (identical(dirname(output_file), ".")) {
            file.path(execute_dir, basename(output_file))
          } else {
            output_file
          }
          shiny::showNotification(
            paste0("Report generated: ", output_target),
            duration = 5,
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error generating report:", conditionMessage(e)),
            duration = 10,
            type = "error"
          )
        }
      )
    })
  })
}
