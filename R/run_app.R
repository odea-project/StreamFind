#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL, uiPattern = "/", ...) {

  # shiny is a hard prerequisite: the wizard itself needs it, so handle separately
  if (!requireNamespace("shiny", quietly = TRUE)) {
    message(
      "The 'shiny' package is required to run the StreamFind app.\n",
      "Install it with: install.packages('shiny')"
    )
    return(invisible(NULL))
  }

  # Remaining packages required for basic app operation.
  # shiny is guaranteed present at this point, so the install wizard can run.
  .app_required_pkgs <- c(
    "golem", "htmltools", "bslib",
    "shinycssloaders", "shinyFiles", "sortable", "DT", "config"
  )

  .missing_pkgs <- .app_required_pkgs[
    !vapply(.app_required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(.missing_pkgs) > 0) {
    message("Launching package installer. Missing: ", paste(.missing_pkgs, collapse = ", "))
    .run_app_install_wizard(.missing_pkgs)
    return(invisible(NULL))
  }

  library(StreamFind)
  dots <- list(...)
  if ("file" %in% names(dots)) {
    file <- dots$file
    if (!is.na(file)) {
      if (!grepl(".sqlite$|.rds$", file)) {
        msg <- paste("The file", file, "is not an sqlite or rds file!")
        stop(msg)
      } else {
        if (!file.exists(file)) {
          msg <- paste("The file", file, "does not exist!")
          stop(msg)
        }
      }
    }
  }
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

# Minimal wizard app launched when required packages are missing.
#' @noRd
.run_app_install_wizard <- function(missing_pkgs) {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style("
        body { font-family: sans-serif; background: #f4f6f8; }
        .sf-wizard {
          max-width: 560px; margin: 60px auto; background: white;
          border-radius: 8px; padding: 32px;
          box-shadow: 0 2px 12px rgba(0,0,0,.12);
        }
        .sf-pkg  { font-family: monospace; color: #c0392b; }
        .sf-done { background: #d4edda; border-radius: 4px; padding: 12px; margin-top: 16px; }
      ")
    ),
    shiny::div(
      class = "sf-wizard",
      shiny::h3("\u26a0 Missing Required Packages"),
      shiny::p("The following packages must be installed before running the StreamFind app:"),
      shiny::tags$ul(
        lapply(missing_pkgs, function(p) shiny::tags$li(shiny::span(class = "sf-pkg", p)))
      ),
      shiny::actionButton(
        "install_btn", "Install All",
        class = "btn btn-primary", style = "margin-top:8px;"
      ),
      shiny::verbatimTextOutput("install_log"),
      shiny::uiOutput("finish_note")
    )
  )

  server <- function(input, output, session) {
    install_done <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$install_btn, {
      shiny::updateActionButton(session, "install_btn",
        label = "Installing \u2026",
        disabled = TRUE
      )
      output$install_log <- shiny::renderText({
        msgs <- character(0)
        for (pkg in missing_pkgs) {
          msgs <- c(msgs, paste("Installing", pkg, "\u2026"))
          tryCatch(
            utils::install.packages(pkg, quiet = TRUE),
            error = function(e) {
              msgs <<- c(msgs, paste("  ERROR:", conditionMessage(e)))
            }
          )
        }
        # Verify
        still_missing <- missing_pkgs[
          !vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)
        ]
        if (length(still_missing) == 0) {
          install_done(TRUE)
          msgs <- c(msgs, "\n\u2713 All packages installed successfully.")
        } else {
          msgs <- c(msgs, paste("\n\u2717 Still missing:", paste(still_missing, collapse = ", ")))
        }
        paste(msgs, collapse = "\n")
      })
    })

    output$finish_note <- shiny::renderUI({
      if (!install_done()) return(NULL)
      shiny::div(
        class = "sf-done",
        shiny::strong("\u2713 Done!"),
        " Restart your R session and run ",
        shiny::code("run_app()"),
        " again to start StreamFind."
      )
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
}
