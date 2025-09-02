# StreamFind RInno Setup
# This script creates a Windows installer for the StreamFind Shiny application
# using the RInno package

library(RInno)

# Function to create StreamFind app installer
create_streamfind_app <- function() {
  
  # Required packages from DESCRIPTION (excluding the ones mentioned to exclude)
  required_packages <- c(
    # Core R packages
    "R6",
    "S7",
    "data.table",
    "checkmate",
    "Rcpp",
    "Rdpack",
    "magrittr",
    "grDevices",
    "jsonlite",
    "plotly",
    "RColorBrewer",
    "scales",
    "stats",
    "tools",
    "utils",
    "plyr",
    "methods",
    "digest",
    "DBI",
    "RSQLite",
    "withr",
    "fst",
    "reticulate",
    "ggplot2",
    "dplyr",
    "gridExtra",

    # Shiny and web packages
    "shiny",
    "golem",
    "shinydashboard",
    "shinycssloaders",
    "shinyFiles",
    "htmltools",
    "sortable",
    "DT",
    "config",
    "bslib"
  )

  remote_packages <- c(
    "odea-project/StreamFind@dev_s3_check"
  )

  # Temporarily increase timeout for large package downloads
  old_timeout <- getOption("timeout")
  options(timeout = 600) # 10 minutes
  on.exit(options(timeout = old_timeout)) # Restore original timeout when function exits

  # Try to get GitHub token safely
  github_token <- tryCatch(
    {
      if (requireNamespace("gitcreds", quietly = TRUE)) {
        gitcreds::gitcreds_get()$password
      } else {
        Sys.getenv("GITHUB_PAT", "")
      }
    },
    error = function(e) ""
  )

  RInno::create_app(
    app_name = "StreamFind",
    app_dir = "inst/app",
    dir_out = "RInno_installer",
    pkgs = required_packages,
    pkgs_path = "bin",
    repo = "https://cran.rstudio.com",
    remotes = "none",#remote_packages,
    locals = NULL,
    app_repo_url = "none",
    auth_user = "none",
    auth_pw = "none",
    auth_token = github_token,
    user_browser = "electron",
    include_R = FALSE,
    R_version = "4.5.1",
    include_Pandoc = FALSE,
    include_Chrome = FALSE,
    include_Rtools = FALSE,
    overwrite = TRUE,
    force_nativefier = TRUE,
    nativefier_opts = c()
  )

  cat("StreamFind installer created successfully!\n")
  cat("Directory:", file.path(getwd(), app_dir), "\n")
  cat("Next steps:\n")
  cat("1. Review the generated files in the app directory\n")
  cat("2. Customize icons, info files if needed\n")
  cat("3. Run compile_iss() to build the installer\n")
}

# Main function to setup everything
setup_streamfind_installer <- function() {
  cat("Setting up StreamFind installer...\n")

  # Create the installer (app directory already prepared)
  create_streamfind_app()

  cat("\nSetup complete! To build the installer:\n")
  cat("1. Review the generated files in inst/app/\n")
  cat("2. Run: compile_iss()\n")
}

# Example usage:
# setup_streamfind_installer()
#
# Or directly:
# create_streamfind_app()
# compile_iss()
