#' @title Load Template Engine with Required Setup
#' @description This function loads an engine RDS file and sets up the required options and libraries for Quarto document rendering. It configures DT options for data tables and loads necessary packages for visualization and document generation.
#' @param engine_rds_path Character string. Path to the engine RDS file.
#' @param packages Character vector. Packages to load. Defaults to common packages used in StreamFind reports.
#' @return The loaded engine object from the RDS file.
#' @examples
#' \dontrun{
#' engine <- load_template_engine("path/to/engine.rds")
#' }
#'
#' @export
load_template_engine <- function(
  engine_rds_path,
  packages = c("StreamFind", "knitr", "kableExtra", "data.table", "DT", "magrittr", "ggplot2", "plotly")
) {
  options(
    DT.options = list(
      dom = "lfrtip",
      autoWidth = TRUE,
      pageLength = 10,
      lengthMenu = c(10, 20, 50, 75, 100),
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '12px'});",
        "$(this.api().table().body()).css({'white-space': 'nowrap'});",
        "}"
      )
    )
  )
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE)
    } else {
      warning(paste("Package", pkg, "is not available"))
    }
  }
  library(StreamFind)
  engine <- readRDS(engine_rds_path)
  return(engine)
}
