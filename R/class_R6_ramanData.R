#' **ramanData** R6 class and methods
#'
#' @description
#' The msData R6 class is a framework with methods for parsing, processing,
#' visualizing and storing RAMAN data.
#'
#' @export
#'
ramanData <- R6::R6Class("ramanData",

  # private fields -----
  private = list(

    ## .headers -----
    .headers = NULL,

    ## .settings -----
    .settings = NULL,

    ## .analyses -----
    .analyses = NULL
  ),

  # public fields/methods -----
  public = list(

    ## system -----
    initialize = function(files = NULL) {

      if (!is.null(files)) {

        analyses <- lapply(files, function(x) {
          spec <- hyperSpec::read.asc.Andor(
            file = x,
            quiet = TRUE,
            dec = ".",
            sep = ";"
          )
          sf <- as.numeric(colnames(spec$spc))
          int <- as.numeric(spec$spc[1,])
          df <- data.table("shift" = sf, "intensity" = int)

          f.name <- basename(x)
          f.ext <- tools::file_ext(f.name)
          f.name <- sub(paste0(".",f.ext), "", f.name)

          list(
            "name" = f.name,
            "spectum" = df
          )
        })

        names(analyses) <- vapply(analyses, function(x) x$name, "")

        private$.analyses <- analyses
      }

      message("\U2713 ramanData class object created!")
    },

    ## get -----
    get_analyses = function() {
      private$.analyses
    }
  )
)
