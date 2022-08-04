
#' @title newAnalysis
#'
#' @description Creates a new \pkg{streamFind} analysis object.
#'
#' @template args-newAnalysis-file
#' @template args-newAnalysis-replicate-blank
#'
#' @note The format of the file will dictate the resulting S4 class.
#' For instance, \emph{.mzML} or \emph{.mzXML} files will lead to the class
#' \linkS4class{msAnalysis}, which will contain the structure for
#' handling MS data.
#'
#' @return An S4 class depending on the added file format.
#' For instance, an \linkS4class{msAnalysis} is returned for
#' \emph{mzML} and \emph{mzXML} files.  If more than one file is given as
#' \code{file}, a \linkS4class{streamProject} object is returned instead.
#'
#' @export
#'
#' @importFrom data.table data.table is.data.table
#' @importFrom tools file_ext
#'
newAnalysis <- function(file = NA_character_,
                        replicate = NULL,
                        blank = NULL) {

  if (is.data.frame(file) | is.data.table(file)) {

    #add file column, for enabling the use of analysisInfo from patRoon
    if ("path" %in% colnames(file) &
        !"file" %in% colnames(file) &
        "analysis" %in% colnames(file)) {

      f_path_file <- apply(file, 1, function(x) {
        list.files(path = x["path"], pattern = x["analysis"], full.names = TRUE)
      })

      if (length(f_path_file) > nrow(file)) {
        p_formats <- c("mzML", "mzXML")
        f_path_file <- f_path_file[file_ext(f_path_file) %in% p_formats]
      }

      file$file <- f_path_file
    }

    if ("replicate" %in% colnames(file)) {
      replicate <- file$replicate
      names(replicate) <- file$file
    }

    if ("group" %in% colnames(fils)) {
      replicate <- file$group
      names(replicate) <- file$file
    }

    if ("blank" %in% colnames(file)) {
      blank <- file$blank
      names(blank) <- file$file
    }

    file <- file$file
  }

  file <- file[file.exists(file)]

  fFormats <- ".mzML|.mzXML"

  file <- file[grepl(fFormats, file)]

  if (length(files) < 1) {

    # TODO suggest to convert files
    # if (TRUE %in% grepl(tools::file_ext(f), compatibleFileFormatsForConversion()$format)) {
    #   warning("MS vendor file found! Use the function convertFiles
    #           for conversion to mzML/mzXML. See ?convertFiles for more information.")
    # }

    warning("A valid file path should be added
            to create an analysis object!")

    return(NULL)
  }

  if (is.null(replicate)) {
    replicate <- rep(NA_character_, length(file))
    names(replicate) <- file
  }

  if (is.null(blank)) {
    blank <- rep(NA_character_, length(file))
    names(blank) <- file
  }

  if (length(file) > 1) {
    return(
      newStreamProject(
        files = file,
        path = getwd(),
        title = NA_character_,
        date = Sys.Date(),
        replicates = replicate,
        blanks = blank,
        makeNewProject = FALSE
      )
    )
  }

  message("Loading analysis file...")

  if (grepl("mzML", file) | grepl("mzXML", file)) {
    analysis <- new("msAnalysis",
      file = file, replicate = unname(replicate), blank = unname(blank)
    )
  }

  # TODO implement further file types check-ups, such as for ramanAnalysis or uvAnalysis, as well for other MS file formats to implement convert functions

  message(" Done! \n")

  return(analysis)
}
