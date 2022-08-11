
checkFileValidity <- function(file = NA_character_) {

  # TODO add other file formats (inc. Raman and UV)
  # and checkups for general evaluation
  fFormats <- ".mzML|.mzXML"

  check <- grepl(fFormats, file) & is.character(file)

  check <- check & file.exists(file)

  return(check)
}



#' @template args-newAnalysis-file
#' @template args-newAnalysis-replicate-blank
#'
#' @importFrom data.table is.data.table
#'
#' @noRd
checkFilesInput <- function(file = NA_character_,
                            replicate = NA_character_,
                            blank = NA_character_) {

  if (is.data.frame(file) | is.data.table(file)) {

    if ("path" %in% colnames(file) &
          !"file" %in% colnames(file) &
            "analysis" %in% colnames(file)) {


      f_path_file <- apply(file, 1, function(x) {
        list.files(path = x["path"],
                   pattern = x["analysis"],
                   full.names = TRUE)
      })

      if (length(f_path_file) > nrow(file)) {
        f_path_file <- f_path_file[checkFileValidity(f_path_file)]
      }

      if (legnth(file$file) == length(f_path_file)) {
        file$file <- f_path_file
      } else {
        warning("Files with same name but different valid formats present!")
        return(NULL)
      }
    }

    if ("replicate" %in% colnames(file)) {
        replicate <- file$replicate
        names(replicate) <- file$file
    }

    if ("group" %in% colnames(file)) {
        replicate <- file$group
        names(replicate) <- file$file
    }

    if ("blank" %in% colnames(file)) {
        blank <- file$blank
        names(blank) <- file$file
    }

    file <- file$file
  }

  if ((length(replicate) == 1 & TRUE %in% is.na(replicate)) |
      length(replicate) != length(file)) {

    replicate <- rep(NA_character_, length(file))
    names(replicate) <- file
  }

  if (length(blank) == 1 & TRUE %in% is.na(blank) |
      length(replicate) != length(file)) {

    blank <- rep(NA_character_, length(file))
    names(blank) <- file
  }

  file <- file[checkFileValidity(file)]

  if (length(file) < 1) {

    # TODO suggest to convert files
    # if (TRUE %in% grepl(tools::file_ext(f), compatibleFileFormatsForConversion()$format)) {
    #   warning("MS vendor file found! Use the function convertFiles
    #           for conversion to mzML/mzXML. See ?convertFiles for more information.")
    # }

    warning("A valid file path should be added
            to create an analysis object!")

    return(NULL)

  } else {

    file_df <- data.frame(
      "file" = file,
      "replicate" = replicate,
      "blank" = blank,
      row.names = NULL
    )

    return(file_df)
  }
}



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
#' \code{file}, a \linkS4class{streamSet} object is returned instead.
#'
#' @export
#'
newAnalysis <- function(file = NA_character_,
                        replicate = NA_character_,
                        blank = NA_character_) {

  file_df <- checkFilesInput(file, replicate, blank)

  if (is.null(file_df)) return(NULL)

  if (nrow(file_df) > 1) {
    message("More than one file, creating a streamSet instead! \n")
    return(newStreamProject(files = file_df))
  }

  if (grepl("mzML", file_df$file) | grepl("mzXML", file_df$file)) {
    analysis <- msAnalysis_loadMetadata(file_df)
    if (is.list(analysis)) analysis <- analysis[[1]]
  }

  # TODO implement further file types check-ups,
  # such as for ramanAnalysis or uvAnalysis,
  # as well for other MS file formats to implement convert functions


  return(analysis)
}



#' @title newStreamSet
#'
#' @description Creates a new \pkg{streamFind} set of analyses.
#'
#' @template args-newStreamSet-files
#' @template args-newStreamSet-path-title-date
#' @template args-newStreamSet-replicates-blanks
#' @param makeNewProject Logical, set to \code{TRUE} to create an R project
#' in the given \code{path} and open a new R session.
#'
#' @note The format of the files added will dictate the subclass of
#' the \linkS4class{streamSet}. For instance, \emph{.mzML} or \emph{.mzXML}
#' files will lead to the subclass \linkS4class{msData}, which will contain
#' an \linkS4class{msAnalysis} for each file added.
#'
#' @return A \linkS4class{streamSet} with a subclass depending on the added
#' file formats. For instance, an \linkS4class{msData} is returned for
#' \emph{mzML} and \emph{mzXML} files. when files from different types are mixed
#' a subclass is not defined, returning the \linkS4class{streamSet} object
#' with the list of file paths in the \code{analyses} slot.
#'
#' @export
#'
newStreamSet <- function(files = NA_character_,
                         path = getwd(),
                         title = NA_character_,
                         date = Sys.Date(),
                         replicates = NA_character_,
                         blanks = NA_character_,
                         makeNewProject = FALSE) {

  file_df <- checkFilesInput(files, replicates, blanks)

  if (is.null(file_df)) return(NULL)

  object <- new("streamSet")
  object@title <- title
  object@date <- date
  object@path <- path

  if (all(grepl(".mzML|.mzXML", file_df$file))) {

    analyses <- msAnalysis_loadMetadata(file_df)

    ana_type <- sapply(analyses, function(x) is(x))

    if (all(ana_type %in% "msAnalysis")) {

      object <- new("msData", object)
      object@analyses <- analyses

    } else {

      warning("More than one file type was added!
              Not possible to assign a set sub-class")
      return(NULL)
    }

  # TODO add other else if() conditions for other file types
  } else {

    warning("More than one file type was added!
            Not possible to assign a set sub-class.")

    return(NULL)
  }

  return(object)
}
