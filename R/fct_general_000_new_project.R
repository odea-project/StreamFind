
checkFileValidity <- function(fl = NA_character_) {

  # TODO add other file formats (inc. Raman and UV)
  # and checkups for general evaluation
  fFormats <- ".mzML|.mzXML"

  check <- grepl(fFormats, fl) & is.character(fl)

  check <- check & file.exists(fl)

  return(check)
}



checkFilesInput <- function(files = NA_character_,
                            replicates = NA_character_,
                            blanks = NA_character_) {

  if (is.data.frame(files) | is.data.table(files)) {

    if ("path" %in% colnames(files) &
          !"file" %in% colnames(files) &
            "analysis" %in% colnames(files)) {


      f_path_file <- apply(files, 1, function(x) {
        list.files(path = x["path"],
                   pattern = x["analysis"],
                   full.names = TRUE)
      })

      if (length(f_path_file) > nrow(files)) {
        f_path_file <- f_path_file[checkFileValidity(f_path_file)]
      }

      if (nrow(files) == length(f_path_file)) {
        files$file <- sapply(files$analysis, function(x, f_path_file) {
          return(f_path_file[grep(x, f_path_file)])
        }, f_path_file = f_path_file)

      } else {
        warning("Files with same name but different valid formats present!")
        return(NULL)
      }
    }

    if ("replicate" %in% colnames(files)) {
        replicates <- file$replicate
        names(replicates) <- files$file
    }

    if ("group" %in% colnames(files)) {
        replicates <- files$group
        names(replicates) <- files$file
    }

    if ("blank" %in% colnames(files)) {
        blank <- files$blank
        blank <- gsub("", NA_character_, blank)
        names(blank) <- files$file

    }

    files_v <- files$file

  } else {
    files_v <- files
  }

  if ((length(replicates) == 1 & TRUE %in% is.na(replicates)) |
      length(replicates) != length(files_v)) {

    replicates <- file_path_sans_ext(basename(files_v))
    replicates <- gsub( "-", "_", replicates)
    replicates <- sub("_[^_]+$", "", replicates)
    names(replicates) <- files_v
  }

  if (length(blanks) == 1 & TRUE %in% is.na(blanks) |
      length(blanks) != length(files)) {

    blanks <- rep(NA_character_, length(files_v))
    names(blanks) <- files_v
  }

  files_v <- files_v[checkFileValidity(files_v)]

  if (length(files_v) < 1) {

    warning("A valid file path should be added
            to create an analysis object!")

    return(NULL)

  } else {

    file_df <- data.table(
      "file" = files_v,
      "replicate" = replicates,
      "blank" = blanks,
      keep.rownames = FALSE
    )

    return(file_df)
  }
}



#' @title newAnalysis
#'
#' @description Creates a \pkg{streamFind} analysis object.
#'
#' @template args-newAnalysis-file
#' @template args-newAnalysis-replicate-blank
#'
#' @note The format of the file will dictate the resulting S4 class.
#' For instance, \emph{.mzML} or \emph{.mzXML} files will lead to the class
#' \linkS4class{msAnalysis}, which will contain the structure for
#' handling MS data. Also, if more than one file is given as
#' \code{file}, a \linkS4class{streamSet} object is returned instead.
#'
#' @return An analysis object with S4 class dependent on the added file format.
#' For instance, an \linkS4class{msAnalysis} object is returned for
#' \emph{mzML} and \emph{mzXML} files.
#'
#' @export
#'
newAnalysis <- function(file = NA_character_) {

  file_df <- checkFilesInput(file)

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
#' @description Creates a \linkS4class{streamSet} with analyses.
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
#' an \linkS4class{msAnalysis} for each file added. when files from different
#' types are mixed a subclass is not defined, returning the
#' \linkS4class{streamSet} object with the list of file paths in the
#' \code{analyses} slot.
#'
#' @return A \linkS4class{streamSet} with a subclass depending on the added
#' file formats.
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

  if (all(grepl(".mzML|.mzXML", file_df$file))) {

    analyses <- msAnalysis_loadMetadata(file_df)

    ana_type <- sapply(analyses, function(x) is(x))

    if (all(ana_type %in% "msAnalysis")) {

      object <- new("msData", object)

      object@analyses <- analyses

      object@features@analyses <- data.table(
        file = filePaths(object),
        analysis = names(analyses)
      )

      object@features@analyses$replicate <- file_df$replicate[
        file_df$file %in% filePaths(object)
      ]

      object@features@analyses$blank <- file_df$blank[
        file_df$file %in% filePaths(object)
      ]

      object@features@analyses$class <- ana_type

      object@features@analyses$polarity <- polarities(object)

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
