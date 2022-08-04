

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
#' @importFrom data.table data.table is.data.table
#' @importFrom BiocParallel bplapply SerialParam SnowParam
#'
newStreamSet <- function(files = NA_character_,
                         path = getwd(),
                         title = NA_character_,
                         date = Sys.Date(),
                         replicates = NULL,
                         blanks = NULL,
                         makeNewProject = FALSE) {

  if (is.data.frame(files) | is.data.table(files)) {

    #add file column, for enabling the use of analysisInfo from patRoon
    if ("path" %in% colnames(files) &
                  !"file" %in% colnames(files) &
                            "analysis" %in% colnames(files)) {

      f_path_files <- apply(files, 1, function(x) {
        list.files(path = x["path"], pattern = x["analysis"], full.names = TRUE)
      })

      if (length(f_path_files) > nrow(files)) {
        p_formats <- c("mzML", "mzXML")
        f_path_files <- f_path_files[tools::file_ext(f_path_files) %in% p_formats]
      }

      files$file <- f_path_files
    }

    if ("replicate" %in% colnames(files)) {
      replicates <- files$replicate
      names(replicates) <- files$file
    }

    if ("group" %in% colnames(files)) {
      replicates <- files$group
      names(replicates) <- files$file
    }

    if ("blank" %in% colnames(files)) {
      blanks <- files$blank
      names(blanks) <- files$file
    }

    files <- files$file
  }

  files <- files[file.exists(files)]

  fFormats <- ".mzML|.mzXML"

  files <- files[grepl(fFormats, files)]

  if (length(files) < 1) {

    # TODO suggest to convert files
    # if (TRUE %in% grepl(tools::file_ext(f), compatibleFileFormatsForConversion()$format)) {
    #   warning("MS vendor file found! Use the function convertFiles
    #           for conversion to mzML/mzXML. See ?convertFiles for more information.")
    # }

    warning("At least one valid file paths should be added
            to create a stream project!")

    return(new("streamSet"))
  }

  object <- new("streamSet")
  object@title <- title
  object@date <- date
  object@path <- path

  if (is.null(replicates) |
                  length(replicates) != length(files) |
                                          is.character(replicates)) {

    replicates <- rep(NA_character_, length(files))
    names(replicates) <- files
  }

  if (is.null(blanks) |
                length(blanks) != length(files) |
                                              is.character(blanks)) {

    blanks <- rep(NA_character_, length(files))
    names(blanks) <- files
  }


  if (all(grepl(".mzML|.mzXML", files))) {

    # TODO check if number of files and workers are high enough to add parallel processing

    cat("Loading analysis files... \n")

    analyses <- BiocParallel::bplapply(files, function(x, replicates, blanks) {
      rpl <- unname(replicates[x])
      blk <- unname(blanks[x])

      ana <- new("msAnalysis", file = x, replicate = rpl, blank = blk)
    },
    BPPARAM = SerialParam(progressbar = TRUE),
    replicates = replicates,
    blanks = blanks)

    names(analyses) <- sapply(analyses, FUN = function(x) analysisNames(x))

    analyses <- analyses[sort(names(analyses), decreasing = FALSE)]

    ana_type <- sapply(analyses, function(x) is(x))

    if (all(ana_type %in% "msAnalysis")) {

      object <- new("msData", object)
      object@analyses <- analyses

    } else {
      object@analyses <- files
      warning("More than one file type was added!
              Not possible to assign a set sub-class.")
    }

  # TODO add other else if conditions for other file types

  } else {

    object@analyses <- files
    warning("More than one file type was added!
            Not possible to assign a set sub-class.")
  }

  return(object)
}
