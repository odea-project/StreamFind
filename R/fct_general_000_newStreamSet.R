

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
#' @note The format of the files added will dictate the subclass of the \linkS4class{streamSet}.
#' For instance, \emph{.mzML} or \emph{.mzXML} files will lead to the subclass
#' \linkS4class{msData}, which will contain an \linkS4class{msAnalysis} for each file.
#'
#' @return A \linkS4class{streamSet} with a subclass depending on the added
#' file formats. For instance, an \linkS4class{msData} is returned for
#' \emph{mzML} and \emph{mzXML} files. when files from different types are mixed
#' a subclass is not defined, returning the \linkS4class{streamSet} object with a list of files.
#'
#' @export
#'
#' @importFrom data.table data.table is.data.table
#'
newStreamSet <- function(files = NA_character_,
                         path = getwd(),
                         title = NA_character_,
                         date = Sys.Date(),
                         replicates = NULL,
                         blanks = NULL,
                         makeNewProject = FALSE) {

  if (TRUE %in% is.na(files)) {
    return(warning("At least one file should be added to create a stream project!"))
  }

  proj <- new("streamSet")
  proj@title <- title
  proj@date <- date
  proj@path <- path

  if (is.data.frame(files) | is.data.table(files)) {

    #check if path and analysis are given instead of file name
    if ("path" %in% colnames(files) & !"file" %in% colnames(files) & "analysis" %in% colnames(files)) {

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

  if (is.null(replicates)) {
    replicates <- rep(NA_character_, length(files))
    names(replicates) <- files
  }

  if (is.null(blanks)) {
    blanks <- rep(NA_character_, length(files))
    names(blanks) <- files
  }

  analyses <- list()

  cat("Loading analysis files... \n")
  pb <- txtProgressBar(
    min = 0,
    max = length(files),
    style = 3,
    width = 50,
    char = "+"
  )

  for (f in files) {

    if (grepl("mzML", f) | grepl("mzXML", f)) {
      analyses[[gsub(".mzML|.mzXML", "", basename(f))]] <- new("msAnalysis", file = f, replicate = unname(replicates[f]), blank = unname(blanks[f]))
    }

    # TODO check for raw format of MS files for asking for conversion

    if (TRUE %in% grepl(tools::file_ext(f), compatibleFileFormatsForConversion()$format)) {
      warning("MS vendor file found! Use the function convertFiles
              for conversion to mzML/mzXML. See ?convertFiles for more information.")
    }

    # TODO implement further file types check-ups, such as for ramanAnalysis or uvAnalysis

    setTxtProgressBar(pb, which(f == files))
  }

  cat(" Done! \n")
  close(pb)

  proj@analyses <- analyses

  #check if a single sample type was found
  an_type <- unique(sapply(analyses, function(x) is(x)))
  if (length(an_type) > 1) {
    proj@analyses <- files
    warning("More than one file type was added! Not possible to assign a set sub-class.")
    return(proj)
  }

  if ("msAnalysis" %in% an_type) {
    object <- new("msData", proj)
    object@analyses <- object@analyses[sort(names(object@analyses), decreasing = FALSE)]

  } else {
    proj@analyses <- files
    warning("File type was not recognized! Not possible to assign a set sub-class.")
    return(proj)
  }

  return(object)
}
