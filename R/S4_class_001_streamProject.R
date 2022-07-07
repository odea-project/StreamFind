

### streamProject ----------------------------------------------------------------------------------------------

#' @title streamProject
#'
#' @description An S4 class object containing the project title, date, path and added analyses.
#'  The \code{streamProject} is the most basic class of the \pkg{streamFind} package.
#'
#' @template slot-streamProject
#' @slot analyses A list of analyses added to the project.
#' The class of the analysis objects are defined according to the file formats.
#'
#' @export
#'
#' @md
setClass("streamProject",
  representation(
    analyses = "list",
    path = "character",
    date = "Date",
    project = "character"
  ),
  prototype = list(
    analyses = list(),
    path = NA_character_,
    date = Sys.Date(),
    project = NA_character_
  )
)


### S4 methods ----------------------------------------------------------------------------------------------

#### show ----------------------------------------------------------------

#' @describeIn streamProject prints the details of an \linkS4class{streamProject} object.
#'
#' @param object An \linkS4class{streamProject} object.
#'
#' @export
#'
setMethod("show", "streamProject", function(object) {

  cat(
    "  Class         ", is(object), "\n",
    "  Project       ", object@project, "\n",
    "  Date          ", as.character(object@date), "\n",
    "  Path          ", object@path, "\n",
    "  ", length(object@analyses) ," analyses:  \n",
    sep = ""
  )
  if (length(object@analyses) > 0) {
    for (i in seq_len(length(object@analyses))) {
      cat("      - ", object@analyses[i], " (", is(object@analyses[[i]])  , ") \n", sep = "")
    }
  } else {
    cat("     n.a.", "\n", sep = "")
  }

})

#### [ sub-setting analyses ----------------------------------------------

#' @describeIn streamProject subset on analysis, using analysis index or name.
#'
#' @param i The indice/s or name/s of the analysis to keep in the \code{x} object.
#'
#' @export
#'
setMethod("[", c("streamProject", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {
    if (!is.character(i)) {
      sname <- analyses(x)[i]
      sidx <- i
    } else {
      if (FALSE %in% (i %in% analyses(x))) {
        warning("Given analysis name/s not found in the msData object.")
        return(x)
      }
      sname <- i
      sidx <- which(analyses(x) %in% sname)
    }
    x@analyses <- x@analyses[sidx]
  }
  return(x)
})

#### projectInfo ---------------------------------------------------------

#' @describeIn streamProject setter and getter for project title, date and path.
#' When the \code{projectTitle} and \code{projectDate} arguments
#' are missing it returns a list with the project title, date and path.
#' If arguments are given, the project title and date are updated
#' based on the specified arguments, returning the original class object.
#'
#' @param projectTitle A character string to be used as project title.
#' @param projectDate The \link{Date} of the project.
#'
#' @export
#'
#' @aliases projectInfo,streamProject,streamProject-method
#'
setMethod("projectInfo", "streamProject", function(object, projectTitle = NULL, projectDate = NULL) {

  if (missing(projectTitle) & missing(projectDate)) {
    return(list(project = object@project, date = object@date, path = object@path))
  }

  if (!missing(projectTitle) & !is.null(projectTitle)) object@project <- projectTitle
  if (!missing(projectDate) & !is.null(projectDate)) object@date <- as.Date(projectDate)
  return(object)
})

#### path ----------------------------------------------------------------

#' @describeIn streamProject getter for project path.
#'
#' @export
#'
#' @aliases path,streamProject,streamProject-method
#'
setMethod("path", "streamProject", function(object) object@path)

#### analysisInfo -------------------------------------------------------

#' @describeIn streamProject getter for analysis info as \link{data.frame} with
#' four columns: path, analysis, group and blank. The \link{data.frame}
#' can be used as analysisInfo in \pkg{patRoon}.
#'
#' @export
#'
#' @importMethodsFrom patRoon analysisInfo
#'
#' @aliases analysisInfo,streamProject,streamProject-method
#'
setMethod("analysisInfo", "streamProject", function(obj) {
  temp <- data.frame(
    "path" = sapply(obj@analyses, function(x) dirname(x@file)),
    "analysis" = sapply(obj@analyses, function(x) x@analysis),
    "group" = sapply(obj@analyses, function(x) x@replicate),
    "blank" = sapply(obj@analyses, function(x) x@blank),
    "class" = sapply(obj@analyses, function(x) is(x)),
    "file" = sapply(obj@analyses, function(x) x@file))

  rownames(temp) <- seq_len(nrow(temp))
  return(temp)
})

#### analysisTable -------------------------------------------------------

#' @describeIn streamProject getter for analysis table as \link{data.table} with
#' four columns: file, analysis, replicate and blank.
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @aliases analysisTable,streamProject,streamProject-method
#'
setMethod("analysisTable", "streamProject", function(object) {
  temp <- data.table(
    "file" = sapply(object@analyses, function(x) x@file),
    "analysis" = sapply(object@analyses, function(x) x@analysis),
    "replicate" = sapply(object@analyses, function(x) x@replicate),
    "blank" = sapply(object@analyses, function(x) x@blank)
  )
  rownames(temp) <- seq_len(nrow(temp))
  return(temp)
})

#### files -----------------------------------------------------------

#' @describeIn streamProject getter for analysis file paths.
#'
#' @export
#'
#' @aliases files,streamProject,streamProject-method
#'
setMethod("files", "streamProject", function(object) sapply(object@analyses, function(x) x@file))

#### analyses ------------------------------------------------------------

#' @describeIn streamProject getter for analysis names.
#'
#' @export
#'
#' @importMethodsFrom patRoon analyses
#'
#' @aliases analyses,streamProject,streamProject-method
#'
setMethod("analyses", "streamProject", function(obj) sapply(obj@analyses, function(x) x@analysis))

#### replicates ----------------------------------------------------------

#' @describeIn streamProject getter for replicate names.
#'
#' @export
#'
#' @aliases replicates,streamProject,streamProject-method
#'
setMethod("replicates", "streamProject", function(object) sapply(object@analyses, function(x) x@replicate))

#### replicates<- --------------------------------------------------------

#' @describeIn streamProject setter for analysis replicate names.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing analysis replicate name for each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases replicates<-,streamProject,streamProject-method
#'
setMethod("replicates<-", signature("streamProject", "ANY"), function(object, value) {

  ana <- analyses(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  names(value) <- ana
  for (a in ana) object@analyses[[a]]@replicate <- unname(value[a])
  return(object)
})

#### blanks --------------------------------------------------------------

#' @describeIn streamProject getter for blank names.
#'
#' @export
#'
#' @aliases blanks,streamProject,streamProject-method
#'
setMethod("blanks", "streamProject", function(object) sapply(object@analyses, function(x) x@blank))

#### blanks<- ------------------------------------------------------------

#' @describeIn streamProject setter for associated blank replicate for each analyses.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing the associated blank replicate name of each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases blanks<-,streamProject,streamProject-method
#'
setMethod("blanks<-", signature("streamProject", "ANY"), function(object, value) {

  ana <- analyses(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  names(value) <- ana
  for (a in ana) object@analyses[[a]]@blank <- unname(value[a])
  return(object)
})

#### metadata ---------------------------------------------------------

#' @describeIn streamProject getter for analysis metadata.
#' Returns a \link[data.table]{date.table} with a row per analysis.
#'
#' @template args-single-which-entry
#'
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @aliases metadata,streamProject,streamProject-method
#'
setMethod("metadata", "streamProject", function(object, analyses = NULL, which = NULL) {

  if (!is.null(analyses)) object <- object[analyses]

  mtd <- lapply(object@analyses, function(x, which) {
    return(metadata(x, which))
  }, which = which)

  mtd <- data.table::rbindlist(mtd)

  return(mtd)
})
