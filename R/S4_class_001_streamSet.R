

### streamSet ----------------------------------------------------------------------------------------------

#' @title streamSet
#'
#' @description An S4 class object containing the set title, date,
#' storage path and a list of analysis files.
#'
#' @template slot-streamSet
#' @slot analyses A list of analyses added to the project.
#' The class of the analysis objects are defined according to the file formats.
#' When a class is not possible to be assigned, a list of file paths.
#'
#' @export
#'
#' @md
setClass("streamSet",
  representation(
    analyses = "list",
    path = "character",
    date = "Date",
    title = "character"
  ),
  prototype = list(
    analyses = list(),
    path = NA_character_,
    date = Sys.Date(),
    title = NA_character_
  )
)


### S4 methods ----------------------------------------------------------------------------------------------

#### show ----------------------------------------------------------------

#' @describeIn streamSet prints the details of an \linkS4class{streamSet} object.
#'
#' @param object An \linkS4class{streamSet} object.
#'
#' @export
#'
setMethod("show", "streamSet", function(object) {

  sInfo <- setInfo(object)

  cat(
    "  Class         ", is(object), "\n",
    "  Title         ", sInfo$title, "\n",
    "  Date          ", as.character(sInfo$date), "\n",
    "  Path          ", path(object), "\n",
    "  ", length(object@analyses) ," analyses:  \n",
    sep = ""
  )
  if (length(object@analyses) > 0) {

    for (i in seq_len(length(object@analyses))) {

      cat("      - ", object@analyses[i],
          " (", is(object@analyses[[i]])  , ") \n", sep = "")
    }

  } else {

    cat("     n.a.", "\n", sep = "")
  }

})

#### [ sub-setting analyses ----------------------------------------------

#' @describeIn streamSet subset on analyses, using analysis index or name.
#'
#' @param i The indice/s or name/s of the analyses to keep in the \code{x} object.
#'
#' @export
#'
setMethod("[", c("streamSet", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {
    if (!is.character(i)) {
      sname <- analysisNames(x)[i]
      sidx <- i
    } else {
      if (FALSE %in% (i %in% analysisNames(x))) {
        warning("Given analysis name/s not found in the object.")
        return(x)
      }
      sname <- i
      sidx <- which(analysisNames(x) %in% sname)
    }

    x@analyses <- x@analyses[sidx]
  }
  return(x)
})

#### setInfo ---------------------------------------------------------

#' @describeIn streamSet setter and getter for set title, date and path.
#' When the \code{title} and \code{date} arguments
#' are missing it returns a list with the set title, date and path.
#' If arguments are given, the set title and date are updated
#' based on the specified arguments, returning the original class object.
#'
#' @param title A character string to be used as set title.
#' @param date The \link{Date} of the set.
#'
#' @export
#'
#' @aliases setInfo,streamSet,streamSet-method
#'
setMethod("setInfo", "streamSet", function(object, title = NULL, date = NULL) {

  if (missing(title) & missing(date)) {
    return(list(title = object@title, date = object@date, path = object@path))
  }

  if (!missing(title) & !is.null(title)) object@project <- title
  if (!missing(date) & !is.null(date)) object@date <- as.Date(date)
  return(object)
})

#### path ----------------------------------------------------------------

#' @describeIn streamSet getter for set path.
#'
#' @importMethodsFrom BiocGenerics path
#'
#' @export
#'
#' @aliases path,streamSet,streamSet-method
#'
setMethod("path", "streamSet", function(object) object@path)

#### analysisNames ----------------------------------------------------------------

#' @describeIn streamSet getter for the analysis names.
#'
#' @export
#'
#' @aliases analysisNames,streamSet,streamSet-method
#'
setMethod("analysisNames", "streamSet", function(object) {

  return(names(object@analyses))

})

#### addAnalyses ---------------------------------------------------------

#' @describeIn streamSet getter for analysis names.
#'
#' @param analysisList A list of named analyses paths as character string.
#'
#' @export
#'
#' @aliases addAnalyses,streamSet,streamSet-method
#'
setMethod("addAnalyses", "streamSet", function(object, analysisList = NULL) {

  if (is.list(analysisList)) {

    lengths <- sapply(analysisList, function(x) length(x))
    cls <- sapply(analysisList, function(x) class(x))

    if (all(cls %in% "character" & lengths == 1)) {

      if (is.null(names(analysisList))) {
        names(analysisList) <- sapply(analysisList, function(x) {
          basename(tools::file_path_sans_ext(x))
        })
      }

      #check name
      if (TRUE %in% (names(analysisList) %in% analysisNames(object))) {
        warning("One or more given file names are already in the streamSet!")
        return(object)
      }

      object@analyses <- c(object@analyses, analysisList)
      object@analyses <-  object@analyses[order(names(object@analyses))]

      return(object)
    }
  }

  warning("No list of analysis files given to add!")
  return(object)
})
