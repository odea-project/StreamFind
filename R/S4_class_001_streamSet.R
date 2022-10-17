

### streamSet ----------------------------------------------------------------------------------------------

#' @title streamSet
#'
#' @description An S4 class object containing the set title, date,
#' storage path and a list of analysis files.
#'
#' @template slot-streamSet
#' @slot analyses A list of analyses added to the project.
#' The class of the analysis objects are defined according to the file formats.
#' When a class is not possible to be assigned, the list contains
#' full path string of each file added.
#'
#' @export
#'
#' @md
setClass("streamSet",
  representation(
    analyses = "list",
    date = "Date",
    title = "character"
  ),
  prototype = list(
    analyses = list(),
    date = Sys.Date(),
    title = NA_character_
  )
)


### S4 methods ----------------------------------------------------------------

#### show ---------------------------------------------------------------------

#' @describeIn streamSet prints the details of
#' an \linkS4class{streamSet} object.
#'
#' @param object An \linkS4class{streamSet} object.
#'
#' @export
#'
setMethod("show", "streamSet", function(object) {

  cat(
    "  Class         ", is(object), "\n",
    "  Title         ", setTitle(object), "\n",
    "  Date          ", as.character(setDate(object)), "\n",
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



#### setTitle -----------------------------------------------------------------

#' @describeIn streamSet getter for the title.
#'
#' @export
#'
#' @aliases setTitle,streamSet,streamSet-method
#'
setMethod("setTitle", "streamSet", function(object) {
  return(object@title)
})



#### setTitle<- ---------------------------------------------------------------

#' @describeIn streamSet setter for title.
#' The \code{value} is a string with the title.
#'
#' @param value A method specific string/vector.
#'
#' @export
#'
#' @aliases setTitle<-,streamSet,streamSet-method
#'
setMethod("setTitle<-", signature("streamSet", "ANY"), function(object, value) {

  object@title <- value[1]

  return(object)
})



#### setDate ------------------------------------------------------------------

#' @describeIn streamSet getter for the date.
#' Note that is not setter for the date.
#'
#' @export
#'
#' @aliases setDate,streamSet,streamSet-method
#'
setMethod("setDate", "streamSet", function(object) {

  return(object@date)

})



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



#### [ sub-setting analyses ---------------------------------------------------

#' @describeIn streamSet subset on analyses, using analysis index or name.
#'
#' @param x A \linkS4class{streamSet} object.
#' @param i The indice/s or name/s of the analyses to keep in \code{x}.
#' @param j Not applicable to \linkS4class{streamSet}.
#' @param drop Not applicable to \linkS4class{streamSet}.
#' @param ... Other arguments.
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



#### addAnalyses ---------------------------------------------------------

#' @describeIn streamSet adds analyses to the \linkS4class{streamSet}.
#'
#' @param analysisList A list of strings with analysis file full paths.
#'
#' @export
#'
#' @aliases addAnalyses,streamSet,streamSet-method
#'
setMethod("addAnalyses", "streamSet", function(object, analysisList = NULL) {

  # TODO improve with method for file paths only

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
