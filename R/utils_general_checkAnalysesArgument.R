
checkAnalysesArgument <- function(object, analyses) {

  if (is.character(analyses)) {
    if (FALSE %in% (analyses %in% analyses(object))) {
      warning("Given analysis names not found in the msData object!")
      return(analyses(object))
    }
    analyses <- analyses(object)[analyses(object) %in% analyses]

  } else if (is.numeric(analyses)) {

    if (max(analyses) > length(analyses(object))) {
      warning("Analyses index not matching the number of analyses in the object!")
      return(analyses(object))
    }
    analyses <- analyses(object)[analyses]

  } else {

    return(analyses(object))
  }
}
