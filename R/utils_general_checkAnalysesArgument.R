
checkAnalysesArgument <- function(object, analyses) {

  if (is.character(analyses)) {
    if (FALSE %in% (analyses %in% analysisNames(object))) {
      warning("Given analysis names not found in the msData object!")
      return(analysisNames(object))
    }
    analyses <- analysisNames(object)[analysisNames(object) %in% analyses]

  } else if (is.numeric(analyses)) {

    if (max(analyses) > length(analysisNames(object))) {
      warning("Analyses index not matching the number of analyses in the object!")
      return(analysisNames(object))
    }
    analyses <- analysisNames(object)[analyses]

  } else {

    return(analysisNames(object))
  }
}
