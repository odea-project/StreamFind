library(streamFind)
# source("../../R/class_R6_msData.R")

#* Echo back the input
#* @get /connect
function() {
  files <- streamFindData::msFilePaths()[1:3]
  ms <- msData$new(files)
  names = ms$get_analysis_names()
  return(unclass(names))
} # TODO: look into install patRoon from fork
