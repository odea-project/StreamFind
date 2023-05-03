library(streamFind)
# source("../../R/class_R6_msData.R")

# DEMO TODOS:
# TODO: Save stuff in sessions --> add to cache.
# TODO: Create docker containers for server and frontend --> create image

#* Echo back the input
#* @get /msData # TODO: add arguments
function() {
  files <- streamFindData::msFilePaths()[1:3]
  ms <- msData$new(files)
  names = ms$get_analysis_names()
  return(unclass(names))
}

#* Echo back the input
#* @get /files
function() {
  # get files by ...
  files <- streamFindData::msFilePaths()[1:3]
  return(unclass(files))
}

