library(streamFind)

# DEMO TODOS:
# TODO: Save stuff in sessions --> add to cache.
# TODO: Create docker containers for server and frontend --> create image

#* Echo back the input
#* @get /files
function() {
  # get files by ...
  files <- streamFindData::msFilePaths()[1:3]
  return(unclass(files))
}

#* MsData for a given file
#* @get /msdata
function() {
  files <- streamFindData::msFilePaths()[1:3]
  ms <- streamFind::MassSpecData$new(files)
  # names = ms$get_analysis_names()
  return(unclass(files))
}

