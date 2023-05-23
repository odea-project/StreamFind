library(streamFind)

# DEMO TODOS:
# TODO: Save stuff in sessions --> add to cache.
# TODO: Create docker containers for server and frontend --> create image
#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin","*")
  plumber::forward()
}

#* Echo back the input
#* @get /files
function() {
  # get files by ...
  files <- streamFindData::msFilePaths()[1:3]
  return(unclass(files))
}


#* @get /files_project
function() {
  files <- list.files(path = "D:/work/streamFind/app/backend/sample mzml", pattern = ".mzML", full.names = TRUE)
  file_names <- basename(files)
  return(file_names)
}


#* MsData for a given file
#* @get /msdata
function() {
  files <- streamFindData::msFilePaths()[1:3]
  ms <- streamFind::MassSpecData$new(files)
  # names = ms$get_analysis_names()
  return(unclass(files))
}

