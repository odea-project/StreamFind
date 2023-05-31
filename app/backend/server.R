
library(tools)

# DEMO TODOS:
# TODO: Save stuff in sessions --> add to cache.
# TODO: Create docker containers for server and frontend --> create image

#*@filter cors
cors <- function(req, res) {

  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }

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
  filesx <- list.files(path = "/Users/ammar/Desktop/streamFind", pattern = "\\.mzML$", full.names = TRUE, recursive = FALSE)
  folders<- list.dirs(path = "/Users/ammar/Desktop/streamFind", full.names = TRUE, recursive = FALSE)
  file_names <- basename(filesx)
  folder_names <- basename(folders)
  filesandfolders<-c(file_names,folder_names)
  return(filesandfolders)
}

#* @post /open_folder
function(req) {
  folder_name <- req$body$name
  print(folder_name)
  folder_path <- paste0("/Users/ammar/Desktop/streamFind/", folder_name)
  filesx <- list.files(path = folder_path, pattern = "\\.mzML$", full.names = TRUE, recursive = FALSE)
  folders<- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  file_names <- basename(filesx)
  folder_names <- basename(folders)
  filesandfolders<-c(file_names,folder_names)
  return(filesandfolders)
}


#* MsData for a given file
#* @get /msdata
function() {
  files <- streamFindData::msFilePaths()[1:3]
  ms <- streamFind::MassSpecData$new(files)
  # names = ms$get_analysis_names()
  return(unclass(files))
}
