source("../../R/class_R6_msData.R")

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Echo back the input
#* @get /test
function() {
  ms <- msData$new()
  overview = ms$get_blank_names()
  return(overview)
}
