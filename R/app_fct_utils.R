
#' @noRd
.get_volumes <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    drives <- system("wmic logicaldisk get name", intern = TRUE)
    drives <- drives[grepl(":", drives)]
    drives <- gsub("\\s+", "", drives)
    names(drives) <- drives
  } else {
    drives <- list.files("/media", full.names = TRUE)
    names(drives) <- basename(drives)
    if (length(drives) == 0) {
      drives <- list.files("/mnt", full.names = TRUE)
      names(drives) <- basename(drives)
    }
  }
  c("wd" = getwd(), drives)
}
