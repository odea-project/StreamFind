
files <- list.files("F:/", full.names = TRUE)
files <- files[1:2]
r1 <- ramanData$new(files)

View(r1$get_analyses())



