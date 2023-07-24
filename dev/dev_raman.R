
files <- list.files("D:/Jana/", full.names = TRUE)
files <- files[1:2]


r1 <- RamanData$new(files, headers = list(
  "name" = "Example files",
  "description" = "Test Raman class."
))

r1


r1$get_spectra(shift = c(300, 100))

plot(r1$get_spectra(shift = c(300, 100))[, 2:3])



plot(r1$get_spectra(2)[1:200, 2:3])

View(r1$get_analyses())






