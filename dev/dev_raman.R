
files <- StreamFindData::get_raman_file_paths()

r1 <- Raman$new(files)

r1

r1$add_replicate_names(c(rep("Sample", 11), rep("Blank", 11)))

r1$add_blank_names(rep("Blank", 22))

r1$get_spectra(analyses = 1)

r1$plot_spectra(colorBy = "replicates", interactive = TRUE)

# plot(r1$get_spectra(analyses = 1)[, 2:3], type = "l")
# 
# plot(r1$get_spectra(2)[1:200, 2:3])
# 
# View(r1$get_analyses())






