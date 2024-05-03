
# files <- StreamFindData::get_raman_file_paths()

# wd <- "E:/iSoft"
wd <- "C:/Users/apoli/Documents/iSoft"


## BVCZ ---------

ant_dir <- paste0(wd, "/Bevacizumab_Avastin_LotB8703H40/LC-Raman/for Ricardo/Messung 3")

ant_files <- list.files(ant_dir, pattern = ".asc", full.names = TRUE)

ant_files <- list.files(ant_dir, pattern = "_\\d.asc", full.names = TRUE)

ant <- RamanEngine$new(ant_files, runParallel = FALSE)

ant$add_replicate_names(sub("_\\d+$", "", ant$get_analysis_names()))
# ant$add_replicate_names(sub("_(\\d)\\d*$", "_\\1", ant$get_analysis_names()))
# ant$merge_spectra_time_series()

ant$bin_spectra(Settings_bin_spectra_StreamFind(windowSpectrumUnits = 5))

ant$normalize_spectra(Settings_normalize_spectra_StreamFind(liftTozero = TRUE))

ant$subtract_spectra_section(Settings_subtract_spectra_section_StreamFind(sectionWindow = c(0, 3)))

ant$smooth_spectra(Settings_smooth_spectra_savgol(fl = 11, forder = 2, dorder = 0))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 330))))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(2000, 2600))))

ant$correct_spectra_baseline(Settings_correct_spectra_baseline_baseline(method = "als", args = list(lambda = 3, p = 0.02, maxit = 20)))

ant$normalize_spectra(Settings_normalize_spectra_StreamFind(liftTozero = TRUE))

ant$plot_spectra(rt = c(5, 8))


View(ant$get_results("spectra"))


# ant$plot_chromatograms()

 # TODO





# ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(10, max(e_sec$get_spectra()$rt)))))





ant$plot_chromatograms()

ant$plot_spectra(rt = c(0, 4))
ant$plot_spectra(rt = c(5, 8))
ant$plot_spectra_baseline(rt = c(5, 8))
ant$plot_spectra(rt = c(11, 14))

# ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(0, 10))))


ant$plot_spectra(rt = c(10, 13))
ant$plot_spectra(rt = c(13, 14.5))
ant$plot_spectra(rt = c(14.5, 16))

# ant$averaged_spectra

ant$plot_spectra_baseline(rt = c(10, 13), interactive = T)

ant$plot_spectra_baseline(interactive = F)






## Cuvette -----

wd <- "C:/Users/apoli/Documents/iSoft"

cuvette_dir <- paste0(wd, "/Jana_cuvette_serum")

cuvette_files <- list.files(cuvette_dir, pattern = "shiftadded", full.names = TRUE)

cuv <- RamanEngine$new(cuvette_files, runParallel = FALSE)

cuv$add_replicate_names(sub("_\\d+$", "", cuv$get_analysis_names()))

cuv$add_blank_names(
  c(
    rep("shiftadded_220726_H2O_100mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_20mW_0,1s_900n", 3),
    rep("shiftadded_220726_H2O_50mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_100mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_20mW_0,1s_900n", 3),
    rep("shiftadded_220726_H2O_50mW_0,1s_300n", 3)
  )
)

cuv$average_spectra()

cuv$subtract_blank_spectra()

cuv$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 470))))

cuv$smooth_spectra(Settings_smooth_spectra_savgol(fl = 11, forder = 2, dorder = 0))

cuv$correct_spectra_baseline(Settings_correct_spectra_baseline_baseline(method = "als", args = list(lambda = 3, p = 0.03, maxit = 10)))

cuv$correct_spectra_baseline(Settings_correct_spectra_baseline_airpls(lambda = 5))

cuv$normalize_spectra(Settings_normalize_spectra_StreamFind())


cuv$raw_spectra

is(Settings_normalize_spectra_StreamFind())

cuv$plot_spectra()

View(cuv$get_results("spectra"))














# anasl <- e_cuv$get_analyses()
# 
# for (i in e_cuv$get_analysis_names()) {
#   
#   ana_name <- i
#   
#   ana_dir <- dirname(anasl[[i]]$file)
#   
#   ana_ext <- file_ext(anasl[[i]]$file)
#   
#   new_file <- paste0(ana_dir, "/", "shiftadded_", ana_name, ".", ana_ext)
#   
#   ana_metadata <- anasl[[i]]$metadata
#   
#   if (file.exists(new_file)) file.remove(new_file)
#   
#   spectra <- anasl[[i]]$spectra
#   
#   spectra$shift <- shiftVals
#   
#   rcpp_write_asc_file(file = new_file, ana_metadata, as.matrix(spectra))
# }

# shiftVals <- e_sec$get_spectra(analyses = 1, rt = c(433.322, 433.324))$shift





## SEC -----

flowrate = 0.35 #ml/min

sec_dir <- paste0(wd, "/Jana_SEC_serum")

sec_files <- list.files(sec_dir, pattern = "_\\d.asc", full.names = TRUE)

e_sec <- RamanEngine$new(sec_files[2:3], runParallel = FALSE)

e_sec$add_replicate_names(sub("_\\d+$", "", e_sec$get_analysis_names()))
# e_sec$add_replicate_names(sub("_(\\d)\\d*$", "_\\1", e_sec$get_analysis_names()))
# e_sec$merge_spectra_time_series()

e_sec$bin_spectra(Settings_bin_spectra_StreamFind(windowSpectrumUnits = 5))

e_sec$normalize_spectra()

e_sec$subtract_spectra_section(Settings_subtract_spectra_section_StreamFind(sectionWindow = c(10, 30)))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 900))))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(0, 435.241 - 5))))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(435.241 + 5, max(e_sec$get_spectra()$rt)))))

e_sec$smooth_spectra(Settings_smooth_spectra_StreamFind(windowSize = 3))

e_sec$correct_spectra_baseline(
  Settings_correct_spectra_baseline_StreamFind(
    method = "als", args = list(lambda = 5, p = 0.05, maxit = 10)
  )
)

View(e_sec$get_results("spectra"))

View(e_sec$averaged_spectra)

specblk <- e_sec$get_spectra(rt = c(20, 200))
specblk <- specblk[, .(intensity = mean(intensity)), by = "shift"]

specpk <- e_sec$get_spectra(rt = c(435 - 2.5, 435 + 2.5))
specpk <- specpk[, .(intensity = mean(intensity)), by = "shift"]

plot(specblk$intensity, type = 'l')
lines(specpk$intensity, col = "red")

e_sec$plot_spectra(rt = c(435 - 2.5, 435 + 2.5))

e_sec$plot_chromatograms()

bsa <- e_sec$get_spectra(analyses = 1)
bsa <- bsa[, .(intensity = mean(intensity)), by = c("shift")]
bsa <- bsa$intensity
bsa <- bsa / max(bsa)

cbsa <- e_cuv$get_spectra(analyses = 4)$intensity
cbsa <- cbsa / max(cbsa)

plot(bsa, type = 'l')
lines(cbsa, col = "darkred")

cor(bsa, cbsa)


## Antibody -----

ant_dir <- paste0(wd, "/Bevacizumab_Avastin_LotB8703H40/LC-Raman/for Ricardo/Messung 1")

ant_files <- list.files(ant_dir, pattern = "_\\d.asc", full.names = TRUE)

ant <- RamanEngine$new(ant_files, runParallel = FALSE)

# ant$add_replicate_names(sub("_\\d+$", "", e_sec$get_analysis_names()))
# ant$add_replicate_names(sub("_(\\d)\\d*$", "_\\1", ant$get_analysis_names()))
# ant$merge_spectra_time_series()

ant$bin_spectra(Settings_bin_spectra_StreamFind(windowSpectrumUnits = 19))

ant$normalize_spectra()

ant$subtract_spectra_section(Settings_subtract_spectra_section_StreamFind(sectionWindow = c(0, 3)))

ant$smooth_spectra(Settings_smooth_spectra_StreamFind(windowSize = 5))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 330))))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(2000, max(e_sec$get_spectra()$shift)))))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(0, 6))))

ant$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(10, max(e_sec$get_spectra()$rt)))))

ant$correct_spectra_baseline(
  Settings_correct_spectra_baseline_StreamFind(
    method = "als", args = list(lambda = 3, p = 0.09, maxit = 10)
  )
)

# TODO function for baseline fitting
# TODO smoothing with SavityGoley
# 

# ant$plot_chromatograms()
ant$plot_spectra()



View(e_sec$get_results("spectra"))

View(e_sec$averaged_spectra)

specblk <- e_sec$get_spectra(rt = c(20, 200))
specblk <- specblk[, .(intensity = mean(intensity)), by = "shift"]

specpk <- e_sec$get_spectra(rt = c(435 - 2.5, 435 + 2.5))
specpk <- specpk[, .(intensity = mean(intensity)), by = "shift"]

plot(specblk$intensity, type = 'l')
lines(specpk$intensity, col = "red")

e_sec$plot_spectra(rt = c(435 - 2.5, 435 + 2.5))

e_sec$plot_chromatograms()


























## Other Code -----





e_sec$get_spectra()

# blank
sec_blank <- e_sec$get_spectra(rt = c(10:200))

# peak BSA
sec_bsa <- e_sec$get_spectra(rt = c(435.241 - 10, 435.241 + 10))

# peak Hem
sec_hem <- e_sec$get_spectra(rt = c(366.216 - 10, 366.216 + 10))



averageRamanSpectra <- function(spectra) {
  if (length(unique(spectra$analysis)) > 1) {
    split_vec <- spectra$analysis
    spectra[["analysis"]] <- NULL
    spectra <- split(spectra, split_vec)
  } else {
    analysis <- unique(spectra$analysis)
    spectra[["analysis"]] <- NULL
    spectra <- list(spectra)
    names(spectra) <- analysis
  }
  ints <- lapply(spectra, function(x) x$intensity)
  ints <- Reduce(`+`, ints) / length(ints) # possibly other options, e.g., max or sum
  
  averageSpectra <- data.table(
    "shift" = spectra[[1]][["shift"]],
    "intensity" = ints
  )
  
  averageSpectra
}


Settings_subtract_spectra_section_StreamFindd()


plot_chromatogram() # TODO when rt column is present, sums up the intensities for each rt

sp <- e_sec$get_spectra()
sp <- sp[, intensity := sum(intensity), by = c("analysis", "rt")]
sp[["shift"]] <- NULL
sp <- unique(sp)
# sp <- split(sp, sp$analysis)
# plot(sp[[1]]$intensity)
# plot(sp[[2]]$intensity)
# plot(sp[[3]]$intensity)

sp$rt[sp$intensity == min(sp$intensity)]

?RamanEngine


e_sec$get_files()


e_cuv$add_replicate_names(sub("_\\d+$", "", e_cuv$get_analysis_names()))
e_cuv$plot_spectra(colorBy = "replicates", interactive = FALSE)





rcpp_parse_asc_file(cuvette_files[6])


# all_spec <- e_cuv$spectra
# 
# unique(all_spec$replicate)
# 
# blk_spec <- all_spec[all_spec$replicate %in% "220726_H2O_20mW_0,1s_900n", ]
# 
# ser_spec <- all_spec[all_spec$replicate %in% "220726_Blutserum_20mW_0,1s_900n", ]
# 
# subst <- ser_spec$intensity - blk_spec$intensity
# 
# subst <- ser_spec$intensity - blk_spec$intensity
# 
# plot(ser_spec$intensity, type = 'l')
# lines(blk_spec$intensity, col = "darkred")
# plot(subst, col = "darkblue", type = 'l')

# e_cuv$plot_spectra(colorBy = "replicates", interactive = TRUE)

# View(e_cuv$get_spectra())





















r1 <- RamanEngine$new(files)

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






