
# files <- StreamFindData::get_raman_file_paths()

# wd <- "E:/iSoft"
wd <- "C:/Users/apoli/Documents/iSoft"


## Cuvette -----


cuvette_dir <- paste0(wd, "/Jana_cuvette_serum")

cuvette_files <- list.files(cuvette_dir, pattern = "shiftadded", full.names = TRUE)

e_cuv <- RamanEngine$new(cuvette_files, runParallel = FALSE)

e_cuv$add_replicate_names(sub("_\\d+$", "", e_cuv$get_analysis_names()))

e_cuv$add_blank_names(
  c(
    rep("shiftadded_220726_H2O_100mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_20mW_0,1s_900n", 3),
    rep("shiftadded_220726_H2O_50mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_100mW_0,1s_300n", 3),
    rep("shiftadded_220726_H2O_20mW_0,1s_900n", 3),
    rep("shiftadded_220726_H2O_50mW_0,1s_300n", 3)
  )
)

e_cuv$average_spectra()

e_cuv$subtract_blank_spectra()

e_cuv$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 470))))

e_cuv$smooth_spectra(Settings_smooth_spectra_StreamFind(windowSize = 2))

e_cuv$normalize_spectra()

e_cuv$correct_spectra_baseline(
  Settings_correct_spectra_baseline_StreamFind(method = "als", args = list(lambda = 3, p = 0.06, maxit = 10))
)

e_cuv$plot_spectra()

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

sec_dir <- paste0(wd, "/Jana_SEC_serum")
sec_files <- list.files(sec_dir, pattern = "_\\d.asc", full.names = TRUE)

e_sec <- RamanEngine$new(sec_files, runParallel = FALSE)

e_sec$add_replicate_names(sub("_\\d+$", "", e_sec$get_analysis_names()))
#e_sec$add_replicate_names(sub("_(\\d)\\d*$", "_\\1", e_sec$get_analysis_names()))
# e_sec$merge_replicates(preCut = 2)

e_sec$bin_spectra(Settings_bin_spectra_StreamFind(windowSpectrumUnits = 5))

e_sec$normalize_spectra()

e_sec$subtract_spectra_section(Settings_subtract_spectra_section_StreamFind(sectionWindow = c(10, 200)))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("shift" = c(-40, 470))))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(0, 435.241 - 5))))

e_sec$delete_spectra_section(Settings_delete_spectra_section_StreamFind(list("rt" = c(435.241 + 5, max(e_sec$get_spectra()$rt)))))

e_sec$smooth_spectra(Settings_smooth_spectra_StreamFind(windowSize = 3))

e_sec$correct_spectra_baseline(
  Settings_correct_spectra_baseline_StreamFind(method = "als", args = list(lambda = 5, p = 0.05, maxit = 10))
)

e_sec$plot_spectra()

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






