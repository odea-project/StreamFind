
all_files <- streamFindData::msFilePaths()
fl <- all_files[1]
ms <- MassSpecData$new(fl)
db <- streamFindData::msSpikedChemicals()
db <- db[grepl("S", db$tag), ]
db$mz <- db$mass + 1.0073
cols <- c("name", "formula", "mz", "rt", "tag")
db <- db[, cols, with = FALSE]
db

ms$plot_spectra(levels = 1, mz = db, colorBy = "targets")

ms$plot_ms1(mz = db[11, ], ppm = 100, sec = 60, interactive = F, isInAllSpectra = F)

library(plotly)
library(dplyr)
library(data.table)








