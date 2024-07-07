library(StreamFind)
library(StreamFindData)

# resources --------------------------------------------------------------------
## files -------------------------------------------------------x----------------
all_ms_files <- StreamFindData::get_ms_file_paths()
mrm_files <- all_ms_files[grepl("mrm", all_ms_files)]
ms_files <- all_ms_files[grepl("influent|blank", all_ms_files)]
raman_files <- StreamFindData::get_raman_file_paths()

## databases -------------------------------------------------------------------
db <- StreamFindData::get_ms_tof_spiked_chemicals()

# MassSpecEngine ---------------------------------------------------------------
ms <- MassSpecEngine$new(files = ms_files)
ms$save(paste0(getwd(), "/ms.sqlite"))
ms$run_app()

# RamanEngine ------------------------------------------------------------------
raman <- RamanEngine$new(files = raman_files)
raman$save(paste0(getwd(), "/raman.sqlite"))
raman$run_app()
