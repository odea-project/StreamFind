
## Core development

# R6 class: CoreEngine

core <- CoreEngine$new()
core

# S3 classes: ProjectHeaders, ProcessingSettings, Analysis

phead <- ProjectHeaders()
phead

settings <- ProcessingSettings()
settings

ana <- Analysis()
ana

xy <- XyEngine$new()
























patRoon::clearCache("all")
msana <- parse.MassSpecAnalysis(StreamFindData::get_ms_file_paths()[1])[[1]]
class(msana)
sloop::s3_dispatch(print(msana))
ana


core <- CoreEngine$new()
core$add_settings(Settings_annotate_features_StreamFind())
core$add_analyses(Analysis())
core$get_analyses()










core$get_history()


uv <- UVEngine$new()
uv

r1 <- RamanEngine$new(files = StreamFindData::get_raman_file_paths())

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