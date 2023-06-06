
# library(streamFind)

# save_default_ProcessingSettings(
#   call = "find_features",
#   software = "xcms",
#   algorithm = "centwave"
# )

# save_default_ProcessingSettings(
#   call = "find_features",
#   algorithm = "openms"
# )

db <- "E:\\Dev_20230530_Orbitrap_AFINTS\\Composition_Mix-Fusion.csv"
db <- data.table::fread(db)
# mol <- rcdk::parse.smiles(db$SMILES)
# db$mass <- vapply(mol, rcdk::get.exact.mass, NA_real_)
# write.csv(db, "E:\\Dev_20230530_Orbitrap_AFINTS\\Composition_Mix-Fusion.csv")

fl <- "E:\\Dev_20230530_Orbitrap_AFINTS\\220428_VqF-Coup_Etablierung_MixFusion_pos_02.mzML"
ms <- MassSpecData$new(fl)
ms$import_settings("./settings.json")
ms$find_features()

ms$plot_bpc()

fts <- ms$get_features()
fts <- fts[order(fts$mz), ]


suspectScreeningPrototype <- function(ms, db, ppm = 4 , sec = 10) {

  overview <- ms$get_overview()

  ana_sus <- lapply(ms$get_analysis_names(), function(a, overview, ms, db, ppm, sec) {

    if (!("mz" %in% colnames(db)) && "mass" %in% colnames(db)) {
      pol <- overview$features[overview$polarity %in% a]

      if ("positive" %in% pol) {
        db$mz <- db$mass + 1.007276
      }

      if ("negative" %in% pol) {
        db$mz <- db$mass - 1.007276
      }
    }

    db$mz <- as.numeric(db$mz)

    if (!"rt" %in% colnames(db)) {
      db$rt <- NA_real_
    } else {
      db$rt[db$rt == ""] <- NA_real_
    }

    db$rt <- as.numeric(db$rt)

    db <- db[!duplicated(db[, c("mz", "SMILES")]), ]

    feats <- overview$features[overview$analysis %in% a]

    if (feats > 0) {

      sus <- lapply(db$SMILES, function(x, a, ms, db, ppm, sec) {

        which_vals = db$SMILES %in% x
        x_mz = db$mz[which_vals]
        x_rt = db$rt[which_vals]
        if (is.na(x_rt)) {
          x_rt = NULL
        }
        x_name = db$name[which_vals]
        x_formula = db$formula[which_vals]

        temp <- ms$get_features(analyses = a, mz = x_mz, rt = x_rt, ppm = ppm, sec = sec)

        if (nrow(temp) > 0) {

          temp$id_level <- NA_character_
          temp$mz_error <- round((abs(temp$mz - x_mz) / temp$mz) * 1E6, digits = 1)
          temp$rt_error <- NA_real_
          temp$suspect <- x_name
          temp$formula <- x_formula
          temp$SMILES <- x

          for (i in seq_len(nrow(temp))) {

            temp$id_level[i] = "4"

            if (!is.null(x_rt)) {
              temp$id_level[i] = "3b"
              temp$rt_error[i] = round(temp$rt[i] - x_rt, digits = 0)
            }

            # add check for MS2 data of feature when MS2 are loaded, if not loaded ask to load

          }
        }

        temp

      },
      a = a,
      ms = ms,
      db = db,
      ppm = ppm,
      sec = sec)

      sus <- rbindlist(sus, fill = TRUE)

    } else {
      data.table()
    }
  },
  overview = overview,
  ms = ms,
  db = db,
  ppm = ppm,
  sec = sec)

  ana_sus <- rbindlist(ana_sus, fill = TRUE)

  setcolorder(ana_sus, c("suspect", "id_level", "mz_error", "rt_error"))

  ana_sus
}


ana_sus <- suspectScreeningPrototype(ms, db, ppm = 4, sec = 10)

unique_suspects <- unique(ana_sus$suspect)
unique_suspects_features <- lapply(unique_suspects, function(x) {
  ana_sus$feature[ana_sus$suspect %in% x]
})
names(unique_suspects_features) <- unique_suspects


i = which(unique_suspects %in% "Sotalol")
# i = 27
unique_suspects[i]
ms$get_features(features = unique_suspects_features[[i]])
ms$plot_features(features = unique_suspects_features[[i]])


ms$plot_features(features = "mz388.105_rt1244_f15264")

ms$plot_features(features = "mz192.138_rt1071_f9041")


ms$map_features(features = output$output$feature[output$output$iso_gr == 1])

ms$get_features(mz = db$mz[343], ppm = 2)

ms$get_features(
  mz = data.frame(
    mzmin = 254, mzmax = 256,
    rtmin = 705, rtmax = 730
  )
)

ms$plot_features_ms1(
  features = "mz399.25_rt1276_f15896",
  rtWindow = c(-2, 2),
  mzWindow = c(-1, 8),
  mzClust = 0.0005
)

ms$plot_features_ms2(
  features = "mz192.138_rt1071_f9041",
  mzClust = 0.0005
)

output <- rcpp_ms_annotation_isotopes(fts)
View(output$output)




