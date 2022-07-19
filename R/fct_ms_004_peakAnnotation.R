

#' @title annotationSettingsDefaultCAMERA
#'
#' @return A \linkS4class{settings} object containing parameters for
#' recursive integration of peaks from analyses not represented in a given feature.
#'
#' @export
#'
#' @importFrom data.table fread
#'
annotationSettingsDefaultCAMERA <- function() {

  return(
    new(
      "settings",
      call = "peakAnnotation",
      algorithm = "camera",
      settings = list(
        ionization = NA_character_,
        onlyIsotopes = FALSE,
        minSize = 1,
        relMinReplicates = 1,
        extraOpts = list(
          sigma = 6,
          perfwhm = 0.35,
          cor_eic_th = 0.3,
          graphMethod = "hcs",
          pval = 0.05,
          calcCiS = TRUE,
          calcIso = TRUE,
          calcCaS = TRUE,
          maxcharge = 3,
          maxiso = 5,
          ppm = 15,
          mzabs = 0.008,
          rules =  data.table::fread(system.file("rules/primary_adducts_pos.csv", package = "CAMERA"), header = TRUE),
          multiplier = 3,
          max_peaks = 500,
          intval = "maxo"
        )
      )
    )
  )
}

#' @title annotationSettingsDefaultRAMClustR
#'
#' @return A \linkS4class{settings} object containing parameters for
#' recursive integration of peaks from analyses not represented in a given feature.
#'
#' @export
#'
annotationSettingsDefaultRAMClustR <- function() {

  return(
    new(
      "settings",
      call = "peakAnnotation",
      algorithm = "ramclustr",
      settings = list(
        ionization = NA_character_,
        st = NULL,
        sr = NULL,
        maxt = 12,
        hmax = 0.3,
        normalize = "TIC",
        absMzDev = 0.005,
        relMzDev = 5,
        minSize = 2,
        relMinReplicates = 1,
        # RCExperimentVals = list(
        #   design = list(platform = "LC-MS"),
        #   instrument = list(ionization = ionization, MSlevs = 1)
        # ),
        extraOptsRC = NULL,
        extraOptsFM = NULL
      )
    )
  )
}

#' @title peakAnnotation
#'
#' @description Annotation of isotopic and adduct peaks in features.
#'
#' @param object An \linkS4class{msData} object with peaks/features.
#' @template args-single-settings
#' @param excludeBlanks Logical, set to \code{TRUE} for excluding
#' blank replicates from annotation. Blank intensities are not considered.
#'
#' @return An \linkS4class{ntsData} object with annotated peaks/features.
#'
#' @export
#'
#' @importFrom checkmate assertClass
#' @importMethodsFrom patRoon generateComponents
#' @importClassesFrom patRoon components
#'
peakAnnotation <- function(object = NULL, settings = NULL) {

  checkmate::assertClass(object, "msData")

  noPeaks <- sapply(object@analyses, function(x) nrow(x@peaks))
  noPeaks <- TRUE %in% (0 %in% noPeaks)

  noFeatures <- nrow(object@features@intensity) == 0

  if (noPeaks | noFeatures) {
    warning("Object does not have peaks/features!")
    return(object)
  }

  pat <- as.featureGroups(object)

  if (is.null(settings)) {
    prs <- getParameters(object, where = "features", call = "peakAnnotation")
    if (length(prs) > 0) {
      algorithm = getAlgorithm(prs)
      settings = getSettings(prs)
    } else {
      algorithm <- NA_character_
    }

  } else if (checkmate::testClass(settings, "settings")) {
    prs <- settings
    algorithm = getAlgorithm(prs)
    settings = getSettings(prs)

  } else {
    algorithm <- NA_character_
  }

  if (is.na(algorithm)) {
    warning("Peak annotation algorithm not defined!")
    return(object)
  }

  ag <- list(fGroups = pat, algorithm = algorithm)

  #check up for polarity
  pola <- polarities(object)
  if (length(unique(pola)) == 1) {
    if (unique(pola) %in% "positive" & algorithm %in% c("camera", "ramclustr")) {
      settings$ionization <- unique(pola)
    }
    # TODO implement check for negative and multiple polarities/connect to patRoon sets
  }

  comp <- do.call(patRoon::generateComponents, c(ag, settings))

  # TODO convert to the simplest structure for components in patRoon
  # TODO sub-setting takes too long. Is it reall nedded or info in features metadata is enough?
  object@features@annotation <- list(comp)

  if (length(unique(pola)) == 1 & "positive" %in% unique(pola)) {
    prefAdduct <- "[M+H]+"
  } else {
    prefAdduct <- "[M-H]-"
  }

  object <- annotateFeatures(object, pat, comp, prefAdduct)

  object <- addParameters(object, where = "features", settings = prs)

  validObject(object)

  return(object)
}


#' @title annotateFeatures
#'
#' @description Selects the ions from the components and amends the festure data table.
#'
#' @param object An \linkS4class{msData} object with peaks/features.
#' @param prefAdduct A character string with the preferred adduct.
#' Possible values are \code{"[M+H]+"} (the default) and \code{"[M-H]-"} for
#' positive and negative ionization, respectively.
#'
#' @importMethodsFrom patRoon componentTable
#' @importFrom data.table rbindlist setnames fread
#'
annotateFeatures <- function(object, pat, comp, prefAdduct = "[M+H]+") {

  feats <- object@features@metadata

  cat("Annotating features based on components... \n")

  pb <- txtProgressBar(
    min = 0,
    max = nrow(feats),
    style = 3,
    width = 50,
    char = "+"
  )

  #change to streamFind package folder
  if ("[M+H]+" %in% prefAdduct) {
    db_adducts <- fread(system.file("rules/primary_adducts_pos.csv", package = "CAMERA"), header = TRUE)
  } else {
    db_adducts <- fread(system.file("rules/primary_adducts_neg.csv", package = "CAMERA"), header = TRUE)
  }

  comp_df <- patRoon::componentTable(comp)
  comp_df <- rbindlist(comp_df, idcol = "component")

  #In cliqueMS colnames are: neutralMass (changes to M_adduct) isonr charge adduct_ion  intensity intensity_rel
  #In ramclustr colnames are: intensity intensity_rel group isogroup isonr charge adduct_ion ppm
  setnames(comp_df, c("group", "neutralMass"), c("id", "M_adduct"), skip_absent = TRUE)

  if (!"M_adduct" %in% colnames(comp_df)) {
    comp_df[, M_adduct := as.numeric(NA)]
  }

  if (!"isogroup" %in% colnames(comp_df)) {
    comp_df[, isogroup := as.numeric(stringr::str_extract(comp_df$component, "[:digit:]"))]
  }
  comp_df$isonr <- as.numeric(comp_df$isonr)

  aft <- feats[, .(id, mz, rt)]
  aft <- aft[, `:=`(
    "neutralMass" = round(mz - db_adducts[name %in% prefAdduct, massdiff], digits = 4),
    "isonr" = 0,
    "monoiso" = id,
    "isogroup" = NA_character_,
    "charge" = 1,
    "adduct_ion" = prefAdduct,
    "intensity" = NA,
    "rel_intensity" = NA,
    "component" = NA_character_,
    "annotated" = NA_integer_
  )]

  aft$rel_intensity <- as.numeric(aft$rel_intensity)
  aft$intensity <- as.numeric(aft$intensity)

  #priority for [M+H]+ or [M-H]-
  #smallest rt difference from correspondent prefAdduct
  #smallest mz differnece from correspondence prefAdduct
  for (f in seq_len(nrow(aft))) {

    x_id <- aft$id[f]

    temp <- comp_df[id %in% x_id, ]

    aft[id %in% x_id, annotated := nrow(temp)]

    #filter preferential adduct, isotopes are not likely to be duplicated
    if (nrow(temp) > 1) {
      if (prefAdduct %in% temp$adduct_ion) {
        temp <- temp[adduct_ion %in% prefAdduct, ]
      } else {
        temp[, `:=`(rt_d = 0, mz_d = 0)]
        for (i in seq_len(nrow(temp))) {
          temp2 <- comp_df[component %in% temp$component[i] & M_adduct == temp$M_adduct[i], ]

          if (prefAdduct %in% temp2$adduct_ion) {
            temp$rt_d[i] <-  abs(temp$ret[i] - temp2[adduct_ion %in% prefAdduct, ret])
            temp$mz_d[i] <-  abs(
              temp$mz[i] -
                temp2[adduct_ion %in% prefAdduct, mz] -
                db_adducts[name %in% temp$adduct_ion[i], massdiff] +
                db_adducts[1, massdiff]
            )
          } else if (length(unique(temp2$adduct_ion)) == 1) {

            temp[1, colnames(temp)[stringr::str_detect(colnames(temp), "ad")] := NA]
            temp <- temp[1, ]
            break

          } else {
            temp$rt_d[i] <-  abs(temp$ret[i] - temp2[M_adduct %in% temp$M_adduct[i] & !adduct_ion %in% temp$adduct_ion[i], ret])
            temp$mz_d[i] <-  abs(
              temp$mz[i] -
                temp2[M_adduct %in% temp$M_adduct[i] & !adduct_ion %in% temp$adduct_ion[i], mz] -
                db_adducts[name %in% temp$adduct_ion[i], massdiff] +
                db_adducts[name %in% temp2[M_adduct %in% temp$M_adduct[i] & !adduct_ion %in% temp$adduct_ion[i], adduct_ion], massdiff]
            )
          }
        }
        temp <- temp[rt_d == min(temp$rt_d), ] #lowest rt diff
        temp <- temp[mz_d == min(temp$mz_d), ] # lowest mz diff
      }
    }

    if (nrow(temp) == 1) {

      ntm <- temp$M_adduct

      #amend neutralMass for multiple charged isotopes
      if (TRUE %in% (temp$charge > 1) & TRUE %in% (temp$isonr == 0)) {
        ntm <- (aft[id %in% x_id, mz] - db_adducts[name %in% prefAdduct, massdiff]) * temp$charge
      }

      #amend direct ions to the protonated ion
      if (TRUE %in% grepl("[M]", temp$adduct_ion, fixed = TRUE)) {
        #retrieve the mass of the respective adduct
        mono_id <- comp_df[component %in% temp$component & isogroup %in% temp$isogroup & isonr %in% 0, ]

        if (nrow(mono_id) > 1) {
          mono_id[, dist := abs(temp$mz - mz)]
          mono_id <- mono_id[dist == min(dist), ]
        }

        mono_x_id <- mono_id$id

        if (length(mono_x_id) > 0) {
          # TODO adds the neutralMass from components but may not work for other algorithms
          ntm <- comp_df[id %in% mono_x_id, M_adduct]
          aft[id %in% x_id, isonr := temp$isonr]
          aft[id %in% x_id, monoiso := mono_x_id]
          aft[id %in% x_id, rel_intensity := as.numeric(temp$intensity) / comp_df[group %in% mono_x_group, intensity]]
        }
        mono_x_id <- character()
      }

      #amend isotopic peaks to related monoisotope
      if (TRUE %in% (temp$isonr > 0)) {

        #retrieve the mass of the respective adduct
        mono_id <- comp_df[component %in% temp$component & isogroup %in% temp$isogroup & isonr %in% 0, ]

        if (nrow(mono_id) > 1) {
          mono_id[, dist := abs(temp$mz - mz)]
          mono_id <- mono_id[dist == min(dist), ]
        }

        if (nrow(mono_id) > 0) {
          mono_x_id <- unique(mono_id$id)
          ntm <- aft[id %in% mono_x_id, neutralMass]
          aft[id %in% x_id, isonr := temp$isonr]
          aft[id %in% x_id, monoiso := mono_x_id]
          temp$adduct_ion <- paste0("[M+", temp$isonr, "]")
          aft[id %in% x_id, rel_intensity := as.numeric(temp$intensity) / aft[id %in% mono_x_id, intensity]]
        }
        mono_x_id <- character()
      }

      if (!is.na(ntm)) {
        aft[id %in% x_id, neutralMass := ntm]

        #condition to not change the adduct ion if there was no annotation added
        if (!is.na(temp$adduct_ion)) aft[id %in% x_id, adduct_ion := temp$adduct_ion]
      }
      ntm <- NA

      aft <- aft[id %in% x_id, `:=`(
        charge = temp$charge,
        intensity = as.numeric(temp$intensity),
        component = temp$component,
        isogroup = temp$isogroup
      )]
    }

    setTxtProgressBar(pb, f)
  }

  close(pb)
  cat("Done! \n")

  aft[is.na(rel_intensity), rel_intensity := 1]
  aft$rel_intensity <- round(aft$rel_intensity, digits = 4)
  aft[, c("mz", "rt") := NULL]

  colN <- colnames(aft)
  colN <- colN[!colN %in% "id"]
  keepColN <- colnames(feats)[!colnames(feats) %in% colN]

  feats <- dplyr::left_join(feats[, keepColN, with = FALSE], aft, by = "id")

  cat("Adding annotation to msFeatures...")
  #amend ids with annotation
  new_id <- sapply(feats$id, function(x, feats) {
    temp <- feats[id %in% x, ]
    if (temp$annotated > 0) {
      f_idx <- gsub(".*_f", "", x)
      x_b <- gsub("_f.*", "", x)
      #x <- stringr::str_extract(x, ".*(?=_f)")
      if (!is.na(temp$charge)) x_b <- paste0(x_b, "_z", temp$charge)
      x_b <- paste0(x_b, "_a", temp$adduct_ion)
      if (!is.na(temp$isogroup) & temp$isonr == 0) {
        iso_p <- rep(0, max(feats$isonr))
        isotopes <- feats[monoiso %in% x & isonr > 0, ]
        if (nrow(isotopes) > 0) {
          iso_p[isotopes$isonr] <- round((isotopes$intensity / temp$intensity) * 100, digits = 0)
          x_b <- paste0(x_b, "_i", paste(iso_p, collapse = "/"))
        }
      }
      x_b <- paste0(x_b, "_f", f_idx)
      x <- x_b
    }
    return(x)
  }, feats = feats)

  names(new_id) <- feats$id


  #amend msFeatures feature id
  feats[, monoiso := new_id[feats$monoiso]]
  feats$id <- new_id

  object@features@metadata <- feats
  object@features@intensity$id <- new_id

  #amend msAnalysis feature id
  object@analyses <- lapply(object@analyses, function(x, new_id) {
    x@peaks[feature %in% names(new_id), feature :=  new_id[names(new_id) %in% x@peaks$feature]]
    return(x)
  }, new_id = new_id)

  cat("Done! \n")

  return(object)
}
