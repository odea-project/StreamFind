#' **MassSpecSettings_FillFeatures_StreamFind**
#'
#' @description Settings for filling missing values in features.
#'
#' @param withinReplicate Logical of length one to fill within replicates not global.
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minIntenisty Numeric of length one with the minimum intensity to collect spectra data for
#' extracted ion chromatograms.
#' @param baseCut Numeric of length one with the base cut for building Gaussian model.
#' @param minNumberTraces Integer of length one with the minimum number of traces to consider a
#' feature.
#' @param minSignalToNoiseRatio Numeric of length one with the minimum signal to noise ratio to
#' consider a feature.
#' @param minGaussianFit Numeric of length one with the minimum Gaussian fit to consider a feature.
#'
#' @return A MassSpecSettings_FillFeatures_StreamFind class object.
#'
#' @export
#'
MassSpecSettings_FillFeatures_StreamFind <- S7::new_class(
  name = "MassSpecSettings_FillFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(withinReplicate = TRUE,
                         rtExpand = 0,
                         mzExpand = 0,
                         minTracesIntensity = 1000,
                         minNumberTraces = 5,
                         baseCut = 0.3,
                         minSignalToNoiseRatio = 3,
                         minGaussianFit = 0.2) {
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "FillFeatures",
        required = c("FindFeatures", "GroupFeatures"),
        algorithm = "StreamFind",
        parameters = list(
          withinReplicate = as.logical(withinReplicate),
          rtExpand = as.numeric(rtExpand),
          mzExpand = as.numeric(mzExpand),
          minTracesIntensity = as.numeric(minTracesIntensity),
          minNumberTraces = as.numeric(minNumberTraces),
          baseCut = as.numeric(baseCut),
          minSignalToNoiseRatio = as.numeric(minSignalToNoiseRatio),
          minGaussianFit = as.numeric(minGaussianFit)
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "FillFeatures")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_logical(self@parameters$withinReplicate, len = 1)
    checkmate::assert_numeric(self@parameters$rtExpand, len = 1)
    checkmate::assert_numeric(self@parameters$mzExpand, len = 1)
    checkmate::assert_integer(as.integer(self@parameters$minNumberTraces), len = 1)
    checkmate::assert_numeric(self@parameters$minTracesIntensity, len = 1)
    checkmate::assert_numeric(self@parameters$baseCut, len = 1)
    checkmate::assert_numeric(self@parameters$minSignalToNoiseRatio, len = 1)
    checkmate::assert_numeric(self@parameters$minGaussianFit, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FillFeatures_StreamFind) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  nts <- engine$nts

  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  analyses_list <- engine$analyses$analyses
  feature_list <- engine$nts$feature_list
  feature_list <- data.table::rbindlist(feature_list, idcol = "analysis", fill = TRUE)
  rpls <- engine$analyses$replicates
  feature_list$replicate <- rpls[feature_list$analysis]
  feature_list_nf <- data.table::copy(feature_list)
  feature_list_nf$group[is.na(feature_list_nf$group)] <- ""
  
  res <- rcpp_ms_fill_features(
    analyses_list,
    feature_list_nf,
    parameters$withinReplicate,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minTracesIntensity,
    as.integer(parameters$minNumberTraces),
    parameters$baseCut,
    parameters$minSignalToNoiseRatio,
    parameters$minGaussianFit
  )
  
  res <- lapply(res, function(z) {
    temp <- data.table::rbindlist(z, fill = TRUE)
    if (nrow(temp) > 0) temp <- temp[!duplicated(temp$group), ]
    temp
  })

  res <- data.table::rbindlist(res, fill = TRUE)

  all_fts <- data.table::rbindlist(list(feature_list, res), fill = TRUE)
  all_fts$filled[is.na(all_fts$filled)] <- FALSE
  all_fts_analysis <- all_fts$analysis
  all_fts$analysis <- NULL
  all_fts$replicate <- NULL
  all_fts <- split(all_fts, all_fts_analysis)
  
  all_fts <- lapply(all_fts, function(z) {
    duplos <- unique(z$feature[duplicated(z$feature)])
    if (length(duplos) > 0) {
      for (duplo in duplos) {
        temp <- z[z$feature %in% duplo, ]
        grp <- temp$group[!is.na(temp$group)]
        if (length(grp) > 0) {
          z$group[z$feature %in% duplo] <- grp[1]
          z <- z[!(z$feature %in% duplo & z$adduct %in% "" & z$mass %in% 0), ]
        }
      }
    }
    z
  })
  
  nts$feature_list <- all_fts
  engine$nts <- nts
  TRUE
}
