#' @title .build_features_table_from_patRoon
#'
#' @param pat An object with class `features` or `featureGroups` from the
#' package \pkg{patRoon}.
#'
#' @param self A `MassSpecData` object. When applied within the R6, the self
#' object.
#'
#' @return A named list with a features \linkS4class{data.table} for each 
#' analysis. The names should match the names of analyses in the `MassSpecData`.
#'
#' @noRd
#'
.build_features_table_from_patRoon <- function(pat, self) {
  
  if ("features" %in% is(pat)) {
    anaInfo <- pat@analysisInfo
    
    isSet <- TRUE %in% grepl("Set", is(pat))
    
    features <- pat@features
    
    if ("featuresXCMS3" %in% is(pat)) {
      if (xcms::hasFilledChromPeaks(pat@xdata)) {
        extra <- xcms::chromPeaks(pat@xdata, isFilledColumn = TRUE)
        extra$is_filled <- as.logical(extra$is_filled)
        extra$analysis <- anaInfo$analysis[extra$sample]
        extra <- split(extra, extra$analysis)
      } else {
        extra <- NULL
      }
      
    } else {
      extra <- NULL
    }
  }
  
  analyses <- names(features)
  
  features <- lapply(analyses, function(x, extra, features, self, isSet) {
    
    temp <- features[[x]]
    
    valid = TRUE
    
    if (!is.data.frame(temp)) valid <- FALSE
    
    if (valid & nrow(temp) == 0) valid <- FALSE
    
    if (!valid) return(data.table())
    
    if (!is.null(extra)) {
      if (temp == nrow(extra[[x]]) & all(temp$mz == extra[[x]]$mz)) {
        temp$filled <- extra[[x]]$is_filled
      }
    }
    
    below_rt_max <- temp$ret <= temp$retmax * 1.05
    if (!all(below_rt_max)) {
      warning(sum(!below_rt_max), " feature/s with retention time value above the rtmax removed!")
      temp <- temp[below_rt_max, ]
    }
    
    above_rt_min <- temp$ret >= temp$retmin * 0.95
    if (!all(above_rt_min)) {
      warning(sum(!above_rt_min), " feature/s with retention time value below the rtmin removed!")
      temp <- temp[above_rt_min, ]
    }
    
    below_mz_max <- temp$mz <= temp$mzmax + (2 * temp$mzmax / 1E6)
    if (!all(below_mz_max)) {
      warning(sum(!below_mz_max), " feature/s with m/z value above the mzmax removed!")
      temp <- temp[below_mz_max, ]
    }
    
    above_mz_min <- temp$mz >= temp$mzmin - (2 * temp$mzmax / 1E6)
    if (!all(above_mz_min)) {
      warning(sum(!above_mz_min), " feature/s with m/z value below the mzmin removed!")
      temp <- temp[above_mz_min, ]
    }
    
    run <- self$get_run(x)
    
    polarity <- unique(run$polarity)
    
    if (length(polarity) > 1) {
      
      scans_pos <- length(run$polarity[run$polarity == 1])
      
      scans_neg <- length(run$polarity[run$polarity == -1])
      
      ratio <- scans_pos/scans_neg
      
      if (ratio < 1.2 & ratio > 0.8) {
        warning("Multiple polarities detected! Currently, find_features algorithms cannot handled multiple polarities properly.", )
        
      } else if (ratio > 1.2) {
        per_pos_pol <- round((scans_pos / nrow(run)) * 100, digits = 0)
        warning("Multiple polarities detected but positive polarity is present in ", per_pos_pol, "% of the spectra!" )
        
        polarity <- 1
        adduct_val <- -1.007276
        
      } else {
        per_neg_pol <- round((scans_neg / nrow(run)) * 100, digits = 0)
        warning("Multiple polarities detected but negative polarity is present in ", per_neg_pol, "% of the spectra!" )
        
        polarity <- -1
        adduct_val <- 1.007276
      }
      
    } else {
      if (polarity == 1) adduct_val <- -1.007276
      if (polarity == -1) adduct_val <- 1.007276
    }
    
    # required as when is set the mz value is neutralized from patRoon
    if (isSet) {
      temp[temp$adduct %in% "[M-H]-", `:=`(
        mzmin = (temp$mz - 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz - 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz - 1.007276
      )]
      
      temp[temp$adduct %in% "[M+H]+", `:=`(
        mzmin = (temp$mz + 1.007276) - (temp$mz - temp$mzmin),
        mzmax = (temp$mz + 1.007276) + (temp$mzmax - temp$mz),
        mz = temp$mz + 1.007276
      )]
    }
    
    if (!"polarity" %in% colnames(temp)) temp$polarity <- polarity
    
    if (!"mass" %in% colnames(temp)) temp$mass <- temp$mz + adduct_val
    
    if (!"filled" %in% colnames(temp)) {
      temp$filled <- FALSE
    } else {
      temp$filled <- as.logical(temp$filled)
    }
    
    if (!"filtered" %in% colnames(temp)) temp$filtered <- FALSE
    
    if (!"filter" %in% colnames(temp)) temp$filter <- NA_character_
    
    setnames(temp,
             c("ID", "ret", "retmin", "retmax"),
             c("feature", "rt", "rtmin", "rtmax"),
             skip_absent = TRUE
    )
    
    temp <- temp[order(temp$mz), ]
    temp <- temp[order(temp$rt), ]
    temp$index <- seq_len(nrow(temp))
    
    d_dig <- max(temp$mzmax - temp$mzmin)
    
    if (d_dig < 0.1) {
      d_dig <- sub('.*\\.(0+)[1-9].*', '\\1', as.character(d_dig))
      d_dig <- nchar(d_dig) + 1
      
    } else if (d_dig >= 1) {
      d_dig <- 0
      
    } else {
      d_dig <- 1
    }
    
    temp$feature <- paste0(
      "mz",
      round(temp$mz, digits = d_dig),
      "_rt",
      round(temp$rt, digits = 0),
      "_f",
      temp$index
    )
    
    setcolorder(
      temp,
      c(
        "feature",
        "polarity",
        "index",
        "rt",
        "mz",
        "mass", 
        "intensity",
        "area",
        "rtmin",
        "rtmax",
        "mzmin",
        "mzmax", 
        "filled",
        "filtered",
        "filter"
      )
    )
    
    temp$rt <- round(temp$rt, 3)
    temp$rtmin <- round(temp$rtmin, 3)
    temp$rtmax <- round(temp$rtmax, 3)
    
    temp$mz <- round(temp$mz, 8)
    temp$mass <- round(temp$mass, 8)
    temp$mzmin <- round(temp$mzmin, 8)
    temp$mzmax <- round(temp$mzmax, 8)
    
    temp$isocount <- NULL
    
    temp
  }, extra = extra, features = features, self = self, isSet = isSet)
  
  names(features) <- analyses
  
  features
}
