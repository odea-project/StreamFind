
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FillFeatures_StreamFind**
#'
#' @description Settings for filling missing values in features.
#' 
#' @param withinReplicate Logical of length one to fill within replicates not global.
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param minIntenisty Numeric of length one with the minimum intensity to collect spectra data for extracted ion chromatograms.
#' @param baseCut Numeric of length one with the base cut for building Gaussian model.
#' @param minNumberTraces Integer of length one with the minimum number of traces to consider a feature.
#' @param minSignalToNoiseRatio Numeric of length one with the minimum signal to noise ratio to consider a feature.
#' @param minGaussianFit Numeric of length one with the minimum Gaussian fit to consider a feature.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_FillFeatures_StreamFind.
#'
#' @export
#'
MassSpecSettings_FillFeatures_StreamFind <- S7::new_class("MassSpecSettings_FillFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(withinReplicate = TRUE,
                         rtExpand = 0,
                         mzExpand = 0,
                         minTracesIntensity = 1000,
                         minNumberTraces = 5,
                         baseCut  = 0.3,
                         minSignalToNoiseRatio = 3,
                         minGaussianFit = 0.2) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FillFeatures",
      algorithm = "StreamFind",
      parameters = list(
        withinReplicate = withinReplicate,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        minTracesIntensity = minTracesIntensity,
        minNumberTraces = minNumberTraces,
        baseCut = baseCut,
        minSignalToNoiseRatio = minSignalToNoiseRatio,
        minGaussianFit = minGaussianFit
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "FillFeatures"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_logical(self@parameters$withinReplicate, len = 1),
      checkmate::test_numeric(self@parameters$rtExpand, len = 1),
      checkmate::test_numeric(self@parameters$mzExpand, len = 1),
      checkmate::test_integer(as.integer(self@parameters$minNumberTraces), len = 1),
      checkmate::test_numeric(self@parameters$minTracesIntensity, len = 1),
      checkmate::test_numeric(self@parameters$baseCut, len = 1),
      checkmate::test_numeric(self@parameters$minSignalToNoiseRatio, len = 1),
      checkmate::test_numeric(self@parameters$minGaussianFit, len = 1)
    )
    if (!valid) return(FALSE)
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
  
  cache <- .load_chache("fill_features", nts$features, x)
  
  if (!is.null(cache$data)) {
    message("\U2139 Filled features loaded from cache!")
    nts$features <- cache$data
    engine$nts <- nts
    return(TRUE)
  }
  
  parameters <- x$parameters
  
  analyses_list <- engine$analyses$analyses
  fts <- engine$get_features()
  
  res <- rcpp_ms_fill_features(
    analyses_list,
    fts,
    parameters$withinReplicate,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minTracesIntensity,
    as.integer(parameters$minNumberTraces),
    parameters$baseCut,
    parameters$minSignalToNoiseRatio,
    parameters$minGaussianFit
  )
  
  res <- lapply(res, function(x) data.table::rbindlist(x, fill = TRUE))
  res <- data.table::rbindlist(res, fill = TRUE)
  
  fg <- nts$features
  fg_groups <- fg@groups
  fg_index <- fg@ftindex
  fg_analyses <- names(fg@features@features)
  
  all_fts <- data.table::rbindlist(list(fts, res), fill = TRUE)
  data.table::setnames(all_fts, "feature", "ID", skip_absent = TRUE)
  data.table::setnames(all_fts, "rt", "ret", skip_absent = TRUE)
  data.table::setnames(all_fts, "rtmin", "retmin", skip_absent = TRUE)
  data.table::setnames(all_fts, "rtmax", "retmax", skip_absent = TRUE)
  all_fts$filled[is.na(all_fts$filled)] <- FALSE
  all_fts_analysis <- all_fts$analysis
  all_fts$analysis <- NULL
  all_fts$replicate <- NULL
  all_fts <- split(all_fts, all_fts_analysis)
  
  # neutralize mz values when featureGroups is a set
  if ("featureGroupsSet" %in% is(fg)) {
    if (identical(patRoon::analyses(fg), names(all_fts))) {
      for (x in names(all_fts)) {
        sel <- fg@analysisInfo$analysis %in% x
        pol <- fg@analysisInfo$set[sel]
        if ("positive" %in% pol) adduct_val <- -1.007276
        if ("negative" %in% pol) adduct_val <- 1.007276
        sel_to_change <- round(all_fts[[x]]$mz, 0) != round(all_fts[[x]]$mass, 0)
        all_fts[[x]]$mz[sel_to_change] <- all_fts[[x]]$mz + adduct_val
        all_fts[[x]]$mzmin[sel_to_change] <- all_fts[[x]]$mzmin + adduct_val
        all_fts[[x]]$mzmax[sel_to_change] <- all_fts[[x]]$mzmax + adduct_val
      }
    }
  }
  
  for (i in seq_len(nrow(res))) {
    ana_idx <- which(fg_analyses == res$analysis[i])
    gr <- res$group[i]
    fg_groups[ana_idx, gr] <- res$intensity[i]
    fg_index[ana_idx, gr] <- which(all_fts[[fg_analyses[ana_idx]]]$group %in% gr)
  }
  
  fg@groups <- fg_groups
  fg@ftindex <- fg_index
  fg@features@features <- all_fts[names(fg@features@features)]
  
  if (!is.null(cache$hash)) {
    .save_cache("fill_features", fg, cache$hash)
    message("\U1f5ab Filled features cached!")
  }
  
  nts$features <- fg
  engine$nts <- nts
  TRUE
}
