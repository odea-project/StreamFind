
#' @title .s3_ms_fill_features.Settings_fill_features_StreamFind
#'
#' @description Settings for filling missing features.
#'
#' @noRd
#'
.s3_ms_fill_features.Settings_fill_features_StreamFind <- function(settings, self, private) {
  
  if (!self$has_groups()) {
    warning("There are no features groups! Run find_features and group_features first!")
    return(FALSE)
  }
  
  replicate <- NULL
  group <- NULL
  name <- NULL
  N <- NULL
  
  cache <- .load_chache("fill_features", ms$featureGroups, settings)
  
  if (!is.null(cache$data)) {
    message("\U2139 Filled features loaded from cache!")
    fg <- cache$data
    self$featureGroups <- fg
    return(TRUE)
  }
  
  fts <- self$get_features()
  
  has_adduct_col <- "adduct" %in% colnames(fts)
  
  rpls <- self$get_replicate_names()
  
  fts$replicate <- rpls[fts$analysis]
  
  fts$name <- paste0(fts$group, "_", fts$replicate)
  
  rpl_presence <- fts[, .N, by = .(name)]
  
  max_anas_in_rpl <- max(rpl_presence$N)
  
  rpl_incomplete <- rpl_presence[N < max_anas_in_rpl]
  
  if (nrow(rpl_incomplete) == 0) {
    warning("There are incomplete replicates! Nothing to do.")
    return(FALSE)
  }
  
  rt_expand <- settings$parameters$rtExpand
  
  mz_expand <- settings$parameters$mzExpand
  
  to_complete <- fts[fts$name %in% rpl_incomplete$name, ]
  
  to_complete <- split(to_complete, to_complete$name)
  
  analyses <- self$get_analysis_names()
  
  to_complete <- lapply(to_complete, function(x) {
    
    rpl <- x$replicate[1]
    
    rpl_anas <- analyses[rpls %in% rpl]
    
    missing_anas <- rpl_anas[!rpl_anas %in% x$analysis]
    
    if (!"polarity" %in% names(x)) {
      polarity <- self$get_spectra_polarity()[x$analysis[1]]
      polarity[polarity == "positive"] <- 1
      polarity[polarity == "negative"] <- -1
    } else {
      polarity <- x$polarity[1]
    }
    
    out <- data.table(
      analysis = missing_anas,
      feature = NA_character_,
      rt = mean(x$rt),
      mz = mean(x$mz),
      area = 0,
      intensity = 0,
      rtmin = min(x$rtmin) - rt_expand,
      rtmax = max(x$rtmax) + rt_expand,
      mzmin = min(x$mzmin) - mz_expand,
      mzmax = max(x$mzmax) + mz_expand,
      filtered = FALSE,
      mass = mean(x$mass),
      polarity = polarity,
      group = x$group[1],
      replicate = rpl,
      name = x$name[1]
    )
    
    # TODO add drift and other info when present
    
    out
  })
  
  to_complete <- rbindlist(to_complete)
  
  spec <- self$get_spectra(mz = to_complete, levels = 1)
  
  spec$id <- paste0(spec$id, "_", spec$analysis)
  
  spec <- split(spec, spec$id)
  
  to_complete$id <- paste0(to_complete$name, "_", to_complete$analysis)
  
  to_complete <- split(to_complete, to_complete$id)
  
  new_fts <- lapply(names(to_complete), function(x) {
    
    ft <- to_complete[[x]]
    
    sp <- spec[[x]]
    
    if (is.null(sp)) return(NULL)
    
    # TODO improve feature filling with peak detection and evaluation
    
    max_int <- max(sp$intensity)
    
    ft$intensity <- max_int
    
    ft$area <- sum(diff(sp$rt) * (sp$intensity[-1] + sp$intensity[-length(sp$intensity)]) / 2)
    
    # if (ft$group[1] %in% "M266_R1007_716" && ft$analysis[1] %in% "03_tof_ww_is_pos_o3sw_effluent-r003") browser()
    
    ft$rt <- sp$rt[which(sp$intensity == max_int)[1]]
    ft$rtmin <- min(sp$rt)
    ft$rtmax <- max(sp$rt)
    ft$mz <- mean(sp$mz)
    ft$mzmin <- min(sp$mz)
    ft$mzmax <- max(sp$mz)
    
    if (ft$polarity == 1) {
      ft$mass <- ft$mz - 1.007276
    } else if (ft$polarity == -1) {
      ft$mass <- ft$mz + 1.007276
    }
    
    if (has_adduct_col) {
      if (ft$polarity == 1) {
        ft$adduct <- "[M+H]+"
      } else if (ft$polarity == -1) {
        ft$adduct <- "[M-H]-"
      }
    }
   
    ft
  })
  
  new_fts <- rbindlist(new_fts)
  new_fts$replicate <- NULL
  new_fts$name <- NULL
  new_fts$id <- NULL
  new_fts$polarity <- NULL
  new_fts_list <- split(new_fts, new_fts$analysis)
  
  fts$replicate <- NULL
  fts$name <- NULL
  fts_filled <- split(fts, fts$analysis)
  
  fts_filled <- lapply(fts_filled, function(x) {
    
    nfts <- new_fts_list[[x$analysis[1]]]
    
    if (is.null(nfts)) return(x)
    
    number_new_fts <- nrow(nfts)
    
    nfts_id <- paste0("filled_", 1:number_new_fts)
    
    if (!"filled" %in% colnames(x)) x$filled <- FALSE
    
    nfts$filled <- TRUE
    
    nfts$feature <- nfts_id
    
    cfts <- rbindlist(list(x, nfts), fill = TRUE)
    
    setnames(cfts, c("feature", "rt", "rtmin", "rtmax"), c("ID", "ret", "retmin", "retmax"))
    
    cfts$analysis <- NULL
    
    cfts
  })
  
  fg <- self$featureGroups
  fg_groups <- fg@groups
  fg_index <- fg@ftindex
  
  for (i in seq_len(nrow(new_fts))) {
    ana_idx <- which(analyses == new_fts$analysis[i])
    gr <- new_fts$group[i]
    fg_groups[ana_idx, gr] <- new_fts$intensity[i]
    fg_index[ana_idx, gr] <- which(fts_filled[[analyses[ana_idx]]]$group %in% gr)
  }
  
  fg@groups <- fg_groups
  fg@ftindex <- fg_index
  fg@features@features <- fts_filled[names(fg@features@features)]
  
  if (!is.null(cache$hash)) {
    .save_cache("fill_features", fg, cache$hash)
    message("\U1f5ab Filled features cached!")
  }
  
  self$featureGroups <- fg
  
  TRUE
}
