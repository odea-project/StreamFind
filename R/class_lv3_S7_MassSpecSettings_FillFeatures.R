
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FillFeatures_StreamFind**
#'
#' @description Settings for filling missing values in features.
#' 
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_FillFeatures_StreamFind.
#'
#' @export
#'
MassSpecSettings_FillFeatures_StreamFind <- S7::new_class("MassSpecSettings_FillFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(rtExpand = 1, mzExpand = 0.0005) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FillFeatures",
      algorithm = "StreamFind",
      parameters = list(rtExpand = rtExpand, mzExpand = mzExpand),
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
      checkmate::test_numeric(self@parameters$rtExpand, len = 1),
      checkmate::test_numeric(self@parameters$mzExpand, len = 1)
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
  
  if (length(engine$analyses) == 0) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_groups()) {
    warning("There are no features groups! Run find_features and group_features first!")
    return(FALSE)
  }

  replicate <- NULL
  group <- NULL
  name <- NULL
  N <- NULL
  . <- NULL
  .N <- NULL
  
  nts <- engine$NTS
  
  cache <- .load_chache("fill_features", nts, x)
  
  if (!is.null(cache$data)) {
    message("\U2139 Filled features loaded from cache!")
    nts <- cache$data
    engine$NTS <- nts
    return(TRUE)
  }
  
  fts <- engine$get_features()
  
  has_adduct_col <- "adduct" %in% colnames(fts)
  
  rpls <- engine$get_replicate_names()
  
  fts$replicate <- rpls[fts$analysis]
  
  fts$name <- paste0(fts$group, "_", fts$replicate)
  
  rpl_presence <- fts[, .N, by = .(name)]
  
  max_anas_in_rpl <- max(rpl_presence$N)
  
  rpl_incomplete <- rpl_presence[N < max_anas_in_rpl]
  
  if (nrow(rpl_incomplete) == 0) {
    warning("There are incomplete replicates! Nothing to do.")
    return(FALSE)
  }
  
  rt_expand <- x$parameters$rtExpand
  
  mz_expand <- x$parameters$mzExpand
  
  to_complete <- fts[fts$name %in% rpl_incomplete$name, ]
  
  to_complete <- split(to_complete, to_complete$name)
  
  analyses <- engine$analyses$names
  
  to_complete <- lapply(to_complete, function(x) {
    
    rpl <- x$replicate[1]
    
    rpl_anas <- analyses[rpls %in% rpl]
    
    missing_anas <- rpl_anas[!rpl_anas %in% x$analysis]
    
    if (!"polarity" %in% names(x)) {
      polarity <- engine$get_spectra_polarity()[x$analysis[1]]
      polarity[polarity == "positive"] <- 1
      polarity[polarity == "negative"] <- -1
    } else {
      polarity <- x$polarity[1]
    }
    
    out <- data.table::data.table(
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
  
  to_complete <- data.table::rbindlist(to_complete)
  to_complete$name <- paste0(to_complete$name, "_", to_complete$analysis)
  
  spec <- engine$get_spectra(mz = data.table::copy(to_complete), levels = 1)
  spec <- split(spec, spec$id)
  
  to_complete <- split(to_complete, to_complete$name)
  
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
  
  new_fts <- data.table::rbindlist(new_fts)
  new_fts$replicate <- NULL
  new_fts$name <- NULL
  new_fts[["id"]] <- NULL
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
    
    cfts <- data.table::rbindlist(list(x, nfts), fill = TRUE)
    
    cfts$analysis <- NULL
    
    cfts
  })
  
  fts_filled <- lapply(fts_filled, function(x) {
    data.table::setnames(x, c("feature", "rt", "rtmin", "rtmax"), c("ID", "ret", "retmin", "retmax"))
    
    # if ("isotope" %in% colnames(x)) {
    #   sel_nulls <- vapply(x$isotope, function(y) {
    #     if (is.null(y)) {
    #       TRUE
    #     } else {
    #       FALSE
    #     }
    #   }, FALSE)
    #   
    #   list_dummies <- lapply(x$ID[sel_nulls], function(y) list())
    #   names(list_dummies) <- x$ID[sel_nulls]
    #   x$isotope[sel_nulls] <- list_dummies
    # }
    
    x
  })
  
  fg <- nts$features
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
  
  nts$features <- fg
  
  if (!is.null(cache$hash)) {
    .save_cache("fill_features", nts, cache$hash)
    message("\U1f5ab Filled features cached!")
  }
  
  engine$NTS <- nts
  TRUE
}
