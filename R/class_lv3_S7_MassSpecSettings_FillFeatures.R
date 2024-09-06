
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FillFeatures_StreamFind**
#'
#' @description Settings for filling missing values in features.
#' 
#' @template arg-ms-rtExpand
#' @template arg-ms-mzExpand
#' @param noise Numeric of length one with the noise threshold.
#' @param withinReplicate Logical of length one to fill within replicates not global.
#'
#' @return A ProcessingSettings S3 class object with subclass MassSpecSettings_FillFeatures_StreamFind.
#'
#' @export
#'
MassSpecSettings_FillFeatures_StreamFind <- S7::new_class("MassSpecSettings_FillFeatures_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(rtExpand = 1, mzExpand = 0.0005, noise  = 1000, withinReplicate = FALSE) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FillFeatures",
      algorithm = "StreamFind",
      parameters = list(
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        noise = noise,
        withinReplicate = withinReplicate
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
      checkmate::test_numeric(self@parameters$rtExpand, len = 1),
      checkmate::test_numeric(self@parameters$mzExpand, len = 1),
      checkmate::test_numeric(self@parameters$noise, len = 1),
      checkmate::test_logical(self@parameters$withinReplicate, len = 1)
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

  replicate <- NULL
  group <- NULL
  name <- NULL
  N <- NULL
  . <- NULL
  .N <- NULL
  
  cache <- .load_chache("fill_features", nts$features, x)
  
  if (!is.null(cache$data)) {
    message("\U2139 Filled features loaded from cache!")
    nts$features <- cache$data
    engine$nts <- nts
    return(TRUE)
  }
  
  parameters <- x$parameters
  
  fts <- engine$get_features()
  
  has_adduct_col <- "adduct" %in% colnames(fts)
  
  if(parameters$withinReplicate) {
    rpls <- engine$get_replicate_names()
    fts$replicate <- rpls[fts$analysis]
    fts$name <- paste0(fts$group, "_", fts$replicate)
    eval_presence <- fts[, .N, by = .(name)]
    max_presence <- max(eval_presence$N)
    set_incomplete <- eval_presence[N < max_presence]
  } else {
    fts$name <- fts$group
    eval_presence <- fts[, .N, by = .(name)]
    max_presence <- max(length(engine$analyses))
    set_incomplete <- eval_presence[N < max_presence]
  }
  
  if (nrow(set_incomplete) == 0) {
    warning("There are incomplete replicates! Nothing to do.")
    return(FALSE)
  }
  
  rt_expand <- x$parameters$rtExpand
  mz_expand <- x$parameters$mzExpand
  
  to_complete <- fts[fts$name %in% set_incomplete$name, ]
  to_complete <- split(to_complete, to_complete$name)
  
  analyses <- engine$analyses$names
  polarities <- engine$get_spectra_polarity()
  
  browser()
  
  to_complete <- lapply(to_complete, function(x) {
    
    if (parameters$withinReplicate) {
      rpl <- x$replicate[1]
      rpl_anas <- analyses[rpls %in% rpl]
      missing_anas <- rpl_anas[!rpl_anas %in% x$analysis]
    } else {
      missing_anas <- analyses[!analyses %in% x$analysis]
    }
    
    polarity <- polarities[missing_anas]
    polarity[polarity == "positive"] <- 1
    polarity[polarity == "negative"] <- -1
    polarity <- as.numeric(polarity)
    
    mass <- rep(mean(x$mass), length(missing_anas))
    mz <- mass + (polarity * 1.007276)
    mzmin <- (mz - max(x$mz - x$mzmin)) - mz_expand
    mzmax <- (mz + max(x$mzmax - x$mz)) + mz_expand
    
    out <- data.table::data.table(
      analysis = missing_anas,
      feature = NA_character_,
      rt = mean(x$rt),
      mz = mz,
      area = 0,
      intensity = 0,
      rtmin = min(x$rtmin) - rt_expand,
      rtmax = max(x$rtmax) + rt_expand,
      mzmin = mzmin,
      mzmax = mzmax,
      filtered = FALSE,
      mass = mass,
      polarity = polarity,
      group = x$group[1]
    )
    
    # TODO add drift and other info when present
    
    out
  })
  
  to_complete <- data.table::rbindlist(to_complete)
  to_complete$name <- paste0(to_complete$group, "_", to_complete$analysis)
  
  spec <- engine$get_spectra(mz = data.table::copy(to_complete), levels = 1, minIntensityMS1 = parameters$noise)
  spec <- split(spec, spec$id)
  
  to_complete <- split(to_complete, to_complete$name)
  
  browser()
  
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
  
  if (!is.null(cache$hash)) {
    .save_cache("fill_features", fg, cache$hash)
    message("\U1f5ab Filled features cached!")
  }
  
  nts$features <- fg
  engine$nts <- nts
  TRUE
}
