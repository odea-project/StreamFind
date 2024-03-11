
#' @title .s3_ms_annotate_features.Settings_annotate_features_StreamFind
#'
#' @description Annotates features with isotopes.
#'
#' @noRd
#'
.s3_ms_annotate_features.Settings_annotate_features_StreamFind <- function(settings, self, private) {

  if (!any(self$has_features())) {
    warning("Features were not found! Run find_features method first!")
    return(FALSE)
  }

  features <- self$feature_list
  
  features <- lapply(features, function(x) {
    x$index <- seq_len(nrow(x))
    x
  })
  
  cached_analyses <- FALSE

  if (.caches_data()) {
    hash <- patRoon::makeHash(names(features), features, settings)
    isotopes <- patRoon::loadCacheData("annotate_features", hash)

    if (!is.null(isotopes)) cached_analyses <- TRUE
    
  } else {
    hash <- NULL
    isotopes <- NULL
  }

  parameters <- settings$parameters

  if (parameters$runParallel & length(features) > 1 & !cached_analyses) {
    workers <- parallel::detectCores() - 1
    if (length(features) < workers) workers <- length(features)
    par_type <- "PSOCK"
    if (parallelly::supportsMulticore()) par_type <- "FORK"
    cl <- parallel::makeCluster(workers, type = par_type)
    doParallel::registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }

  parameters$runParallel <- NULL

  if (!cached_analyses) {
    message("\U2699 Annotating features from ",
      length(self$get_analyses()),
      " analyses...",
      appendLF = FALSE
    )

    i <- NULL

    vars <- c("rcpp_ms_annotation_isotopes")

    isotopes <- foreach(i = features, .packages = "StreamFind", .export = vars) %dopar% {
      do.call("rcpp_ms_annotation_isotopes", c(list("features" = i), parameters))
    }

    names(isotopes) <- names(features)
  
    message(" Done!")
    
    if (!is.null(hash)) {
      patRoon::saveCacheData("annotate_features", isotopes, hash)
      message("\U1f5ab Annotated features cached!")
    }

  } else {
    message("\U2139 Features annotation loaded from cache!")
  }
  
  iso_col <- lapply(names(isotopes), function(x, isotopes, features) {
    
    temp_i <- isotopes[[x]]$output2
    
    temp_i_fts <- vapply(temp_i, function(x) x$feature, NA_character_)
    
    names(temp_i) <- temp_i_fts
    
    temp_f <- copy(features[[x]])
    
    temp_f_fts <- temp_f$feature
    
    if (!all(temp_i_fts %in% temp_f_fts)) {
      stop("Annotated features do not exist in features!")
    }
    
    temp_i <- temp_i[temp_f_fts]
    
    if (!identical(names(temp_i), temp_f_fts)) {
      stop("Annotated features do not match features!")
    }

    temp_i
  },
  isotopes = isotopes,
  features = features
  )
  
  names(iso_col) <- names(features)
  
  if (private$.add_features_column("isotope", iso_col)) {
    
    message(paste0("\U2713 ", "Features annotated!"))
    
    TRUE
    
  } else {
    FALSE
  }
}
