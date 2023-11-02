
#' @title .s3_ms_annotate_features.Settings_annotate_features_StreamFind
#'
#' @description Annotates features with isotopes.
#'
#' @noRd
#'
.s3_ms_annotate_features.Settings_annotate_features_StreamFind <- function(settings, self) {

  if (!any(self$has_features())) {
    warning("Features were not found! Run find_features method first!")
    return(FALSE)
  }

  features <- lapply(self$get_analyses(), function(x) x$features)

  cached_analyses <- FALSE

  if (.caches_data()) {
    hash <- patRoon::makeHash(names(features), features, settings)
    iso_features <- patRoon::loadCacheData("annotate_features", hash)

    if (!is.null(iso_features)) {

      check <- vapply(names(iso_features),
        function(x, iso_features, features) {
          temp_i <- iso_features[[x]]$feature
          temp_f <- features[[x]]$feature
          s_length <- length(temp_i) == length(temp_f)
          s_id <- all(temp_i %in% temp_f)
          all(c(s_length, s_id))
        },
        iso_features = iso_features,
        features = features,
        FALSE
      )

      if (all(check)) {
        cached_analyses <- TRUE
      } else {
        iso_features <- NULL
      }
    }
  } else {
    hash <- NULL
    iso_features <- NULL
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

    iso_output <- foreach(i = features, .packages = "StreamFind", .export = vars
    ) %dopar% {
      do.call("rcpp_ms_annotation_isotopes", c(list("features" = i), parameters))
    }

    names(iso_output) <- names(features)

    iso_group_count <- 0

    iso_features <- lapply(names(iso_output),
      function(x, iso_output, features) {
        temp_i <- copy(iso_output[[x]]$output)
        temp_i[["index"]] <- NULL
        temp_i[["mz"]] <- NULL
        temp_i[["rt"]] <- NULL
        temp_i[["intensity"]] <- NULL

        feature = NULL

        temp_f <- copy(features[[x]])

        `.` <- list

        temp <- temp_f[temp_i, on = .(feature = feature)]

        temp_max_gr <- max(temp$iso_gr)

        temp$iso_gr <- temp$iso_gr + iso_group_count

        iso_group_count <<- iso_group_count + temp_max_gr

        temp
      },
      iso_output = iso_output,
      features = features
    )

    names(iso_features) <- names(iso_output)

    check <- vapply(names(iso_features),
      function(x, iso_features, features) {
        temp_i <- iso_features[[x]]$feature
        temp_f <- features[[x]]$feature
        s_length <- length(temp_i) == length(temp_f)
        s_id <- all(temp_i %in% temp_f)
        all(c(s_length, s_id))
      },
      iso_features = iso_features,
      features = features,
      FALSE
    )

    if (!all(check)) return(FALSE)

    message(" Done!")

    if (!is.null(hash)) {
      patRoon::saveCacheData("annotate_features", iso_features, hash)
      message("\U1f5ab Annotated features cached!")
    }

  } else {
    message("\U2139 Features annotation loaded from cache!")
  }

  self$add_features(iso_features, replace = TRUE)

  TRUE
}
