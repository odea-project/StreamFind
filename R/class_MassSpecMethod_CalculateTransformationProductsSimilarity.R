#' @title MassSpecMethod_CalculateTransformationProductsSimilarity_native Class
#' @description Processing method for calculating MS2 spectral similarity between transformation products and their parent compounds using cosine similarity. This method updates the MassSpecResults_TransformationProducts object with parent similarity scores and maximum shared fragments information.
#' @param mzr The m/z tolerance for fragment matching. Default is 0.008.
#' @param minNumberSharedFragments Minimum number of shared fragments required for filtering. Default is 1.
#' @param minCosine Minimum cosine similarity required for filtering. Default is 0.2.
#' @param filter Logical, if TRUE applies filtering based on minNumberSharedFragments and minCosine. Default is FALSE.
#' @return A `MassSpecMethod_CalculateTransformationProductsSimilarity_native` object.
#' @export
#'
MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(
    mzr = 0.008,
    minNumberSharedFragments = 1,
    minCosine = 0.2,
    filter = FALSE) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "CalculateTransformationProductsSimilarity",
    required = c("FindTransformationProducts"),
    algorithm = "native",
    input_class = "MassSpecResults_TransformationProducts",
    output_class = "MassSpecResults_TransformationProducts",
    parameters = list(
      "mzr" = as.numeric(mzr),
      "minNumberSharedFragments" = as.integer(minNumberSharedFragments),
      "minCosine" = as.numeric(minCosine),
      "filter" = as.logical(filter)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_CalculateTransformationProductsSimilarity_native object!")
  }
}

#' @export
#' @noRd
validate_object.MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "CalculateTransformationProductsSimilarity")
  checkmate::assert_choice(x$algorithm, "native")
  checkmate::assert_number(x$parameters$mzr, lower = 0)
  checkmate::assert_int(x$parameters$minNumberSharedFragments, lower = 0)
  checkmate::assert_number(x$parameters$minCosine, lower = 0, upper = 1)
  checkmate::assert_logical(x$parameters$filter, len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_CalculateTransformationProductsSimilarity_native <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_TransformationProducts)) {
    warning("No MassSpecResults_TransformationProducts object available! Not done.")
    return(FALSE)
  }

  tps <- engine$Results$MassSpecResults_TransformationProducts

  if (length(tps$transformation_products) == 0) {
    warning("MassSpecResults_TransformationProducts object does not have transformation products! Not done.")
    return(FALSE)
  }

  if (nrow(tps$parents) == 0) {
    warning("MassSpecResults_TransformationProducts object does not have parents! Not done.")
    return(FALSE)
  }

  if (!"ms2" %in% colnames(tps$parents)) {
    warning("Parents data table does not have 'ms2' column! Not done.")
    return(FALSE)
  }

  tp_missing_ms2 <- vapply(tps$transformation_products, function(tp) {
    !"ms2" %in% colnames(tp)
  }, logical(1))

  if (all(tp_missing_ms2)) {
    warning("Transformation products data tables do not have 'ms2' column! Not done.")
    return(FALSE)
  }

  message("Calculating MS2 spectral similarity between transformation products and parents...", appendLF = FALSE)

  mzr <- x$parameters$mzr
  minNumberSharedFragments <- x$parameters$minNumberSharedFragments
  minCosine <- x$parameters$minCosine
  filter <- x$parameters$filter

  parents <- tps$parents

  res <- lapply(tps$transformation_products, function(z) {
    ms2_list <- z$ms2
    z$parent_similarity <- 0
    z$max_shared_fragments <- 0L

    for (i in seq_len(nrow(z))) {
      ms2 <- ms2_list[[i]]

      if (is.null(ms2)) next
      if (nrow(ms2) == 0) next

      parent <- parents[parents$name %in% z$parent_name[i], ]
      if (nrow(parent) == 0) next

      parent_ms2_list <- parent$ms2

      max_cosine_similarity <- 0
      max_shared_fragments <- 0

      for (j in seq_len(length(parent_ms2_list))) {
        parent_ms2 <- parent_ms2_list[[j]]

        if (is.null(parent_ms2)) next
        if (nrow(parent_ms2) == 0) next

        fragments <- data.table::copy(parent_ms2)

        mzr_calc <- fragments$mz * mzr / 1E6
        mzr_calc[mzr_calc < mzr] <- mzr
        fragments$mzmin <- fragments$mz - mzr_calc
        fragments$mzmax <- fragments$mz + mzr_calc

        fragments$exp_idx <- vapply(
          seq_len(nrow(fragments)),
          function(z_idx, ms2, fragments) {
            idx <- which(
              ms2$mz >= fragments$mzmin[z_idx] &
                ms2$mz <= fragments$mzmax[z_idx]
            )
            if (length(idx) == 0) {
              NA_integer_
            } else {
              if (length(idx) > 1) {
                candidates <- ms2$mz[idx]
                mz_error <- abs(candidates - fragments$mz[z_idx])
                idx <- idx[which.min(mz_error)]
                idx <- idx[1]
              }
              as.integer(idx)
            }
          },
          ms2 = ms2,
          fragments = fragments,
          integer(1)
        )

        number_shared_fragments <- length(fragments$exp_idx[
          !is.na(fragments$exp_idx)
        ])

        if (number_shared_fragments > 0) {
          fragments$exp_mz <- ms2$mz[fragments$exp_idx]
          fragments$mass_error <- round(
            fragments$mz - fragments$exp_mz,
            digits = 4
          )
          fragments$exp_intensity <- ms2$intensity[fragments$exp_idx]
          fragments$exp_intensity[is.na(fragments$exp_intensity)] <- 0
          sel <- fragments$exp_intensity > 0
          intensity <- fragments$intensity[sel]
          intensity <- intensity / max(intensity)
          intensity_exp <- fragments$exp_intensity[sel]
          intensity_exp <- intensity_exp / max(intensity_exp)
          dot_pro <- intensity * intensity_exp
          dot_pro <- sum(dot_pro)
          mag_int <- sqrt(sum(intensity^2))
          mag_exp_int <- sqrt(sum(intensity_exp^2))
          cosine_similarity <- round(
            dot_pro / (mag_int * mag_exp_int),
            digits = 4
          )

          ms2_unknown <- ms2[
            unique(-fragments$exp_idx[!is.na(fragments$exp_idx)]),
            c("mz", "intensity"),
            with = FALSE
          ]
          if (nrow(ms2_unknown) > 1) {
            ms2_unknown$formula <- "unknown"
            data.table::setnames(
              ms2_unknown,
              c("mz", "intensity"),
              c("exp_mz", "exp_intensity")
            )
            fragments <- data.table::rbindlist(
              list(fragments, ms2_unknown),
              fill = TRUE
            )
          }
        } else {
          cosine_similarity <- 0
        }

        if (cosine_similarity > max_cosine_similarity) {
          max_cosine_similarity <- cosine_similarity
        }
        if (number_shared_fragments > max_shared_fragments) {
          max_shared_fragments <- number_shared_fragments
        }
      }

      z$parent_similarity[i] <- max_cosine_similarity
      z$max_shared_fragments[i] <- max_shared_fragments
    }

    if (filter && (minNumberSharedFragments > 0 || minCosine > 0)) {
      filter_condition <- TRUE
      if (minNumberSharedFragments > 0) {
        filter_condition <- filter_condition & (z$max_shared_fragments >= minNumberSharedFragments)
      }
      if (minCosine > 0) {
        filter_condition <- filter_condition & (z$parent_similarity >= minCosine)
      }
      z <- z[filter_condition, ]
    }

    return(z)
  })

  message(" Done!")

  updated_tps <- MassSpecResults_TransformationProducts(
    parents = tps$parents,
    transformation_products = res
  )

  engine$Results <- updated_tps

  total_tps <- sum(vapply(res, nrow, integer(1)))
  tps_with_similarity <- sum(vapply(res, function(z) sum(z$parent_similarity > 0), integer(1)))

  message("\U2713 Transformation products similarity calculation completed!")
  message("Total transformation products: ", total_tps)
  message("Transformation products with MS2 similarity > 0: ", tps_with_similarity)

  if (filter) {
    message("Filtering applied with minCosine = ", minCosine, " and minSharedFragments = ", minNumberSharedFragments)
  }

  TRUE
}
