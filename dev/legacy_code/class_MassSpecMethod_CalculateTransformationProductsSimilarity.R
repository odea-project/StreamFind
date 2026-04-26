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
  tp_list <- tps$transformation_products

  parents_name <- names(tp_list)
  if (is.null(parents_name)) {
    warning("Transformation products list does not have names corresponding to parent names! Not done.")
    return(FALSE)
  }

  tp_list <- Map(function(tp, pn) {
    tp$parent_main <- pn
    tp
  }, tp_list, parents_name)

  # Function to calculate cosine similarity between two spectra
  calculate_cosine_similarity <- function(spec1, spec2, tol = 0.008) {
    if (nrow(spec1) == 0 || nrow(spec2) == 0) return(0)
    # Create intensity vectors for matching m/z values
    all_mz <- sort(unique(c(spec1$mz, spec2$mz)))
    int1 <- numeric(length(all_mz))
    int2 <- numeric(length(all_mz))
    # Fill intensity vectors
    for (i in seq_along(all_mz)) {
      mz_val <- all_mz[i]
      #tol <- mz_val * ppm_tol / 1e6      
      # Find matching peaks in spec1
      match1 <- which(abs(spec1$mz - mz_val) <= tol)
      if (length(match1) > 0) {
        int1[i] <- max(spec1$intensity[match1])  # Use max intensity if multiple matches
      }
      # Find matching peaks in spec2
      match2 <- which(abs(spec2$mz - mz_val) <= tol)
      if (length(match2) > 0) {
        int2[i] <- max(spec2$intensity[match2])  # Use max intensity if multiple matches
      }
    }
    # Calculate cosine similarity
    dot_product <- sum(int1 * int2)
    norm1 <- sqrt(sum(int1^2))
    norm2 <- sqrt(sum(int2^2))
    if (norm1 == 0 || norm2 == 0) return(0)
    return(dot_product / (norm1 * norm2))
  }

  res <- lapply(tp_list, function(z) {
    ms2_list <- z$ms2
    z$parent_similarity <- 0
    z$number_shared_fragments <- 0L
    z$number_parent_fragments <- 0L
    z$number_tp_fragments <- 0L
    for (i in seq_len(nrow(z))) {
      ms2 <- ms2_list[[i]]
      if (is.null(ms2)) next
      if (nrow(ms2) == 0) next
      z$number_tp_fragments <- nrow(ms2)
      parent <- parents[parents$name %in% z$parent_main[i], ]
      if (nrow(parent) == 0) next
      parent_ms2_list <- parent$ms2
      out_cosine_similarity <- 0
      out_shared_fragments <- 0
      out_parent_fragments <- 0
      for (j in seq_len(length(parent_ms2_list))) {
        parent_ms2 <- parent_ms2_list[[j]]
        if (is.null(parent_ms2)) next
        if (nrow(parent_ms2) == 0) next
        fragments <- data.table::copy(parent_ms2)
        similarity <- calculate_cosine_similarity(
          ms2,
          fragments,
          tol = mzr
        )
        matched_peaks <- 0
        for (k in seq_len(nrow(fragments))) {
          parent_mz <- fragments$mz[k]
          if (any(abs(ms2$mz - parent_mz) <= mzr)) {
            matched_peaks <- matched_peaks + 1
          }
        }
        if (matched_peaks > out_shared_fragments) {
          out_cosine_similarity <- similarity
          out_shared_fragments <- matched_peaks
          out_parent_fragments <- nrow(fragments)
        }
      }
      z$parent_similarity[i] <- out_cosine_similarity
      z$number_shared_fragments[i] <- out_shared_fragments
      z$number_parent_fragments[i] <- out_parent_fragments
    }
    if (filter && (minNumberSharedFragments > 0 || minCosine > 0)) {
      filter_condition <- TRUE
      if (minNumberSharedFragments > 0) {
        filter_condition <- filter_condition & (z$number_shared_fragments >= minNumberSharedFragments)
      }
      if (minCosine > 0) {
        filter_condition <- filter_condition & (z$parent_similarity >= minCosine)
      }
      z <- z[filter_condition, ]
    }
    z$parent_main <- NULL
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
