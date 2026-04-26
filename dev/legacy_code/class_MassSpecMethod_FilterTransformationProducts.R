#' MassSpecMethod_FilterTransformationProducts_StreamFind Class
#'
#' @description Settings for filtering of transformation products.
#'
#' @param minParentSimilarity Numeric (length 1) with the minimum parent similarity threshold.
#' Transformation products with parent_similarity below this value will be filtered out.
#' @param minSharedParentFragments Numeric (length 1) with the minimum number of shared parent fragments.
#' Transformation products with number_shared_fragments below this value will be filtered out.
#'
#' @return A `MassSpecMethod_FilterTransformationProducts_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_FilterTransformationProducts_StreamFind <- function(
  minParentSimilarity = 0,
  minSharedParentFragments = 0
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FilterTransformationProducts",
    required = "FindTransformationProducts",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_TransformationProducts",
    output_class = "MassSpecResults_TransformationProducts",
    parameters = list(
      minParentSimilarity = as.numeric(minParentSimilarity),
      minSharedParentFragments = as.numeric(minSharedParentFragments)
    ),
    number_permitted = Inf,
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
    stop("Invalid MassSpecMethod_FilterTransformationProducts_StreamFind object!")
  }
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_FilterTransformationProducts_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FilterTransformationProducts")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_numeric(x$parameters$minParentSimilarity, len = 1)
  checkmate::assert_numeric(x$parameters$minSharedParentFragments, len = 1)
  NULL
}

#' @export
#' @noRd
#'
run.MassSpecMethod_FilterTransformationProducts_StreamFind <- function(
  x,
  engine = NULL
) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Analyses$results[["MassSpecResults_TransformationProducts"]])) {
    warning("No MassSpecResults_TransformationProducts object available! Not done.")
    return(FALSE)
  }

  tps <- engine$Results$MassSpecResults_TransformationProducts

  if (sum(vapply(tps$transformation_products, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_TransformationProducts object does not have transformation products! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  filters <- names(parameters)

  n_tp_initial <- sum(vapply(
    tps$transformation_products,
    function(x) nrow(x),
    0
  ))

  .filter_minParentSimilarity <- function(value = NULL, engine) {
    if (
      sum(vapply(
        engine$Results$MassSpecResults_TransformationProducts$transformation_products,
        function(z) nrow(z),
        0
      )) > 0 &&
        is.numeric(value) &&
        length(value) == 1
    ) {
      if (is.na(value)) {
        return()
      }

      tps <- engine$Results$MassSpecResults_TransformationProducts
      tp_list <- tps$transformation_products

      tp_list <- lapply(tp_list, function(x) {
        if ("parent_similarity" %in% colnames(x) && nrow(x) > 0) {
          # Filter rows where parent_similarity is below threshold
          sel <- x$parent_similarity < value
          x <- x[!sel, ]
        }
        x
      })

      tps$transformation_products <- tp_list
      engine$Results <- tps
    } else {
      warning("There are no transformation products in the MassSpecEngine!")
    }
  }

  .filter_minSharedParentFragments <- function(value = NULL, engine) {
    if (
      sum(vapply(
        engine$Results$MassSpecResults_TransformationProducts$transformation_products,
        function(z) nrow(z),
        0
      )) > 0 &&
        is.numeric(value) &&
        length(value) == 1
    ) {
      if (is.na(value)) {
        return()
      }

      tps <- engine$Results$MassSpecResults_TransformationProducts
      tp_list <- tps$transformation_products

      tp_list <- lapply(tp_list, function(x) {
        if ("number_shared_fragments" %in% colnames(x) && nrow(x) > 0) {
          # Filter rows where number_shared_fragments is below threshold
          sel <- x$number_shared_fragments < value
          x <- x[!sel, ]
        }
        x
      })

      tps$transformation_products <- tp_list
      engine$Results <- tps
    } else {
      warning("There are no transformation products in the MassSpecEngine!")
    }
  }

  # MARK: Switch Loop
  # __Switch Loop ----

  for (i in seq_len(length(filters))) {
    if (
      is.na(parameters[[filters[i]]]) || length(parameters[[filters[i]]]) == 0
    ) {
      next
    }

    switch(
      filters[i],
      minParentSimilarity = .filter_minParentSimilarity(
        parameters[[filters[i]]],
        engine
      ),
      minSharedParentFragments = .filter_minSharedParentFragments(
        parameters[[filters[i]]],
        engine
      )
    )
  }

  n_tp_after <- sum(vapply(
    engine$Results$MassSpecResults_TransformationProducts$transformation_products,
    function(x) nrow(x),
    0
  ))

  n_tp_filtered <- n_tp_initial - n_tp_after

  if (n_tp_filtered < 0) {
    n_tp_filtered <- 0
  }

  message(paste0("\U2713 ", n_tp_filtered, " transformation products filtered!"))

  TRUE
}


