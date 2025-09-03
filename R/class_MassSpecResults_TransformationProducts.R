# MARK: MassSpecResults_TransformationProducts
#' @title Constructor and methods to store transformation products results from non-target analysis
#' @description The `MassSpecResults_TransformationProducts` class is a child of [StreamFind::Results] and is used to store transformation products results from non-target analysis.
#' @param parents A data.table containing parent compound information.
#' @param transformation_products A list containing transformation products for each parent compound.
#' @export
#' @seealso [StreamFind::Results]
#'
MassSpecResults_TransformationProducts <- function(
  parents = data.table::data.table(),
  transformation_products = list()
) {
  x <- structure(
    list(
      type = "MassSpec",
      name = "MassSpecResults_TransformationProducts",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      parents = parents,
      transformation_products = transformation_products
    ),
    class = c("MassSpecResults_TransformationProducts", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecResults_TransformationProducts object!")
  }
}

#' @describeIn MassSpecResults_TransformationProducts Validate the MassSpecResults_TransformationProducts object, returning NULL if valid.
#' @template arg-ms-tp-x
#' @export
#'
validate_object.MassSpecResults_TransformationProducts <- function(x) {
  checkmate::assert_data_table(x$parents)
  checkmate::assert_list(x$transformation_products)
  if (length(x$transformation_products) > 0) {
    if (nrow(x$parents) > 0) {
      checkmate::assert_true(length(x$transformation_products) == nrow(x$parents))
    }
    for (tp in x$transformation_products) {
      checkmate::assert_data_frame(tp)
    }
  }
  NextMethod()
  NULL
}

# MARK: Methods
# Methods ------

# MARK: show
#' @describeIn MassSpecResults_TransformationProducts Show the MassSpecResults_TransformationProducts object.
#' @template arg-ms-tp-x
#' @export
#'
show.MassSpecResults_TransformationProducts <- function(x) {
  cat("Number of parents: ", nrow(x$parents), "\n")
  if (length(x$transformation_products) > 0) {
    tp_counts <- vapply(x$transformation_products, nrow, integer(1))
    cat("Transformation products per parent: ", paste(tp_counts, collapse = ", "), "\n")
    cat("Total transformation products: ", sum(tp_counts), "\n")
  } else {
    cat("Number of transformation products: ", 0, "\n")
  }
}

# MARK: `[`
#' @describeIn MassSpecResults_TransformationProducts Subset the MassSpecResults_TransformationProducts object.
#' @template arg-ms-tp-x
#' @export
#'
`[.MassSpecResults_TransformationProducts` <- function(x, i) {
  x$parents <- x$parents[i, ]
  if (length(x$transformation_products) > 0) {
    x$transformation_products <- x$transformation_products[i]
  }
  x
}
