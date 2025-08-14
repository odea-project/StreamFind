#' @title MassSpecMethod_ClusterSpectra_StreamFind Class
#'
#' @description Clusters spectra based on a variable (i.e. column name).
#'
#' @param val Character (length 1) with the variable to be used for clustering.
#' @param clustVal Numeric (length 1) with the clustering value.
#' @param presence Numeric (length 1) with the minimum presence of traces in a cluster to be
#' considered.
#'
#' @return A MassSpecMethod_ClusterSpectra_StreamFind object.
#'
#' @export
#'
MassSpecMethod_ClusterSpectra_StreamFind <- function(
  val = "mz",
  clustVal = 0.001,
  presence = 0.1
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "ClusterSpectra",
    required = "LoadSpectra",
    algorithm = "StreamFind",
    parameters = list(
      val = as.character(val),
      clustVal = as.numeric(clustVal),
      presence = as.numeric(presence)
    ),
    number_permitted = 1,
    version = as.character(utils::packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_ClusterSpectra_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_ClusterSpectra_StreamFind Validate the MassSpecMethod_ClusterSpectra_StreamFind object, returning NULL if valid.+
#' @param x A MassSpecMethod_ClusterSpectra_StreamFind object.
#' @export
#'
validate_object.MassSpecMethod_ClusterSpectra_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "ClusterSpectra")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_character(x$parameters$val, min.len = 1)
  checkmate::assert_number(x$parameters$clustVal)
  checkmate::assert_number(x$parameters$presence)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_ClusterSpectra_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["MassSpecResults_Spectra"]])) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  spec_obj <- engine$Results[["MassSpecResults_Spectra"]]
  spec_list <- spec_obj$spectra
  val <- x$parameters$val
  clustVal <- x$parameters$clustVal
  presence <- x$parameters$presence
  has_val <- all(vapply(
    spec_list,
    function(x) {
      if (nrow(x) > 0) {
        val %in% colnames(x)
      } else {
        TRUE
      }
    },
    FALSE
  ))
  if (!has_val) {
    warning("Val not found in spectra data.tables! Not done.")
    return(FALSE)
  }
  number_analyses <- length(spec_list)
  processed <- 0
  spec_list <- lapply(
    spec_list,
    function(z, val, clustVal, presence) {
      if (nrow(z) > 0) {
        z$mz <- z[[val]]
        z$unique_id <- z$id
        z$analysis <- ""
        res <- rcpp_ms_cluster_spectra(z, clustVal, presence, FALSE)
        res <- data.table::rbindlist(res, fill = TRUE)
        if (nrow(res) == 0) {
          return(z)
        }
        res <- res[order(res$mz), ]
        res <- res[order(res$id), ]
        res$analysis <- NULL
        data.table::setnames(res, "mz", val)
        processed <<- processed + 1
        message(paste0(
          "\U2713 ",
          "Processed ",
          processed,
          " of ",
          number_analyses,
          " analyses!"
        ))
        return(res)
      } else {
        message(
          paste0(
            "\U2713 ",
            "No data to process for ",
            processed,
            " of ",
            number_analyses,
            " analyses!"
          )
        )
        return(z)
      }
    },
    val = val,
    clustVal = clustVal,
    presence = presence
  )
  spec_obj$spectra <- spec_list
  engine$Results <- spec_obj
  message(paste0("\U2713 ", "Spectra clustered!"))
  TRUE
}
