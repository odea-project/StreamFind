#' MassSpecMethod_FilterSuspects_StreamFind Class
#'
#' @description Settings for filtering of suspects based on screening quality metrics.
#'
#' @param minSharedFragments Numeric (length 1) with the minimum number of shared fragments.
#' Suspects with shared_fragments below this value will be filtered out.
#' @param minCuiness Numeric (length 1) with the minimum cuiness score.
#' Suspects with cusiness below this value will be filtered out.
#' @param maxIdLevel Numeric (length 1) with the maximum identification level.
#' Suspects with id_level above this value will be filtered out.
#'
#' @return A `MassSpecMethod_FilterSuspects_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_FilterSuspects_StreamFind <- function(
  minSharedFragments = NA_real_,
  minCuiness = NA_real_,
  maxIdLevel = NA_real_
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "FilterSuspects",
    required = "SuspectScreening",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      minSharedFragments = as.numeric(minSharedFragments),
      minCuiness = as.numeric(minCuiness),
      maxIdLevel = as.numeric(maxIdLevel)
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
    stop("Invalid MassSpecMethod_FilterSuspects_StreamFind object!")
  }
}

#' @export
#' @noRd
#'
validate_object.MassSpecMethod_FilterSuspects_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "FilterSuspects")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_numeric(x$parameters$minSharedFragments, len = 1)
  checkmate::assert_numeric(x$parameters$minCuiness, len = 1)
  checkmate::assert_numeric(x$parameters$maxIdLevel, len = 1)
  NULL
}

#' @export
#' @noRd
#'
run.MassSpecMethod_FilterSuspects_StreamFind <- function(
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

  if (is.null(engine$Analyses$results[["MassSpecResults_NonTargetAnalysis"]])) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (sum(vapply(nts$features, function(z) nrow(z), 0)) == 0) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters
  filters <- names(parameters)

  # Count initial suspects
  n_suspects_initial <- sum(vapply(
    nts$features,
    function(x) {
      if ("suspects" %in% colnames(x)) {
        sum(vapply(x$suspects, function(s) {
          if (is.data.frame(s) && nrow(s) > 0) nrow(s) else 0
        }, 0))
      } else {
        0
      }
    },
    0
  ))

  .filter_minSharedFragments <- function(value = NULL, engine) {
    if (
      sum(vapply(
        engine$Results$MassSpecResults_NonTargetAnalysis$features,
        function(z) nrow(z),
        0
      )) > 0 &&
        is.numeric(value) &&
        length(value) == 1
    ) {
      if (is.na(value)) {
        return()
      }

      nts <- engine$Results$MassSpecResults_NonTargetAnalysis
      features <- nts$features

      features <- lapply(features, function(x) {
        if ("suspects" %in% colnames(x) && nrow(x) > 0) {
          x$suspects <- lapply(x$suspects, function(s) {
            if (is.data.frame(s) && nrow(s) > 0 && "shared_fragments" %in% colnames(s)) {
              # Filter rows where shared_fragments is below threshold
              s <- s[s$shared_fragments >= value, ]
            }
            s
          })
        }
        x
      })

      nts$features <- features
      engine$Results <- nts
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }

  .filter_minCuiness <- function(value = NULL, engine) {
    if (
      sum(vapply(
        engine$Results$MassSpecResults_NonTargetAnalysis$features,
        function(z) nrow(z),
        0
      )) > 0 &&
        is.numeric(value) &&
        length(value) == 1
    ) {
      if (is.na(value)) {
        return()
      }

      nts <- engine$Results$MassSpecResults_NonTargetAnalysis
      features <- nts$features

      features <- lapply(features, function(x) {
        if ("suspects" %in% colnames(x) && nrow(x) > 0) {
          x$suspects <- lapply(x$suspects, function(s) {
            if (is.data.frame(s) && nrow(s) > 0 && "cusiness" %in% colnames(s)) {
              # Filter rows where cusiness is below threshold
              s <- s[s$cusiness >= value, ]
            }
            s
          })
        }
        x
      })

      nts$features <- features
      engine$Results <- nts
    } else {
      warning("There are no features in the MassSpecEngine!")
    }
  }

  .filter_maxIdLevel <- function(value = NULL, engine) {
    if (
      sum(vapply(
        engine$Results$MassSpecResults_NonTargetAnalysis$features,
        function(z) nrow(z),
        0
      )) > 0 &&
        is.numeric(value) &&
        length(value) == 1
    ) {
      if (is.na(value)) {
        return()
      }

      nts <- engine$Results$MassSpecResults_NonTargetAnalysis
      features <- nts$features

      features <- lapply(features, function(x) {
        if ("suspects" %in% colnames(x) && nrow(x) > 0) {
          x$suspects <- lapply(x$suspects, function(s) {
            if (is.data.frame(s) && nrow(s) > 0 && "id_level" %in% colnames(s)) {
              # Filter rows where id_level is above threshold
              s <- s[s$id_level <= value, ]
            }
            s
          })
        }
        x
      })

      nts$features <- features
      engine$Results <- nts
    } else {
      warning("There are no features in the MassSpecEngine!")
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
      minSharedFragments = .filter_minSharedFragments(
        parameters[[filters[i]]],
        engine
      ),
      minCuiness = .filter_minCuiness(
        parameters[[filters[i]]],
        engine
      ),
      maxIdLevel = .filter_maxIdLevel(
        parameters[[filters[i]]],
        engine
      )
    )
  }

  # Count suspects after filtering
  n_suspects_after <- sum(vapply(
    engine$Results$MassSpecResults_NonTargetAnalysis$features,
    function(x) {
      if ("suspects" %in% colnames(x)) {
        sum(vapply(x$suspects, function(s) {
          if (is.data.frame(s) && nrow(s) > 0) nrow(s) else 0
        }, 0))
      } else {
        0
      }
    },
    0
  ))

  n_suspects_filtered <- n_suspects_initial - n_suspects_after

  if (n_suspects_filtered < 0) {
    n_suspects_filtered <- 0
  }

  message(paste0("\U2713 ", n_suspects_filtered, " suspects filtered!"))

  TRUE
}


