#' @title MassSpecMethod_SuspectScreening_StreamFind Class
#' @description Processing method for suspect screening using a data.frame with target compounds.
#' @param database A data.frame with at least the columns name and mass, indicating the name and neutral monoisotopic mass of the suspect targets.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-ppmMS2
#' @param mzrMS2 Numeric length 1 with the absolute m/z range window for matching MS2 fragments with experimental MS2 traces.
#' @template arg-ms-minFragments
#' @param minCusiness Numeric length 1 with the minimum cusiness value of the experimental and database fragments for a feature to be considered a suspect.
#' @template arg-ms-filtered
#' @return A `MassSpecMethod_SuspectScreening_StreamFind` object.
#' @export
#'
MassSpecMethod_SuspectScreening_StreamFind <- function(
  database = data.table::data.table(
    name = character(),
    formula = character(),
    mass = numeric()
  ),
  ppm = 5,
  sec = 10,
  ppmMS2 = 10,
  mzrMS2 = 0.008,
  minCusiness = 0.5,
  minFragments = 3,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "SuspectScreening",
    required = "FindFeatures",
    algorithm = "StreamFind",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "database" = data.table::as.data.table(database),
      "ppm" = as.numeric(ppm),
      "sec" = as.numeric(sec),
      "ppmMS2" = as.numeric(ppmMS2),
      "mzrMS2" = as.numeric(mzrMS2),
      "minCusiness" = as.numeric(minCusiness),
      "minFragments" = as.numeric(minFragments),
      "filtered" = as.logical(filtered)
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
    stop("Invalid MassSpecMethod_SuspectScreening_StreamFind object!")
  }
}

#' @describeIn MassSpecMethod_SuspectScreening_StreamFind Validate the MassSpecMethod_SuspectScreening_StreamFind object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_SuspectScreening_StreamFind` object.
#' @export
#'
validate_object.MassSpecMethod_SuspectScreening_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_number(x$parameters$ppm)
  checkmate::assert_number(x$parameters$sec)
  checkmate::assert_number(x$parameters$ppmMS2)
  checkmate::assert_number(x$parameters$mzrMS2)
  checkmate::assert_number(x$parameters$minCusiness)
  checkmate::assert_number(x$parameters$minFragments)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  checkmate::assert_data_table(x$parameters$database)
  checkmate::assert_true(
    all(c("name", "mass") %in% colnames(x$parameters$database)) ||
      all(c("name", "neutralMass") %in% colnames(x$parameters$database)) ||
      all(c("name", "mz") %in% colnames(x$parameters$database))
  )
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SuspectScreening_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_NonTargetAnalysis)) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  suspect_features <- get_suspects(
    nts,
    database = parameters$database,
    ppm = parameters$ppm,
    sec = parameters$sec,
    ppmMS2 = parameters$ppmMS2,
    mzrMS2 = parameters$mzrMS2,
    minCusiness = parameters$minCusiness,
    minFragments = parameters$minFragments,
    filtered = parameters$filtered
  )

  if (nrow(suspect_features) > 0) {
    suspect_cols <- colnames(suspect_features)
    suspect_features_l <- split(suspect_features, suspect_features$analysis)
    features <- nts$features
    sus_col <- lapply(
      names(features),
      function(a, features, suspect_features_l, suspect_cols) {
        suspects <- suspect_features_l[[a]]
        fts <- features[[a]]
        if (!is.null(suspects)) {
          suspects_l <- lapply(
            fts$feature,
            function(z, suspects, suspect_cols) {
              sus_idx <- which(suspects$feature %in% z)
              if (length(sus_idx) > 0) {
                sus_temp <- suspects[sus_idx, ]
                sus_temp[["analysis"]] <- NULL
                if (nrow(sus_temp) > 0) {
                  sus_temp
                } else {
                  data.table::data.table()
                }
              } else {
                data.table::data.table()
              }
            },
            suspects = suspects,
            suspect_cols = suspect_cols
          )
          suspects_l
        } else {
          lapply(fts$feature, function(x) data.table::data.table())
        }
      },
      features = features,
      suspect_features_l = suspect_features_l,
      suspect_cols = suspect_cols
    )
    names(sus_col) <- names(features)
    features <- Map(
      function(fts, i) {
        fts$suspects <- i
        fts
      },
      features,
      sus_col
    )
    nts$features <- features
    engine$Results <- nts
    TRUE
  } else {
    message("\U26a0 No suspects found!")
    FALSE
  }
}

#' @title MassSpecMethod_SuspectScreening_forident Class
#' @description Settings for performing suspect screening using the \href{https://water.for-ident.org/}{FOR-IDENT} platform.
#' @param addMS2 Logical length 1. When `TRUE` and MS2 data is available, the fragments pattern (i.e., MS2 averaged spectra) is added to the .txt file to import in FOR-IDENT platform. Note that when `addMS2` is `TRUE` the \emph{m/z} values are used instead of neutral mass even is `useNeutralMass` is set to `TRUE`.
#' @param useNeutralMass Logical length 1. When `TRUE` and neutral mass is available, the neutral mass of features/feature groups is used instead of the \emph{m/z}.
#' @param path Character length 1 with the path to save the .txt file with the list of features for identification.
#' @param name Character length 1 with the name of the file (without extension) to be saved in the `path`.
#' @note
#' After processing, a .txt file as defined by name and path is created with the list of features or feature groups to be imported in the FOR-IDENT platform (\url{https://water.for-ident.org/}). Note that log in credentials are needed.
#' @return A `MassSpecMethod_SuspectScreening_forident` object.
#' @export
#'
MassSpecMethod_SuspectScreening_forident <- function(
  addMS2 = FALSE,
  useNeutralMass = TRUE,
  path = getwd(),
  name = "feature_list"
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "SuspectScreening",
    algorithm = "forident",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "addMS2" = addMS2,
      "useNeutralMass" = useNeutralMass,
      "path" = path,
      "name" = name
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "forident",
    developer = "Sylvia Grosse, Thomas Letzel",
    contact = "support@for-ident.org",
    link = "https://water.for-ident.org/#!home",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_SuspectScreening_forident object!")
  }
}

#' @describeIn MassSpecMethod_SuspectScreening_forident Validate the MassSpecMethod_SuspectScreening_forident object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_SuspectScreening_forident` object.
#' @export
#'
validate_object.MassSpecMethod_SuspectScreening_forident <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "forident")
  checkmate::assert_true(dir.exists(x$parameters$path))
  checkmate::assert_true(is.character(x$parameters$name))
  checkmate::assert_true(length(x$parameters$name) == 1)
  checkmate::assert_logical(x$parameters$addMS2, max.len = 1)
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SuspectScreening_forident <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_NonTargetAnalysis)) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  has_groups <- any(vapply(nts$features, function(z) any(!(is.na(z$group) | z$group %in% "")), FALSE))

  if (has_groups) {
    polarities <- vapply(
      nts$headers,
      function(a) {
        paste0(unique(a$polarity), collapse = ", ")
      },
      NA_character_
    )
    if (length(polarities) > 1 && x$parameters$addMS2) {
      warning(
        "Using ms2 data of feature groups from multiple polarities ",
        "is not possible! Using features of each analysis instead."
      )

      out_list <- get_features(nts)
      out_list$rt <- out_list$rt / 60
      out_list$label <- paste0(out_list$analysis, "_", out_list$feature)
    } else {
      out_list <- get_groups(nts, metadata = TRUE)
      out_list$rt <- out_list$rt / 60
      out_list$label <- out_list$group
    }
  } else {
    out_list <- engine$get_features()
    out_list$rt <- out_list$rt / 60
    out_list$label <- paste0(out_list$analysis, "_", out_list$feature)
  }

  if (x$parameters$useNeutralMass & "mass" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mass
  } else if ("mz" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mz
  } else {
    out_list$Mass <- out_list$mass
  }

  if (!x$parameters$addMS2) {
    sink(paste0(x$parameters$path, "/", x$parameters$name, ".txt"))
    cat("\n")
    cat("\n")

    for (i in seq_len(nrow(out_list))) {
      cat("NAME: ")
      cat(out_list$label[i])
      cat("\n")
      cat("RETENTIONTIME: ")
      cat(round(out_list$rt[i], digits = 3))
      cat("\n")
      cat("Mass: ")
      cat(round(out_list$Mass[i], digits = 4))
      cat("\n")
      cat("Formula: ")
      cat("\n")
      cat("//")
      cat("\n")
      cat("\n")
    }
    sink()

    message(
      "\U2713 List saved as ",
      paste0(settings$parameters$path, "/", settings$parameters$name, ".txt")
    )
  } else {
    if (has_groups) {
      ms2 <- get_groups_ms2(nts)
      ms2 <- split(ms2, ms2$group)
      out_list$ms2 <- lapply(
        out_list$group,
        function(x, ms2) {
          if (x %in% names(ms2)) {
            ms2[[x]]
          } else {
            NULL
          }
        },
        ms2 = ms2
      )
    }

    if ("ms2" %in% colnames(out_list)) {
      if ("mass" %in% colnames(out_list) && x$parameters$useNeutralMass) {
        out_list$Mass <- out_list$mass
      } else if ("mz" %in% colnames(out_list)) {
        out_list$Mass <- out_list$mz
      } else {
        warning("Mass data not found!")
        return(FALSE)
      }

      sink(paste0(x$parameters$path, "/", x$parameters$name, ".txt"))
      cat("\n")
      cat("\n")

      for (i in seq_len(nrow(out_list))) {
        cat("NAME: ")
        cat(out_list$label[i])
        cat("\n")
        cat("RETENTIONTIME: ")
        cat(round(out_list$rt[i], digits = 3))
        cat("\n")
        cat("PRECURSORMZ: ")
        cat(round(out_list$Mass[i], digits = 4))
        cat("\n")
        cat("Formula: ")
        cat("\n")
        if (is.data.frame(out_list$ms2[[i]])) {
          temp_ms2 <- out_list$ms2[[i]]
          for (j in seq_len(nrow(temp_ms2))) {
            cat(
              paste(
                round(temp_ms2$mz[j], digits = 4),
                round(temp_ms2$intensity[j], digits = 0),
                sep = " "
              )
            )
            cat(" ")
          }
          rm(j, temp_ms2)
        } else {
          cat("N/A")
        }
        cat("\n")
        cat("//")
        cat("\n")
        cat("\n")
      }
      sink()

      message(
        "\U2713 List saved as ",
        paste0(x$parameters$path, "/", x$parameters$name, ".txt")
      )
    } else {
      warning(
        "MS2 data not found! Load features and/or feature groups MS2 data."
      )
      return(FALSE)
    }
  }
  TRUE
}

#' @title MassSpecMethod_SuspectScreening_patRoon Class
#' @description Settings for performing suspect screening using the function \link[patRoon]{screenSuspects} from the patRoon R package.
#' @param suspects A data.frame with suspect information. See section Suspect list format in \link[patRoon]{screenSuspects} for more information.
#' @param rtWindow The retention time window (in seconds) that will be used for matching a suspect (+/- feature data).
#' @param mzWindow The m/z window that will be used for matching a suspect (+/- feature data).
#' @template arg-ms-filtered
#' @return A `MassSpecMethod_SuspectScreening_patRoon` object.
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
MassSpecMethod_SuspectScreening_patRoon <- function(
  suspects = data.table::data.table(),
  rtWindow = 12,
  mzWindow = 0.005,
  filtered = FALSE
) {
  x <- ProcessingStep(
    type = "MassSpec",
    method = "SuspectScreening",
    algorithm = "patRoon",
    input_class = "MassSpecResults_NonTargetAnalysis",
    output_class = "MassSpecResults_NonTargetAnalysis",
    parameters = list(
      "suspects" = data.table::as.data.table(suspects),
      "rtWindow" = as.numeric(rtWindow),
      "mzWindow" = as.numeric(mzWindow),
      "filtered" = as.logical(filtered)
    ),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "patRoon",
    developer = "Rick Helmus",
    contact = "r.helmus@uva.nl",
    link = "https://github.com/rickhelmus/patRoon",
    doi = "https://doi.org/10.1186/s13321-020-00477-w"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecMethod_SuspectScreening_patRoon object!")
  }
}

#' @describeIn MassSpecMethod_SuspectScreening_patRoon Validate the MassSpecMethod_SuspectScreening_patRoon object, returning `NULL` if valid.
#' @param x A `MassSpecMethod_SuspectScreening_patRoon` object.
#' @export
#'
validate_object.MassSpecMethod_SuspectScreening_patRoon <- function(x) {
  checkmate::assert_choice(x$type, "MassSpec")
  checkmate::assert_choice(x$method, "SuspectScreening")
  checkmate::assert_choice(x$algorithm, "patRoon")
  checkmate::assert_number(x$parameters$rtWindow)
  checkmate::assert_number(x$parameters$mzWindow)
  checkmate::assert_logical(x$parameters$filtered, max.len = 1)
  checkmate::assert_data_table(x$parameters$suspects)
  if (nrow(x$parameters$suspects) > 0) {
    if (
      !(all(c("name", "neutralMass") %in% colnames(x$parameters$suspects)) ||
        all(c("name", "mz") %in% colnames(x$parameters$suspects)))
    ) {
      stop(
        "Suspects must have at least the columns name and neutralMass or name and mz!"
      )
    }
  }
  NULL
}

#' @export
#' @noRd
run.MassSpecMethod_SuspectScreening_patRoon <- function(x, engine = NULL) {
  if (FALSE & requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }

  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (nrow(x$parameters$suspects) == 0) {
    warning("Suspects data.frame is empty! Not done.")
    return(FALSE)
  }

  if (is.null(engine$Results$MassSpecResults_NonTargetAnalysis)) {
    warning("No MassSpecResults_NonTargetAnalysis object available! Not done.")
    return(FALSE)
  }

  nts <- engine$Results$MassSpecResults_NonTargetAnalysis

  if (
    sum(vapply(nts$features, function(z) nrow(z), 0)) == 0
  ) {
    warning("MassSpecResults_NonTargetAnalysis object does not have features! Not done.")
    return(FALSE)
  }

  has_groups <- any(vapply(nts$features, function(z) any(!(is.na(z$group) | z$group %in% "")), FALSE))

  if (!has_groups) {
    warning(
      "MassSpecResults_NonTargetAnalysis object does not have feature groups! Not done."
    )
    return(FALSE)
  }

  parameters <- x$parameters

  res <- patRoon::screenSuspects(
    fGroups = get_patRoon_features(nts),
    suspects = parameters$suspects,
    rtWindow = parameters$rtWindow,
    mzWindow = parameters$mzWindow,
    skipInvalid = TRUE,
    prefCalcChemProps = TRUE,
    neutralChemProps = TRUE,
    onlyHits = TRUE,
    adduct = NULL
  )

  suspect_list <- res@screenInfo

  features <- nts$features

  features <- lapply(
    features,
    function(fts, suspect_list) {
      has_suspect_features <- any(suspect_list$group %in% fts$group)

      if (has_suspect_features) {
        suspects_l <- lapply(
          seq_len(nrow(fts)),
          function(z, fts, suspect_list) {
            ft <- fts[z, ]

            sus_idx <- which(suspect_list$group %in% ft$group)

            if (length(sus_idx) > 0) {
              sus_temp <- suspect_list[sus_idx, ]

              if ("rt" %in% colnames(sus_temp)) {
                if (!is.na(sus_temp$rt)) {
                  sus_temp$d_rt <- sus_temp$rt - ft$rt
                  sus_temp$d_rt <- round(sus_temp$d_rt, digits = 1)
                }
                setnames(sus_temp, "rt", "exp_rt")
                setnames(sus_temp, "d_rt", "error_rt")
              }

              if ("neutralMass" %in% colnames(sus_temp)) {
                sus_temp$d_mz <- (sus_temp$neutralMass - ft$mass) /
                  ft$mass *
                  1E6
                sus_temp$d_mz <- round(sus_temp$d_mz, digits = 1)
                setnames(sus_temp, "neutralMass", "exp_mass")
                setnames(sus_temp, "d_mz", "error_mass")
              }

              # TODO make case for mz column

              sus_temp[["group"]] <- NULL
              sus_temp[["sets"]] <- NULL
              sus_temp[["molNeutralized-negative"]] <- NULL
              sus_temp[["molNeutralized-positive"]] <- NULL
              sus_temp[["molNeutralized"]] <- NULL

              if (nrow(sus_temp) > 0) {
                sus_temp
              } else {
                NULL
              }
            } else {
              NULL
            }
          },
          fts = fts, suspect_list = suspect_list
        )

        fts$suspects <- suspects_l
      }
      fts
    },
    suspect_list = suspect_list
  )
  nts$features <- features
  engine$Results <- nts
  message("\U2713 Suspect screening done with patRoon!")
  TRUE
}
