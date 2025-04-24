#' MassSpecMethod_SuspectScreening_StreamFind S7 class
#'
#' @description Settings for performing suspect screening using a data.frame with target compounds.
#'
#' @param database A data.frame with at least the columns name and mass, indicating the name and
#' neutral monoisotopic mass of the suspect targets.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#' @template arg-ms-ppmMS2
#' @param mzrMS2 Numeric length 1 with the absolute m/z range window for matching MS2 fragments with
#' experimental MS2 traces.
#' @template arg-ms-minFragments
#' @param minCusiness Numeric length 1 with the minimum cusiness value of the experimental and
#' database fragments for a feature to be considered a suspect.
#' @template arg-ms-filtered
#'
#' @return A `MassSpecMethod_SuspectScreening_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_SuspectScreening_StreamFind <- S7::new_class(
  name = "MassSpecMethod_SuspectScreening_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(database = data.table::data.table(
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
                         filtered = FALSE) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "SuspectScreening",
        required = "FindFeatures",
        algorithm = "StreamFind",
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
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "SuspectScreening")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$ppm)
    checkmate::assert_number(self@parameters$sec)
    checkmate::assert_number(self@parameters$ppmMS2)
    checkmate::assert_number(self@parameters$mzrMS2)
    checkmate::assert_number(self@parameters$minCusiness)
    checkmate::assert_number(self@parameters$minFragments)
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    checkmate::assert_data_table(self@parameters$database)
    checkmate::assert_true(
      all(c("name", "mass") %in% colnames(self@parameters$database)) ||
      all(c("name", "neutralMass") %in% colnames(self@parameters$database)) ||
      all(c("name", "mz") %in% colnames(self@parameters$database))
    )
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_SuspectScreening_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }
  
  nts <- engine$NonTargetAnalysisResults
  
  if (!nts@has_features) {
    warning("NonTargetAnalysisResults object is empty! Not done.")
    return(FALSE)
  }
  
  parameters <- x@parameters
  
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
    
    features <- nts$feature_list
    
    sus_col <- lapply(names(features), function(x, features, suspect_features_l, suspect_cols) {
      suspects <- suspect_features_l[[x]]
      fts <- features[[x]]
      
      if (!is.null(suspects)) {
        suspects_l <- lapply(fts$feature, function(z, suspects, suspect_cols) {
          
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
        }, suspects = suspects, suspect_cols = suspect_cols)
        
        suspects_l
        
      } else {
        lapply(fts$feature, function(x) data.table::data.table())
      }
    }, features = features, suspect_features_l = suspect_features_l, suspect_cols = suspect_cols)
    
    names(sus_col) <- names(features)
    
    features <- Map(function(fts, i) {
      fts$suspects <- i
      fts
    }, features, sus_col)
    
    nts$feature_list <- features
    engine$NonTargetAnalysisResults <- nts
    TRUE
  } else {
    message("\U26a0 No suspects found!")
    FALSE
  }
}

#' MassSpecMethod_SuspectScreening_forident S7 class
#'
#' @description Settings for performing suspect screening using the
#' \href{https://water.for-ident.org/}{FOR-IDENT} platform.
#'
#' @param addMS2 Logical length 1. When `TRUE` and MS2 data is available, the
#' fragments pattern (i.e., MS2 averaged spectra) is added to the .txt file to
#' import in FOR-IDENT platform. Note that when `addMS2` is `TRUE` the \emph{m/z}
#' values are used instead of neutral mass even is `useNeutralMass` is set to `TRUE`.
#' @param useNeutralMass Logical length 1. When `TRUE` and neutral mass is
#' available, the neutral mass of features/feature groups is used instead of the
#' \emph{m/z}.
#' @param path Character length 1 with the path to save the .txt file with the
#' list of features for identification.
#' @param name Character length 1 with the name of the file (without extension)
#' to be saved in the `path`.
#'
#' @note
#' After processing, a .txt file as defined by name and path is created with the
#' list of features or feature groups to be imported in the FOR-IDENT platform
#' (\url{https://water.for-ident.org/}). Note that log in credentials are needed.
#'
#' @return A `MassSpecMethod_SuspectScreening_forident` object.
#'
#' @export
#'
MassSpecMethod_SuspectScreening_forident <- S7::new_class(
  name = "MassSpecMethod_SuspectScreening_forident",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(addMS2 = FALSE,
                         useNeutralMass = TRUE,
                         path = getwd(),
                         name = "feature_list") {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "SuspectScreening",
        algorithm = "forident",
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
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "SuspectScreening")
    checkmate::assert_choice(self@algorithm, "forident")
    checkmate::assert_true(dir.exists(self@parameters$path))
    checkmate::assert_true(is.character(self@parameters$name))
    checkmate::assert_true(length(self@parameters$name) == 1)
    checkmate::assert_logical(self@parameters$addMS2, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_SuspectScreening_forident) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }
  
  NonTargetAnalysisResults <- engine$NonTargetAnalysisResults
  
  if (NonTargetAnalysisResults@number_features == 0) {
    warning("NonTargetAnalysisResults object is empty! Not done.")
    return(FALSE)
  }
  
  if (NonTargetAnalysisResults$has_groups) {
    
    polarities <- unique(engine$get_spectra_polarity())
    
    if (length(polarities) > 1 && x$parameters$addMS2) {
      warning(
        "Using ms2 data of feature groups from multiple polarities ", 
        "is not possible! Using features of each analysis instead."
      )
      
      out_list <- engine$get_features()
      out_list$rt <- out_list$rt / 60
      out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
    } else {
      out_list <- engine$get_groups(metadata = TRUE)
      out_list$rt <- out_list$rt / 60
      out_list$label <- out_list$group
    }
    
  } else {
    out_list <- engine$get_features()
    out_list$rt <- out_list$rt / 60
    out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
  }
  
  if (x$parameters$useNeutralMass & "mass" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mass
    
  } else if ("mz" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mz
    
  } else {
    out_list$Mass <- out_list$mass
  }
  
  if (!x$parameters$addMS2) {
    
    sink(paste0(x$parameters$path,"/", x$parameters$name, ".txt"))
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
      paste0(settings$parameters$path,"/", settings$parameters$name, ".txt")
    )
    
  } else {
    
    if (NonTargetAnalysisResults$has_groups) {
      ms2 <- engine$get_groups_ms2()
      ms2 <- split(ms2, ms2$group)
      out_list$ms2 <- lapply(out_list$group, function(x, ms2) {
        if (x %in% names(ms2)) {
          ms2[[x]]
        } else {
          NULL
        }
      }, ms2 = ms2)
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
      
      sink(paste0(x$parameters$path,"/", x$parameters$name, ".txt"))
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
      
      message("\U2713 List saved as ", paste0(x$parameters$path,"/", x$parameters$name, ".txt"))
      
    } else {
      warning("MS2 data not found! Load features and/or feature groups MS2 data.")
      return(FALSE)
    }
  }
  TRUE
}

#' MassSpecMethod_SuspectScreening_patRoon S7 class
#'
#' @description Settings for performing suspect screening using the function
#' \link[patRoon]{screenSuspects} from the patRoon R package.
#'
#' @param suspects A data.frame with suspect information. See section Suspect list format in
#' \link[patRoon]{screenSuspects} for more information.
#' @param rtWindow The retention time window (in seconds) that will be used for matching a
#' suspect (+/- feature data).
#' @param mzWindow The m/z window that will be used for matching a suspect (+/- feature data).
#' @template arg-ms-filtered
#'
#' @return A `MassSpecMethod_SuspectScreening_patRoon` object.
#' 
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' @export
#'
MassSpecMethod_SuspectScreening_patRoon <- S7::new_class(
  name = "MassSpecMethod_SuspectScreening_patRoon",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(suspects = data.table::data.table(),
                         rtWindow = 12,
                         mzWindow = 0.005,
                         filtered = FALSE) {
    
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "SuspectScreening",
        algorithm = "patRoon",
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
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "SuspectScreening")
    checkmate::assert_choice(self@algorithm, "patRoon")
    checkmate::assert_number(self@parameters$rtWindow)
    checkmate::assert_number(self@parameters$mzWindow)
    checkmate::assert_logical(self@parameters$filtered, max.len = 1)
    checkmate::assert_data_table(self@parameters$suspects)
    if (nrow(self@parameters$suspects) > 0) {
      if (!(
          all(c("name", "neutralMass") %in% colnames(self@parameters$suspects)) ||
          all(c("name", "mz") %in% colnames(self@parameters$suspects))
          )) stop("Suspects must have at least the columns name and neutralMass or name and mz!")
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_SuspectScreening_patRoon) <- function(x, engine = NULL) {
  
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
  
  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }
  
  if (nrow(x$parameters$suspects) == 0) {
    warning("Suspects data.frame is empty! Not done.")
    return(FALSE)
  }
  
  NonTargetAnalysisResults <- engine$NonTargetAnalysisResults
  
  if (!NonTargetAnalysisResults@has_groups) {
    warning("NonTargetAnalysisResults object does not have feature groups! Not done.")
    return(FALSE)
  }
  
  parameters <- x$parameters
  
  res <- patRoon::screenSuspects(
    fGroups = NonTargetAnalysisResults$features,
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
  
  NonTargetAnalysisResults$features <- res
  
  features <- NonTargetAnalysisResults$feature_list
  
  features <- lapply(features, function(x, suspect_list) {
    
    has_suspect_features <- any(suspect_list$group %in% x$group)
    
    if (has_suspect_features) {
      
      suspects_l <- lapply(seq_len(nrow(x)), function(z, suspect_list) {
        
        ft <- x[z, ]
        
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
            sus_temp$d_mz <- (sus_temp$neutralMass - ft$mass) / ft$mass * 1E6
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
        
      }, suspect_list = suspect_list)
      
      x$suspects <- suspects_l
    }
    
    x
    
  }, suspect_list = suspect_list)
  
  NonTargetAnalysisResults$feature_list <- features
  engine$NonTargetAnalysisResults <- NonTargetAnalysisResults
  message("\U2713 Suspect screening done with patRoon!")
  TRUE
}
