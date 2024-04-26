#' **MassSpecAnalysis** S3 class constructor, methods and functions
#'
#' @description
#' Creates a *MassSpecAnalysis* S3 class object.
#'
#' @param name *mzML* or *mzXML* file name without extension.
#' @param replicate Character with length one, representing the analysis replicate group name.
#' @param blank Character with length one, representing the associated blank replicate group name.
#' @param file *mzML* or *mzXML* full file path (with extension).
#' @param format Character with length one. One of *mzML* or *mzXML*.
#' @param type Character with length one defining the type of analysis.
#' @param spectra_number Integer with the number of spectra in the file.
#' @param spectra_headers data.table run information for each spectrum.
#' @param spectra data.table with the raw spectra (only present if loaded).
#' @param chromatograms_number Integer with the number of chromatograms in the file.
#' @param chromatograms_headers data.table headers information for each chromatogram.
#' @param chromatograms data.table with the raw chromatograms (only present if loaded).
#' @param features_eic list with the feature extracted ions chromatograms (EICs) from binning of spectra.
#' @param features data.table with the features from data processing.
#' @param metadata List with flexible storage for experimental metadata (e.g., concentration, location, etc.).
#'
#' @return An *MassSpecAnalysis* S3 class object.
#'
#' @export
#'
MassSpecAnalysis <- function(name = NA_character_,
                             replicate = NA_character_,
                             blank = NA_character_,
                             file = NA_character_,
                             format = NA_character_,
                             type = NA_character_,
                             spectra_number = NA_integer_,
                             spectra_mode = NA_character_,
                             spectra_levels = NA_integer_,
                             spectra_headers = data.table(),
                             spectra = data.table(),
                             chromatograms_number = NA_integer_,
                             chromatograms_headers = data.table(),
                             chromatograms = data.table(),
                             features_eic = list(),
                             features = data.table(),
                             metadata = list()) {
  
  x <- Analysis(name, replicate, blank)

  if (is.list(features)) features <- as.data.table(features)
  
  if (is.data.frame(features)) {
    if ("ms1" %in% colnames(features)) {
      features$ms1 <- lapply(features$ms1, function(z) {
        if (!is.null(z)) z <- as.data.table(z)
        z
      })
    }

    if ("ms2" %in% colnames(features)) {
      features$ms2 <- lapply(features$ms2, function(z) {
        if (!is.null(z)) z <- as.data.table(z)
        z
      })
    }
    
    if ("suspects" %in% colnames(features)) {
      features$suspects <- lapply(features$suspects, function(z) {
        if (!is.null(z)) z <- as.data.table(z)
        z
      })
    }
  }
  
  if (is.list(features_eic)) {
    if (length(features_eic) > 0) {
      features_eic <- lapply(features_eic, as.data.table)
    }
  }

  x <- c(x, list(
    "file" = as.character(file),
    "format" = as.character(format),
    "type" = as.character(type),
    "spectra_number" = as.integer(spectra_number),
    "spectra_mode" = as.character(spectra_mode),
    "spectra_levels" = as.integer(spectra_levels),
    "spectra_headers" = as.data.table(spectra_headers),
    "spectra" = as.data.table(spectra),
    "chromatograms_number" = as.integer(chromatograms_number),
    "chromatograms_headers" = as.data.table(chromatograms_headers),
    "chromatograms" = as.data.table(chromatograms),
    "features_eic" = features_eic,
    "features" = features,
    "metadata" = metadata
  ))

  if (validate.MassSpecAnalysis(x)) {
    
    x <- structure(x, class = c("MassSpecAnalysis", "Analysis"))
    
    x
    
  } else {
    NULL
  }
}

#' @describeIn MassSpecAnalysis
#' S3 method to validate a *MassSpecAnalysis* S3 class object, returning a logical value of length one.
#'
#' @param x A *MassSpecAnalysis* S3 class object.
#'
#' @export
#'
validate.MassSpecAnalysis <- function(x = NULL) {
  
  valid <- validate.Analysis(x)

  if (valid) {
    name <- x$name
    
    if (length(x$file) != 1 || !is.character(x$file)) {
      warning("Analysis file path entry not conform!")
      valid <- FALSE
    } else if (!is.na(x$file) && !file.exists(x$file)) {
      warning(paste0(x$file, " does not exist!"))
      valid <- FALSE
    }
    
    if (length(x$format) != 1) {
      warning("Analysis format not conform!")
      valid <- FALSE
    } else if (!(x$format %in% c("mzML", "mzXML"))) {
      warning("Analysis format must be 'mzML' ot 'mzXML'!")
      valid <- FALSE
    }

    if (length(x$type) != 1) {
      warning("Analysis type entry not conform!")
      valid <- FALSE
      
    } else if (!(x$type %in% c("MS", "IM-MS", "MS/MS-DDA", "MS/MS-DIA", "MS/MS-AllIons", "IM-MS/MS-DDA", "IM-MS/MS-DIA", "IM-MS/MS-AllIons", "SRM"))) {
      warning("Analysis type must be 'MS', 'IM-MS', 'MS/MS-DDA', 'MS/MS-DIA', 'MS/MS-AllIons', 'IM-MS/MS-DDA', 'IM-MS/MS-DIA', 'IM-MS/MS-AllIons', or 'SRM'!")
      valid <- FALSE
    }

    if (!is.integer(x$spectra_number) &&
        length(x$spectra_number) != 1) {
      warning("Analysis spectra_numebr entry not conform!")
      valid <- FALSE
    }

    if (!is.integer(x$chromatograms_number) &&
        length(x$chromatograms_number) != 1) {
      warning("Analysis chromatograms_number entry not conform!")
      valid <- FALSE
    }
    
    if (!is.data.frame(x$spectra_headers)) {
      warning("Analysis spectra_headers entry not conform!")
      valid <- FALSE
    }
    
    if (!is.data.frame(x$chromatograms_headers)) {
      warning("Analysis chromatograms_headers entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$spectra)) {
      warning("Analysis spectra entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$chromatograms)) {
      warning("Analysis chromatograms entry not conform!")
      valid <- FALSE
    }

    if (!is.list(x$features_eic)) {
      warning("Analysis features_eic entry not conform!")
      valid <- FALSE
    }

    if (!is.data.frame(x$features)) {
      warning("Analysis features entry not conform!")
      valid <- FALSE
    }

    if (!is.list(x$metadata)) {
      warning("Analysis metadata entry not conform!")
      valid <- FALSE
    }
    
    if (!valid) warning("Issue/s found with analysis ", x$name, "!")
  }

  valid
}

#' @describeIn MassSpecAnalysis
#' S3 method to print the *MassSpecAnalysis* S3 class object in the console.
#'
#' @param ... Not used.
#'
#' @export
print.MassSpecAnalysis <- function(x, ...) {
  cat("\n")
  cat(
    " ", class(x), "\n"
  )
  cat(
    "  name              ", x$name, "\n",
    "  replicate         ", x$replicate, "\n",
    "  blank             ", x$blank, "\n",
    "  format            ", x$format, "\n",
    "  type              ", x$type, "\n",
    "  spectra number    ", x$spectra_number, "\n",
    "  spectra mode      ", paste(unique(x$spectra_headers$mode), collapse = " "), "\n",
    "  spectra levels    ", paste(unique(x$spectra_headers$level), collapse = " "), "\n",
    "  chromatograms     ", x$chromatograms_number, "\n",
    "  has ion mobility  ", any(x$spectra_headers$drift > 0), "\n",
    "  polarity          ", paste(unique(x$spectra_headers$polarity), collapse = "; "), "\n",
    sep = ""
  )
  cat("\n")
}

#' @describeIn MassSpecAnalysis
#' S3 method to converts a *MassSpecAnalysis* S3 class object into a JSON string.
#'
#' @export
asJSON.MassSpecAnalysis <- function(x) {
  toJSON(
    x,
    dataframe = "columns",
    Date = "ISO8601",
    POSIXt = "string",
    factor = "string",
    complex = "string",
    null = "null",
    na = "null",
    auto_unbox = FALSE,
    digits = 8,
    pretty = TRUE,
    force = TRUE
  )
}

#' @describeIn MassSpecAnalysis
#' S3 method to convert the argument files in a *MassSpecAnalysis* S3 class object.
#'
#' @param value A list to be checked and/or converted to *MassSpecAnalysis* S3 class.
#'
#' @export
as.MassSpecAnalysis <- function(value) {
  if (length(value) == 1 & is.list(value)) value <- value[[1]]
  do.call(MassSpecAnalysis, value)
}

#' @describeIn MassSpecAnalysis
#' Parses information from *mzML* or *mzXML* file/s and returns a list with
#' *MassSpecAnalysis* S3 class object/s. On error, returns \code{NULL}.
#'
#' @param files A character vector with *mzML* or *mzXML* full file path/s.
#' Alternatively, a data.frame with the column/s file, replicate and blank
#' with the full file path/s, the replicate group name/s (string) and the
#' associated blank replicate group name (string).
#' @template arg-runParallel
#'
#' @export
parse_MassSpecAnalysis <- function(files = NULL, runParallel = FALSE) {

  if (is.data.frame(files)) {
    
    if ("file" %in% colnames(files)) {
      
      if ("replicate" %in% colnames(files)) {
        replicates <- as.character(files$replicate)
      } else {
        replicates <- rep(NA_character_, nrow(files))
      }
      
      if ("blank" %in% colnames(files)) {
        blanks <- as.character(files$blank)
      } else {
        blanks <- rep(NA_character_, nrow(files))
      }
      
      files <- files$file
      
    } else {
      files <- ""
    }

  } else {
    replicates <- rep(NA_character_, length(files))
    blanks <- rep(NA_character_, length(files))
  }

  possible_ms_file_formats <- ".mzML|.mzXML"

  valid_files <- vapply(files,
    FUN.VALUE = FALSE,
    function(x, possible_ms_file_formats) {
      if (!file.exists(x)) {
        return(FALSE)
      }
      if (FALSE %in% grepl(possible_ms_file_formats, x)) {
        return(FALSE)
      }
      TRUE
    }, possible_ms_file_formats = possible_ms_file_formats
  )

  if (!all(valid_files)) {
    warning("File/s not valid!")
    return(NULL)
  }
  
  names(replicates) <- as.character(files)
  
  names(blanks) <- as.character(files)
  
  analyses <- lapply(files, function(x) {
    
    cached <- FALSE
    
    if (.caches_data()) {
      hash <- patRoon::makeHash(x)
      ana <- patRoon::loadCacheData("parsed_ms_analyses", hash)
      
      if (!is.null(ana)) {
        if (validate.MassSpecAnalysis(ana)) {
          cached <- TRUE
        } else {
          ana <- NULL
        }
      }
      
    } else {
      hash <- NULL
      ana <- NULL
    }
    
    if (!cached) {
      message("\U2699 Parsing ", x, " MS file...", appendLF = FALSE)
      
      ana <- rcpp_parse_ms_analysis_v2(x)
      
      if (ana$spectra_number > 0) {
        int_polarity <- rep(0, nrow(ana$spectra_headers))
        int_polarity[ana$spectra_headers$polarity == "positive"] <- 1
        int_polarity[ana$spectra_headers$polarity == "negative"] <- -1
        ana$spectra_headers$polarity <- int_polarity
        
        int_mode <- rep(0, nrow(ana$spectra_headers))
        int_mode[ana$spectra_headers$mode == "profile"] <- 1
        int_mode[ana$spectra_headers$mode == "centroid"] <- 2
        ana$spectra_headers$mode <- int_mode
      }
      
      class_ana <- class(ana)[1]
      
      if (!class_ana %in% "MassSpecAnalysis") return(NULL)
      
      message(" Done!")
      
      rpl <- replicates[x]
      
      if (is.na(rpl)) {
        rpl <- ana$name
        rpl <- sub("-[^-]+$", "", rpl)
      }
      
      ana$replicate <- rpl
      
      blk <- blanks[x]
      
      if (!is.na(blk)) ana$blank <- blk
      
      ana$blank <- blk
      
      if (!is.null(hash)) {
        patRoon::saveCacheData("parsed_ms_analyses", ana, hash)
        message("\U1f5ab Parsed MS files cached!")
      }
    } else {
      message("\U2139 Analysis loaded from cache!")
    }
    
    ana
  })
  
  names(analyses) <- vapply(analyses, function(x) x$name, "")
  
  analyses <- analyses[order(names(analyses))]

  # cached_analyses <- FALSE
  # 
  # if (.caches_data()) {
  #   hash <- patRoon::makeHash(files)
  #   analyses <- patRoon::loadCacheData("parsed_ms_analyses", hash)
  # 
  #   if (!is.null(analyses)) {
  #     if (all(vapply(analyses, validate.MassSpecAnalysis, FALSE))) {
  #       cached_analyses <- TRUE
  #     } else {
  #       analyses <- NULL
  #     }
  #   }
  # 
  # } else {
  #   hash <- NULL
  #   analyses <- NULL
  # }
  # 
  # if (runParallel & length(files) > 1 & !cached_analyses) {
  #   workers <- parallel::detectCores() - 1
  #   if (length(files) < workers) workers <- length(files)
  #   par_type <- "PSOCK"
  #   if (parallelly::supportsMulticore()) par_type <- "FORK"
  #   cl <- parallel::makeCluster(workers, type = par_type)
  #   doParallel::registerDoParallel(cl)
  # } else {
  #   registerDoSEQ()
  # }
  # 
  # if (!cached_analyses) {
  #   message("\U2699 Parsing ", length(files),  " MS file/s...",
  #     appendLF = FALSE
  #   )
  # 
  #   i <- NULL
  # 
  #   vars <- c("rcpp_parse_ms_analysis")
  # 
  #   analyses <- foreach(i = files,
  #     .packages = "StreamFind",
  #     .export = vars
  #   ) %dopar% { rcpp_parse_ms_analysis(i) }
  #   
  #   class_analyses <- vapply(analyses, function(x) class(x)[1], "")
  # 
  #   if (!all(class_analyses %in% "MassSpecAnalysis")) return(NULL)
  # 
  #   message(" Done!")
  #   
  #   if (runParallel & length(files) > 1) parallel::stopCluster(cl)
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   if (!is.null(analyses)) {
  #     if (all(is.na(replicates))) {
  #       replicates <- vapply(analyses, function(x) x$name, "")
  #       # replicates <- gsub("-", "_", replicates)
  #       replicates <- sub("-[^-]+$", "", replicates)
  #     }
  #     
  #     analyses <- Map(
  #       function(x, y) {
  #         x$replicate <- y
  #         x
  #       },
  #       analyses, replicates
  #     )
  #     
  #     if (!is.null(blanks) & length(blanks) == length(analyses)) {
  #       if (all(blanks %in% replicates)) {
  #         analyses <- Map(
  #           function(x, y) {
  #             x$blank <- y
  #             x
  #           },
  #           analyses, blanks
  #         )
  #       }
  #     }
  #     
  #     names(analyses) <- vapply(analyses, function(x) x$name, "")
  #     
  #     analyses <- analyses[order(names(analyses))]
  #   }
  # 
  #   if (!is.null(hash)) {
  #     patRoon::saveCacheData("parsed_ms_analyses", analyses, hash)
  #     message("\U1f5ab Parsed MS files/s cached!")
  #   }
  # 
  # } else {
  #   message("\U2139 Analyses loaded from cache!")
  # }

  analyses
}
