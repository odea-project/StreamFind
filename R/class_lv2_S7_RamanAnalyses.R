
#' @export
#' @noRd
RamanAnalyses <- S7::new_class("RamanAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
    
    ## __analyses -----
    analyses = S7::new_property(S7::class_list, default = list()),
    
    ## __names -----
    names = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$name, NA_character_)
    }),
    
    ## __replicates -----
    replicates = S7::new_property(S7::class_character,
      getter = function(self) vapply(self@analyses, function(x) x$replicate, NA_character_),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of replicates not conform!")
          return(self)
        }
        if (!is.character(value)) {
          warning("Replicates must be character!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$replicate <- value[i]
        self
      }
    ),
    
    ## __blanks -----
    blanks = S7::new_property(S7::class_character,
      getter = function(self) vapply(self@analyses, function(x) x$blank, NA_character_),
      setter = function(self, value) {
        if (length(value) != length(self)) {
          warning("Length of blanks not conform!")
          return(self)
        }
        if (!is.character(value)) {
          warning("Blanks must be character!")
          return(self)
        }
        if (!all(value %in% self@replicates)) {
          warning("Blank names must be in replicate names!")
          return(self)
        }
        for (i in seq_len(length(self))) self@analyses[[i]]$blank <- value[i]
        self
      }
    ),
    
    ## __types -----
    types = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$type, NA_character_)
    }),
    
    ## __files -----
    files = S7::new_property(S7::class_character, getter = function(self) {
      vapply(self@analyses, function(x) x$file, NA_character_)
    }),
    
    ## __info -----
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (length(self) > 0) {
        df <- data.table::data.table(
          "analysis" = vapply(self@analyses, function(x) x$name, ""),
          "replicate" = vapply(self@analyses, function(x) x$replicate, ""),
          "blank" = vapply(self@analyses, function(x) x$blank, ""),
          "type" = vapply(self@analyses, function(x) x$type, ""),
          "spectra" = vapply(self@analyses, function(x) nrow(x$spectra), 0)
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    }),
    
    ## __raw_spectra -----
    raw_spectra = S7::new_property(S7::class_list, getter = function(self) {
      if (length(self) > 0) {
        lapply(self@analyses, function(x) x$spectra)
      } else {
        list()
      }
    }),
    
    ## __has_processed_spectra -----
    has_processed_spectra = S7::new_property(S7::class_logical, getter = function(self) {
      if (length(self) == 0) return(FALSE)
      !is.null(self@results[["Spectra"]])
    }),
    
    ## __spectra -----
    spectra = S7::new_property(S7::class_data.frame,
      getter = function(self) {
        if (self$has_processed_spectra) {
          lapply(self@results[["spectra"]], function(x) x$spectra)
        } else {
          lapply(self@analyses, function(x) x$spectra)
        }
      },
      setter = function(self, value) {
        if (is.list(value)) {
          if (names(value) > 0) {
            analyses_names <- unname(self$names)
            replicate_names <- unique(unname(self$replicates))
            value_names <- names(value)
            if (identical(analyses_names, value_names)) {
              self$results$Spectra <- Spectra(spectra = value, is_averaged = FALSE)
            } else if (identical(replicate_names, value_names)) {
              self$results$Spectra <- Spectra(spectra = value, is_averaged = TRUE)
            } else {
              warning("Analysis/replicate names do not match! Not done.")
            }
          } else {
            warning("Spectra must be named! Not done.")
          }
        } else {
          warning("Value must be a named list object! Not done.")
        }
        self
      }
    )
  ),

  constructor = function(files = NULL) {
    analyses <- .get_RamanAnalysis_from_files(files)
    S7::new_object(Analyses(), possible_formats = c(".asc"), analyses = analyses)
  },

  validator = function(self) {
    valid <- all(
      checkmate::test_true(identical(self@possible_formats, c(".asc"))),
      if (length(self) > 0) checkmate::test_true(identical(names(self@analyses), unname(self@names)))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(names, Analyses) <- function(x) {
  vapply(self@analyses, function(x) x$name, NA_character_)
}

#' @export
#' @noRd
S7::method(add, RamanAnalyses) <- function(x, value) {
  
  if (is.character(value)) {
    if (grepl(x@possible_formats, value)) {
      value <- .get_RamanAnalysis_from_files(value)
    } else {
      warning("File/s not valid!")
      return(x)
    }
  }
  
  if (!all(vapply(value, function(a) is(a, "RamanAnalysis"), FALSE))) {
    warning("Analysis/s not valid!")
    return(x)
  }
  if (any(vapply(value, function(a) a$name %in% x@names, FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  
  analyses <- c(x@analyses, value)
  analyses <- analyses[order(names(analyses))]
  
  if (length(analyses) > length(x@analyses)) {
    warning("All results removed!")
    x@results <- list()
  }
  
  x@analyses <- analyses
  x
}

#' @export
#' @noRd
S7::method(remove, RamanAnalyses) <- function(x, value) {
  if (is.character(value)) {
    x$analyses <- x$analyses[!x$names %in% value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  } else if (is.numeric(value)) {
    x@analyses <- x@analyses[-value]
    x@analyses <- x@analyses[order(names(x@analyses))]
  }
  if (x@has_processed_spectra) {
    Spectra_names <- names(x@results$Spectra)
    if (!x@results$Spectra$is_averaged) {
      x@results$Spectra <- x@results$Spectra[x$names %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    } else {
      x@results$Spectra <- x@results$Spectra[x$replicates %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    }
  }
  x
}

#' @export
#' @noRd
S7::method(`[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[i]
  if (x@has_processed_spectra) {
    Spectra_names <- names(x@results$Spectra)
    if (!x@results$Spectra$is_averaged) {
      x@results$Spectra <- x@results$Spectra[x$names %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    } else {
      x@results$Spectra <- x@results$Spectra[x$replicates %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead.")
  return(x)
}

#' @export
#' @noRd
S7::method(`[[`, RamanAnalyses) <- function(x, i) {
  x@analyses <- x@analyses[[i]]
  if (x@has_processed_spectra) {
    Spectra_names <- names(x@results$Spectra)
    if (!x@results$Spectra$is_averaged) {
      x@results$Spectra <- x@results$Spectra[x$names %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    } else {
      x@results$Spectra <- x@results$Spectra[x$replicates %in% Spectra_names]
      x@results$Spectra <- x@results$Spectra[order(names(x@results$Spectra))]
    }
  }
  return(x)
}

#' @export
#' @noRd
S7::method(`[[<-`, RamanAnalyses) <- function(x, i, value) {
  warning("Method not implemented in RamanAnalyses! Use add or remove methods instead." )
  return(x)
}

#' @noRd
.get_RamanAnalysis_from_files <- function(files = NULL) {
  if (!is.null(files)) {
    
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
    
    possible_ms_file_formats <- ".asc"
    
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
      return(list())
    }
    
    names(replicates) <- as.character(files)
    
    names(blanks) <- as.character(files)
    
    analyses <- lapply(files, function(x) {
      
      cache <- .load_chache("parsed_raman_analyses", x)
      
      if (!is.null(cache$data)) {
        message("\U2139 Analysis loaded from cache!")
        cache$data
        
      } else {
        
        message("\U2699 Parsing ", basename(x), "...", appendLF = FALSE)
        
        ana <- rcpp_parse_asc_file(x)
        
        class_ana <- class(ana)[1]
        
        if (!class_ana %in% "RamanAnalysis") {
          message(" Not Done!")
          return(NULL)
        }
        
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
        
        if (!is.null(cache$hash)) {
          .save_cache("parsed_raman_analyses", ana, cache$hash)
          message("\U1f5ab Parsed file cached!")
        }
        
        ana
      }
    })
    
    names(analyses) <- vapply(analyses, function(x) x[["name"]], "")
    
    analyses <- analyses[order(names(analyses))]
    
    if (all(vapply(analyses, function(x) "RamanAnalysis" %in% is(x), FALSE))) {
      analyses
    } else {
      warning("Not all added files could be converted as RamanAnalysis!")
      list()
    }
  } else {
    list()
  }
}
