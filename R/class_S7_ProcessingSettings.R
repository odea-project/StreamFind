
#' @noRd
run <- S7::new_generic("run", "x")

#' @noRd
export <- S7::new_generic("export", "x")

#' @noRd
show <- S7::new_generic("show", "x")

#' @noRd
read <- S7::new_generic("read", "x")

#' @noRd
ProcessingSettings_S7 <- S7::new_class("ProcessingSettings_S7",
  properties = list(
    engine = S7::new_property(S7::class_character, default = NA_character_),
    call = S7::new_property(S7::class_character, default = NA_character_),
    algorithm = S7::new_property(S7::class_character, default = NA_character_),
    parameters = S7::new_property(S7::class_list, default = list()),
    version = S7::new_property(S7::class_character, default = NA_character_),
    software = S7::new_property(S7::class_character, default = NA_character_),
    developer = S7::new_property(S7::class_character, default = NA_character_),
    contact = S7::new_property(S7::class_character, default = NA_character_),
    link = S7::new_property(S7::class_character, default = NA_character_),
    doi = S7::new_property(S7::class_character, default = NA_character_)
  ),
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_character(self@call, len = 1),
      checkmate::test_character(self@algorithm, len = 1),
      checkmate::test_list(self@parameters),
      checkmate::test_character(self@version, len = 1),
      checkmate::test_character(self@software, len = 1),
      checkmate::test_character(self@developer, len = 1),
      checkmate::test_character(self@contact, len = 1),
      checkmate::test_character(self@link, len = 1),
      checkmate::test_character(self@doi, len = 1)
    )
    if (!valid) return(FALSE)
  }
)

#' @noRd
S7::method(export, ProcessingSettings_S7) <- function(x, format = "json", name = "settings", path = getwd()) {

  if (format %in% "json") {
    js_out <- jsonlite::toJSON(
      list(
        engine = x@engine,
        call = x@call,
        algorithm = x@algorithm,
        parameters = x@parameters,
        version = x@version,
        software = x@software,
        developer = x@developer,
        contact = x@contact,
        link = x@link,
        doi = x@doi
      ),
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
    
    write(js_out, file = paste0(path, "/", name, ".json"))
  }
  
  if (format %in% "rds") saveRDS(x, file = paste0(path, "/", name, ".rds"))
}

#' @export
#' 
S7::method(read, ProcessingSettings_S7) <- function(x, file) {
  
  res <- NULL
  
  if (grepl(".json", file)) {
    x <- jsonlite::fromJSON(file)
    
    res <- ProcessingSettings_S7(
      engine = as.character(x$engine),
      call = as.character(x$call),
      algorithm = as.character(x$algorithm),
      parameters = x$parameters,
      version = as.character(x$version),
      software = as.character(x$software),
      developer = as.character(x$developer),
      contact = as.character(x$contact),
      link = as.character(x$link),
      doi = as.character(x$doi)
    )
    
    if (!is.na(res@algorithm)) {
      child_class <- paste0(res@engine, "Settings_", res@call, "_", res@algorithm, "_S7")
      res <- do.call(child_class, res@parameters)
    }
  }
  
  if (grepl(".rds", file)) {
    res <- readRDS(file)
  }
  
  res
}

#' @noRd
S7::method(show, ProcessingSettings_S7) <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
    " engine       ", x@engine, "\n",
    " call         ", x@call, "\n",
    " algorithm    ", x@algorithm, "\n",
    " version      ", x@version, "\n",
    " software     ", x@software, "\n",
    " developer    ", x@developer, "\n",
    " contact      ", x@contact, "\n",
    " link         ", x@link, "\n",
    " doi          ", x@doi, "\n",
    sep = ""
  )
  
  if (isS4(x@parameters) || length(x@parameters) == 1) {
    if (is.list(x@parameters)) {
      if (isS4(x@parameters[[1]])) {
        cat("\n")
        print(x@parameters[[1]])
      } else {
        cat("\n")
        cat(" parameters: ", "\n")
        for (i in seq_len(length(x@parameters))) {
          
          if (is.data.frame(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i], " (only head rows)", "\n")
            cat("\n")
            print(head(x@parameters[[i]]), quote = FALSE)
            cat("\n")
            
          } else if (is.list(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i], ": ", "\n")
            for (i2 in seq_len(length(x@parameters[[i]]))) {
              cat("      - ", names(x@parameters[[i]])[i2], x@parameters[[i]][[i2]], "\n")
            }
            
          } else if ("function" %in% is(x@parameters[[i]])) {
            cat("  - ", names(x@parameters)[i])
            quote(x@parameters[[i]])
            cat("\n")
            
          } else {
            cat("  - ", names(x@parameters)[i], x@parameters[[i]], "\n")
          }
        }
      }
    } else {
      cat("\n")
      print(x@parameters)
    }
    
  } else {
    cat("\n")
    cat(" parameters: ")
    
    if (length(x@parameters) == 0) {
      cat("empty ", "\n")
      
    } else {
      cat("\n")
      
      for (i in seq_len(length(x@parameters))) {
        if (is.data.frame(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], " (only head rows)", "\n")
          cat("\n")
          print(head(x@parameters[[i]]), quote = FALSE)
          cat("\n")
          
        } else if (is.list(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], ": ", "\n")
          for (i2 in seq_len(length(x@parameters[[i]]))) {
            cat("      - ", names(x@parameters[[i]])[i2], x@parameters[[i]][[i2]], "\n")
          }
          
        } else if ("function" %in% is(x@parameters[[i]])) {
          cat("  - ", names(x@parameters)[i], ":\n")
          print(x@parameters[[i]])
          cat("\n")
          
        } else {
          cat("  - ", names(x@parameters)[i], x@parameters[[i]], "\n")
        }
      }
    }
  }
}











#' @noRd
MassSpecSettings_BinSpectra_StreamFind_S7 <- S7::new_class("MassSpecSettings_BinSpectra_StreamFind_S7",
                                                           parent = ProcessingSettings_S7,
                                                           constructor = function(unitsVal = NULL, unitsNumber = NULL, bins = NULL, refBinAnalysis = NULL) {
                                                             parameters <- list(unitsVal = unitsVal, unitsNumber = unitsNumber, bins = bins, refBinAnalysis = refBinAnalysis)
                                                             version <- as.character(packageVersion("StreamFind"))
                                                             software <- "StreamFind"
                                                             developer <- "Ricardo Cunha"
                                                             contact <- "cunha@iuta.de"
                                                             link <- "https://odea-project.github.io/StreamFind"
                                                             doi <- NA_character_
                                                             S7::new_object(ProcessingSettings_S7("MassSpec", "BinSpectra", "StreamFind", parameters, version, software, developer, contact, link, doi))
                                                           },
                                                           validator = function(self) {
                                                             valid <- all(
                                                               checkmate::test_choice(self@engine, "MassSpec"),
                                                               checkmate::test_choice(self@call, "BinSpectra"),
                                                               checkmate::test_choice(self@algorithm, "StreamFind"),
                                                               checkmate::test_character(self@parameters$unitsVal, len = 1, null.ok = TRUE),
                                                               checkmate::test_integer(self@parameters$unitsNumber, len = 1, null.ok = TRUE),
                                                               checkmate::test_list(self@parameters$bins, null.ok = TRUE),
                                                               checkmate::test_integer(self@parameters$refBinAnalysis, len = 1, null.ok = TRUE)
                                                             )
                                                             if (!valid) {
                                                               return(FALSE)
                                                             }
                                                           }
)

#' @noRd
S7::method(run, MassSpecSettings_BinSpectra_StreamFind_S7) <- function(x, self = NULL, private = NULL) {
  cat("Binning Spectra\n")
}











