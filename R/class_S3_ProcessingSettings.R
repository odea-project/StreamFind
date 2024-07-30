#' **ProcessingSettings** S3 class constructor, methods and functions
#'
#' @description Creates a ProcessingSettings S3 class object. The ProcessingSettings are used in \pkg{StreamFind} for 
#' processing data in a given data class method.
#' 
#' @param engine Character of length one with the name of the engine where the processing settings are to be applied.
#' @param call Character of length one with the name of the method where the processing settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to be used.
#' @param parameters List with parameters specific for the method `call` and `algorithm`.
#' @param version Character of length one with the version of the processing settings.
#' @param software Character of length one with the name of the software or package.
#' @param developer Character of length one with the name of the developer/s.
#' @param contact Character of length one with the email of the developer.
#' @param link Character of length one with the a web link.
#' @param doi Character of length one with the DOI of algorithm.
#'
#' @return A ProcessingSettings S3 class
#'
#' @export
#'
ProcessingSettings <- function(engine = NA_character_,
                               call = NA_character_,
                               algorithm = NA_character_,
                               parameters = NULL,
                               version = NA_character_,
                               software = NA_character_,
                               developer = NA_character_,
                               contact = NA_character_,
                               link = NA_character_,
                               doi = NA_character_) {

  x <- list(
    "engine" = engine,
    "call" = call,
    "algorithm" = algorithm,
    "parameters" = parameters,
    "version" = version,
    "software" = software,
    "developer" = developer,
    "contact" = contact,
    "link" = link,
    "doi" = doi
  )

  x$engine <- as.character(x$engine)
  
  x$call <- as.character(x$call)
  
  x$algorithm <- as.character(x$algorithm)
  
  x$parameters <- as.list(x$parameters)
  
  x$version <- as.character(x$version)
  
  if (is.na(x$version)) x$version <- as.character(packageVersion("StreamFind"))
  
  x$software <- as.character(x$software)
  
  x$developer <- as.character(x$developer)
  
  x$contact <- as.character(x$contact)
  
  x$link <- as.character(x$link)
  
  x$doi <- as.character(x$doi)
  
  s3_classes <- "ProcessingSettings"
  
  if (!is.na(x$engine) && !is.na(x$call) && !is.na(x$algorithm)) {
    s3_classes <- append(s3_classes, paste0(x$engine, "Settings", "_", x$call, "_" , x$algorithm))
  }
  
  patRoon_algorithms <- c("openms", "xcms", "xcms3", "envipick", "sirius", "kpic2", "safd", "GenForm", "MetFrag")

  if (any(vapply(patRoon_algorithms, function(a) grepl(a, x$algorithm, fixed = FALSE), FALSE))) {
    s3_classes <- append(s3_classes, "patRoon")
  }
  
  x <- structure(x, class = s3_classes)

  if (validate(x)) {
    x
  } else {
    NULL
  }
}

#' @describeIn validate Validates a ProcessingSettings S3 object.
#' 
#' @param x A ProcessingSettings S3 object.
#' 
#' @export
#' 
validate.ProcessingSettings <- function(x) {
  
  valid <- FALSE
  
  if (is.list(x)) {
    
    if (all(c("engine", "call", "algorithm", "parameters") %in% names(x))) {
      
      valid <- TRUE

      if (!length(x$engine) == 1 && !all(is.character(x$engine))) {
        warning("Engine entry must be of length 1 and type character!")
        valid <- FALSE
      }
      
      if (!length(x$call) == 1 && !all(is.character(x$call))) {
        warning("Call entry must be of length 1!")
        valid <- FALSE
      }

      if (length(x$algorithm) != 1 && !all(is.character(x$algorithm))) {
        warning("Algorithm entry must be of length 1 and type character!")
        valid <- FALSE
      }

      if (!(is.list(x$parameters) || isS4(x$parameters))) {
        warning("Parameters entry must be a list or an S4 class!")
        valid <- FALSE
      }

      if (!length(x$version) == 1 && !all(is.character(x$version))) {
        warning("Version entry must be of length 1 and type character!")
        valid <- FALSE
      }

    } else {
      warning("Settings elements must be named engine, call, algorithm and parameters!")
    }
  }

  if (valid && length(is(x)) > 1) {
    NextMethod()
  } else if (valid) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
#' @noRd
#'
print.ProcessingSettings <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
    " engine       ", x$engine, "\n",
    " call         ", x$call, "\n",
    " algorithm    ", x$algorithm, "\n",
    " version      ", x$version, "\n",
    " software     ", x$software, "\n",
    " developer    ", x$developer, "\n",
    " contact      ", x$contact, "\n",
    " link         ", x$link, "\n",
    " doi          ", x$doi, "\n",
    sep = ""
  )

  if (isS4(x$parameters) || length(x$parameters) == 1) {
    if (is.list(x$parameters)) {
      if (isS4(x$parameters[[1]])) {
        cat("\n")
        print(x$parameters[[1]])
      } else {
        cat("\n")
        cat(" parameters: ", "\n")
        for (i in seq_len(length(x$parameters))) {

          if (is.data.frame(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i], " (only head rows)", "\n")
            cat("\n")
            print(head(x$parameters[[i]]), quote = FALSE)
            cat("\n")

          } else if (is.list(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i], ": ", "\n")
            for (i2 in seq_len(length(x$parameters[[i]]))) {
              cat("      - ", names(x$parameters[[i]])[i2], x$parameters[[i]][[i2]], "\n")
            }

          } else if ("function" %in% is(x$parameters[[i]])) {
            cat("  - ", names(x$parameters)[i])
            quote(x$parameters[[i]])
            cat("\n")
            
          } else {
            cat("  - ", names(x$parameters)[i], x$parameters[[i]], "\n")
          }
        }
      }
    } else {
      cat("\n")
      print(x$parameters)
    }
    
  } else {
    cat("\n")
    cat(" parameters: ")
    
    if (length(x$parameters) == 0) {
      cat("empty ", "\n")
      
    } else {
      cat("\n")
      
      for (i in seq_len(length(x$parameters))) {
        if (is.data.frame(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], " (only head rows)", "\n")
          cat("\n")
          print(head(x$parameters[[i]]), quote = FALSE)
          cat("\n")
          
        } else if (is.list(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], ": ", "\n")
          for (i2 in seq_len(length(x$parameters[[i]]))) {
            cat("      - ", names(x$parameters[[i]])[i2], x$parameters[[i]][[i2]], "\n")
          }
          
        } else if ("function" %in% is(x$parameters[[i]])) {
          cat("  - ", names(x$parameters)[i], ":\n")
          print(x$parameters[[i]])
          cat("\n")
          
        } else {
          cat("  - ", names(x$parameters)[i], x$parameters[[i]], "\n")
        }
      }
    }
  }
}

#' @noRd
as.ProcessingSettings <- function(value) {
  
  if (length(value) == 1 && is.list(value)) value <- value[[1]]
  
  must_have_elements <- c("engine", "call", "algorithm", "parameters")
  
  if (!all(must_have_elements %in% names(value))) return(NULL)

  if (!"version" %in% names(value)) value$version <- as.character(packageVersion("StreamFind"))
  
  if (is.na(value$version)) value$version <- as.character(packageVersion("StreamFind"))

  ProcessingSettings(
    value$engine,
    value$call,
    value$algorithm,
    value$parameters,
    value$version,
    value$software,
    value$developer,
    value$contact,
    value$link,
    value$doi
  )
}
