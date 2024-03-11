#' **ProcessingSettings** S3 class constructor, methods and functions
#'
#' @description
#' Creates a ProcessingSettings S3 class object. The ProcessingSettings are used
#' in \pkg{StreamFind} for processing data in a given data class method.
#'
#' @param call Character of length one with the name of the method where the 
#' processing settings are to be applied.
#' @param algorithm Character of length one with the name of the algorithm to
#' be used.
#' @param parameters List with parameters specific for the method `call` and
#' `algorithm`.
#' @param version Character of length one with the version of the processing
#' settings.
#' @param software Character of length one with the name of the software or
#' package.
#' @param developer Character of length one with the name of the developer/s.
#' @param contact Character of length one with the email of the developer.
#' @param link Character of length one with the a web link.
#' @param doi Character of length one with the DOI of algorithm.
#'
#' @details See the documentation of the method where the processing settings
#' are to be applied for more information about applicable algorithms and
#' parameters.
#'
#' @return A ProcessingSettings S3 class
#'
#' @export
#'
ProcessingSettings <- function(call = NA_character_,
                               algorithm = NA_character_,
                               parameters = NULL,
                               version = NA_character_,
                               software = NA_character_,
                               developer = NA_character_,
                               contact = NA_character_,
                               link = NA_character_,
                               doi = NA_character_) {

  x <- list(
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

  x$call <- as.character(x$call)
  
  x$algorithm <- as.character(x$algorithm)
  
  x$parameters <- as.list(x$parameters)
  
  x$version <- as.character(x$version)
  
  x$software <- as.character(x$software)
  
  x$developer <- as.character(x$developer)
  
  x$contact <- as.character(x$contact)
  
  x$link <- as.character(x$link)
  
  x$doi <- as.character(x$doi)
  
  s3_classes <- "ProcessingSettings"
  
  if (!is.na(x$algorithm)) {
    
    algo <- paste0("Settings_", x$call, "_" , x$algorithm)
    
    s3_classes <- append(s3_classes, algo)
    
    # TODO add validation for StreamFind implemented algorithm, may a list of added native algorithms
    if ("StreamFind" %in% x$software) {
      page <- "https://odea-project.github.io/StreamFind/reference/"
      x$link <- paste0(page, algo, ".html")
    }
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

#' @describeIn ProcessingSettings
#' Validates a ProcessingSettings S3 class object, returning a logical value of
#' length one.
#'
#' @param x A ProcessingSettings S3 class object.
#'
#' @export
#'
validate.ProcessingSettings <- function(x) {
  
  valid <- FALSE
  
  if (is.list(x)) {
    
    if (all(c("call", "algorithm", "parameters") %in% names(x))) {
      valid <- TRUE

      if (!length(x$call) == 1 && !all(is.character(x$algorithm))) {
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
      warning("Settings elements must be named call, algorithm and parameters!")
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

#' @describeIn ProcessingSettings
#' Prints the ProcessingSettings S3 class object in the console.
#'
#' @param ... Not used.
#'
#' @export
print.ProcessingSettings <- function(x, ...) {
  cat("\n")
  cat("", class(x)[1], "\n")
  cat(
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

#' @describeIn ProcessingSettings
#' Converts a ProcessingSettings S3 class object to a JSON string.
#'
#' @export
asJSON.ProcessingSettings <- function(x) {
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

#' @describeIn ProcessingSettings
#' Exports a ProcessingSettings S3 class object to a JSON or RDS file.
#'
#' @template arg-ms-save-format
#' @template arg-ms-save-name
#' @template arg-ms-save-path
#'
#' @export
#'
export.ProcessingSettings <- function(x,
                                      format = "json",
                                      name = "settings",
                                      path = getwd(), ...) {

  if ("ProcessingSettings" %in% class(x)) {
    if (validate(x)) {
      if (format %in% "json") {
        settings <- toJSON(
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
        write(settings, file = paste0(path, "/", name, ".json"))
      }

      if (format %in% "rds") {
        saveRDS(settings, file = paste0(path, "/", name, ".rds"))
      }
    }
  }
}

#' @describeIn ProcessingSettings
#' Converts the argument in a ProcessingSettings S3 class object.
#'
#' @param value A list to be checked and/or converted to ProcessingSettings S3
#' class.
#'
#' @export
as.ProcessingSettings <- function(value) {
  
  if (length(value) == 1 && is.list(value)) value <- value[[1]]
  
  must_have_elements <- c("call", "algorithm", "parameters")
  
  if (!all(must_have_elements %in% names(value))) return(NULL)

  if (!"version" %in% names(value)) value$version <- NA_character_

  ProcessingSettings(
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
