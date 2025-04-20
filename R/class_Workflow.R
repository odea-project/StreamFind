# MARK: Workflow
# Workflow -----
#' @title Workflow Class
#' 
#' @description The Workflow class is used to manage an ordered list of \code{\link{ProcessingStep}}
#' objects of a specific type of data.
#' 
#' @param processing_steps A list of \code{\link{ProcessingStep}} objects.
#' 
#' @slot methods (getter) A character vector of the methods of each processing step.
#' @slot overview (getter) A data frame with an overview of processing steps in the workflow.
#' 
#' @export
#' 
Workflow <- S7::new_class(
  name = "Workflow",
  package = "StreamFind",
  properties = list(
    
    # MARK: processing_steps
    # processing_steps ----
    processing_steps = S7::new_property(S7::class_list, default = list()),
    
    # MARK: methods
    # methods ----
    methods = S7::new_property(
      S7::class_character,
      getter = function(self) {
        if (length(self) == 0) return(character())
        vapply(self@processing_steps, function(x) {
          paste0(x$data_type, "Method_", x$method, "_", x$algorithm)
        }, NA_character_)
      }
    ),
    
    # MARK: overview
    # overview ----
    overview = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        if (length(self) > 0) {
          data.frame(
            index = seq_along(self@processing_steps),
            method = vapply(self@processing_steps, function(x) x$method, NA_character_),
            algorithm = vapply(self@processing_steps, function(x) x$algorithm, NA_character_),
            number_permitted = vapply(self@processing_steps, function(x) x$number_permitted, NA_real_),
            version = vapply(self@processing_steps, function(x) x$version, NA_character_),
            software = vapply(self@processing_steps, function(x) x$software, NA_character_),
            developer = vapply(self@processing_steps, function(x) x$developer, NA_character_),
            contact = vapply(self@processing_steps, function(x) x$contact, NA_character_),
            link = vapply(self@processing_steps, function(x) x$link, NA_character_),
            doi = vapply(self@processing_steps, function(x) x$doi, NA_character_)
          )
        } else {
          data.frame()
        }
      }
    )
  ),
  
  # MARK: constructor
  constructor = function(processing_steps = list()) {
    if (!(is.list(processing_steps) || is(processing_steps, "StreamFind::Workflow"))) {
      stop("processing_steps must be a list!")
    }

    if (is.list(processing_steps)) {
      processing_steps <- lapply(processing_steps, function(x) {
        if (is.list(x) && !is(x, "StreamFind::ProcessingStep")) {
          tryCatch({
            x <- as.ProcessingStep(x)
          }, error = function(e) {
            warning("Error converting to ProcessingStep object!")
            x <- NULL
          })
        }
        x
      })
    } else {
      processing_steps <- processing_steps@processing_steps
    }
    
    if (length(processing_steps) > 0) {
      w_names <- vapply(processing_steps, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(processing_steps) <- w_names
      
      data_type <- vapply(processing_steps, function(z) z$data_type, NA_character_)[1]
      possible_methods <- .get_available_methods(data_type)
      workflow_methods <- NA_character_
      for (i in names(processing_steps)) {
        
        if (!processing_steps[[i]]$method %in% possible_methods) {
          warning(
            "Method ", processing_steps[[i]]$method, " not available for the engine ", engine, "Engine!"
          )
          processing_steps[[i]] <- NULL
          next
        }
        
        ri <- processing_steps[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          processing_steps[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, processing_steps[[i]]$method)
      }
    }
    S7::new_object(S7::S7_object(), processing_steps = processing_steps)
  },
  
  # MARK: validator
  validator = function(self) {
    checkmate::assert_list(self@processing_steps)
    if (length(self@processing_steps) > 0) {
      lapply(self@processing_steps, function(x) {
        checkmate::assert_true(is(x, "StreamFind::ProcessingStep"))
      })
      
      data_type <- unique(vapply(self@processing_steps, function(x) x$data_type, NA_character_))
      if (length(data_type) > 1) {
        stop("All ProcessingStep objects must be for the same type of data!")
      }
      
      methods <- self@methods
      available_methods <- .get_available_processing_methods(data_type)
      if (!all(methods %in% available_methods)) {
        stop("All processing methods must be available for the defined type of data!")
      }
      
      permitted <- vapply(self@processing_steps, function(x) x$number_permitted, NA_real_)
      if (any(permitted == 1)) {
        unique_methods <- unique(methods[permitted == 1])
        if (length(unique_methods) != length(methods[permitted == 1])) {
          stop("All ProcessingStep objects with number_permitted == 1 must be unique!")
        }
      }
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(length, Workflow) <- function(x) {
  length(x@processing_steps)
}

#' @export
#' @noRd
S7::method(names, Workflow) <- function(x) {
  names(x@processing_steps)
}

#' @export
#' @noRd
`$.StreamFind::Workflow` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`[.StreamFind::Workflow` <- function(x, i) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@processing_steps[i]
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
`[<-.StreamFind::Workflow` <- function(x, i, value) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@processing_steps[i] <- value
    
    if (length(x$processing_steps) > 0) {
      w_names <- vapply(x$processing_steps, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(x@processing_steps) <- w_names
      
      data_type <- vapply(x@processing_steps, function(z) z$data_type, NA_character_)[1]
      possible_methods <- .get_available_methods(data_type)
      workflow_methods <- NA_character_
      for (i in names(x@processing_steps)) {
        
        if (!x@processing_steps[[i]]$method %in% possible_methods) {
          warning(
            "Method ", x@processing_steps[[i]]$method, " not available for ", data_type, " data!"
          )
          x@processing_steps[[i]] <- NULL
          next
        }
        
        ri <- x@processing_steps[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          x@processing_steps[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, x@processing_steps[[i]]$method)
      }
    }
    x
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
`[[.StreamFind::Workflow` <- function(x, i) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@processing_steps[[i]]
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
`[[<-.StreamFind::Workflow` <- function(x, i, value) {
  if (is.numeric(i) || is.logical(i) || is.character(i)) {
    x@processing_steps[[i]] <- value

    if (length(x$processing_steps) > 0) {
      w_names <- vapply(x$processing_steps, function(z) paste0(z$method, "_", z$algorithm), NA_character_)
      w_idx <- seq_along(w_names)
      w_names <- paste0(w_idx, "_", w_names)
      names(x@processing_steps) <- w_names
      
      data_type <- vapply(x@processing_steps, function(z) z$data_type, NA_character_)[1]
      possible_methods <- .get_available_methods(data_type)
      workflow_methods <- NA_character_
      for (i in names(x@processing_steps)) {
        
        if (!x@processing_steps[[i]]$method %in% possible_methods) {
          warning(
            "Method ", x@processing_steps[[i]]$method, " not available for ", data_type, " data!"
          )
          x@processing_steps[[i]] <- NULL
          next
        }
        
        ri <- x@processing_steps[[i]]$required
        if (!all(ri %in% workflow_methods)) {
          warning(
            "Required methods not present! Please include first: \n", paste(ri, collapse = "\n")
          )
          x@processing_steps[[i]] <- NULL
          next
        }
        workflow_methods <- c(workflow_methods, x@processing_steps[[i]]$method)
      }
    }
    x
  } else {
    stop("Index must be numeric, logical or character!")
  }
}

#' @export
#' @noRd
S7::method(as.list, Workflow) <- function(x, ...) {
  processing_steps <- lapply(x@processing_steps, function(s) as.list(s))
  names(processing_steps) <- names(x@processing_steps)
  processing_steps
}

#' @export
#' @noRd
S7::method(save, Workflow) <- function(x, file = "workflow.rds") {
  if (length(x) > 0) {
    format <- tools::file_ext(file)
    if (format %in% "json") {
      processing_steps <- lapply(x@processing_steps, function(s) {
        list(
          data_type = s@data_type,
          method = s@method,
          required = s@required,
          algorithm = s@algorithm,
          parameters = s@parameters,
          number_permitted = s@number_permitted,
          version = s@version,
          software = s@software,
          developer = s@developer,
          contact = s@contact,
          link = s@link,
          doi = s@doi
        )
      })
      names(processing_steps) <- names(x)
      processing_steps <- .convert_to_json(processing_steps)
      write(processing_steps, file)
    } else if (format %in% "rds") {
      saveRDS(x, file)
    } else {
      warning("Format not supported!")
    }
  } else {
    warning("No processing_steps to save!")
  }
}

#' @export
#' @noRd
S7::method(read, Workflow) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Workflow(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::Workflow")) {
      return(res)
    } else {
      warning("File is not a Workflow object!")
    }
  }
  x
}

#' @export
#' @noRd
S7::method(show, Workflow) <- function(x, ...) {
  if (length(x) > 0) {
    names_processing_steps <- vapply(x@processing_steps, function(x) x$method, "")
    algorithms <- vapply(x@processing_steps, function(x) x$algorithm, "")
    cat(
      paste0(seq_len(length(names_processing_steps)), ": ", names_processing_steps, " (", algorithms, ")"),
      sep = "\n"
    )
  } else {
    cat("empty")
  }
}

#' @export
#' @noRd
S7::method(print, Workflow) <- function(x, ...) {
  show(x)
}
