#' **ProjectHeaders** S7 class constructor, slots and methods
#'
#' @description Creates a ProjectHeaders S7 class object.
#'
#' @param ... Arguments to be added as headers. Note that all given arguments must be of length one. The name of the 
#' argument becomes the name of the header entry. Alternatively, a single list argument with the headers (all length 
#' one) can be given. If an argument or element name is given, it must be type character. If an argument or element 
#' date is given, it must be class `POSIXct` or `POSIXt`. If given date is character, conversion to class `POSIXct` 
#' or `POSIXt` is attempted.
#' 
#' @slot headers `List` of headers.
#' @slot names Dynamic `Character` returning the header names.
#' @slot length Dynamic `Numeric` returning the number of headers.
#' 
#' @section Methods:
#' \itemize{
#'  \item `[`, `[[`, `$`, `[<-`, `[[<-`, `$<-` are implemented to get and set the elements from the headers list.  
#'  \item `length` returns the number of headers.  
#'  \item `names` returns the header names.  
#'  \item `show` prints the headers.  
#' }
#' 
#' @return A ProjectHeaders S7 class object.
#'
#' @export
#' @noRd
ProjectHeaders <- S7::new_class("ProjectHeaders", package = "StreamFind",
  
  properties = list(
    headers = S7::new_property(class = S7::class_list, default = list())
  ),
  
  constructor = function(...) {
    x <- list(...)
    
    if (length(x) == 1) if (is.list(x[[1]])) x <- x[[1]]
    
    x_names <- names(x)
    
    if (any(duplicated(x_names))) {
      warning("ProjectHeaders must have names and not permitted duplicated names!")
      x <- x[!duplicated(x_names)]
      x_names <- names(x)
    }
    
    x <- lapply(x, function(h) {
      if (any(is.null(h))) h <- NA_character_
      if (any(is.na(h))) h <- NA_character_
      h
    })
    
    if ("date" %in% x_names) {
      x$date <- as.POSIXct(x$date)
      attr(x$date, "tzone") <- NULL
    }
    
    if (!"name" %in% x_names) x$name <- NA_character_
    if (!"author" %in% x_names) x$author <- NA_character_
    if (!"date" %in% x_names) x$date <- Sys.time()
    
    S7::new_object(S7::S7_object(), headers = x)
  },
  
  validator = function(self) {
    valid <- FALSE
    
    if (is.list(self@headers)) {
      valid <- TRUE
      
      if (any(sapply(self@headers[!names(self@headers) %in% "date"], function(x) !(is.character(x) || is.numeric(x)) || length(x) != 1))) {
        warning("All headers must be of type character or numeric and of length 1!")
        valid <- FALSE
      }
      
      if (length(unique(names(self@headers))) != length(self@headers)) {
        warning("ProjectHeaders must have names and not permitted duplicated names!")
        valid <- FALSE
      }
      
      if (!all(c("name", "author", "date") %in% names(self@headers))) {
        warning("ProjectHeaders must contain at least entries name, author, file and date!")
        valid <- FALSE
      }
      
      if ("name" %in% names(self@headers)) {
        if (!is.character(self@headers$name)) {
          warning("ProjectHeaders entry name must be character length 1!")
          valid <- FALSE
        }
      }
      
      if ("author" %in% names(self@headers)) {
        if (!is.character(self@headers$author)) {
          warning("ProjectHeaders entry author must be character length 1!")
          valid <- FALSE
        }
      }
      
      if ("date" %in% names(self@headers)) {
        if (!all(grepl("POSIXct|POSIXt", class(self@headers$date)))) {
          warning("ProjectHeaders entry date class must be POSIXct or POSIXt length 1!")
          valid <- FALSE
        }
      }
    }
    
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(length, ProjectHeaders) <- function(x) {
  length(x@headers)
}

#' @export
#' @noRd
S7::method(names, ProjectHeaders) <- function(x) {
  names(x@headers)
}

#' @export
#' @noRd
S7::method(`[`, ProjectHeaders) <- function(x, i) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.numeric(i)) {
    return(headers_list[i])
  } else if (is.character(i)) {
    return(headers_list[i])
  } else if (is.logical(i)) {
    return(headers_list[i])
  } else {
    stop("Invalid headers subset type")
  }
}

#' @export
#' @noRd
S7::method(`[<-`, ProjectHeaders) <- function(x, i, value) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.numeric(i)) {
    headers_list[i] <- value
    x@headers <- headers_list
    return(x)
  } else if (is.character(i)) {
    headers_list[i] <- value
    x@headers <- headers_list
    return(x)
  } else if (is.logical(i)) {
    headers_list[i] <- value
    x@headers <- headers_list
    return(x)
  } else {
    stop("Invalid headers setter type")
  }
}

#' @export
#' @noRd
S7::method(`[[`, ProjectHeaders) <- function(x, i) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.numeric(i)) {
    return(headers_list[[i]])
  } else if (is.character(i)) {
    return(headers_list[[i]])
  } else {
    stop("Invalid headers subset type")
  }
}

#' @export
#' @noRd
S7::method(`[[<-`, ProjectHeaders) <- function(x, i, value) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.numeric(i)) {
    headers_list[i] <- value
    x@headers <- headers_list
    return(x)
  } else if (is.character(i)) {
    headers_list[[i]] <- value
    x@headers <- headers_list
    return(x)
  } else {
    stop("Invalid headers setter type")
  }
}

#' @export
#' @noRd
S7::method(`$`, ProjectHeaders) <- function(x, i) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.character(i)) {
    return(headers_list[[i]])
  } else {
    stop("Invalid headers getter type")
  }
}

#' @export
#' @noRd
S7::method(`$<-`, ProjectHeaders) <- function(x, i, value) {
  headers_list <- x@headers
  if (missing(i)) return(headers_list)
  if (is.character(i)) {
    headers_list[[i]] <- value
    x@headers <- headers_list
    return(x)
  } else {
    stop("Invalid headers setter type")
  }
}

#' @export
#' @noRd
S7::method(as.list, ProjectHeaders) <- function(x) {
  x@headers
}

#' @export
#' @noRd
S7::method(save, ProjectHeaders) <- function(x, file = "headers.json") {
  format <- tools::file_ext(file)
  if (format %in% "json") {
    x <- .convert_to_json(as.list(x))
    write(x, file)
  } else if (format %in% "rds") {
    saveRDS(x, file)
  } else {
    warning("Invalid format!")
  }
  invisible(NULL)
}

#' @export
#' @noRd
S7::method(read, ProjectHeaders) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(ProjectHeaders(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::ProjectHeaders")) return(res)
  }
  NULL
}

#' @export
#' @noRd
S7::method(show, ProjectHeaders) <- function(x, ...) {
  names <- names(x)
  str <- c("")
  for (n in names) str <- c(str, paste(n, ": ", as.character(x[[n]]), sep = ""))
  str <- str[-1]
  cat(str, sep = "\n")
}
