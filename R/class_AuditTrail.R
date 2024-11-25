#' **AuditTrail** S7 class constructor, slots and methods
#'
#' @description Creates an AuditTrail S7 class object.
#'
#' @param engine_type A character with the engine type.
#' 
#' @slot entries `List` of entries with the audit trail information.
#' @slot length Dynamic `Numeric` returning the number of headers.
#' 
#' @section Methods:
#' \itemize{
#'  \item `$` retrieves the entries.  
#'  \item `add` adds an entry to the audit trail.  
#'  \item `length` returns the number of headers.  
#'  \item `show` prints the headers.  
#' }
#' 
#' @return A AuditTrail S7 class object.
#'
#' @export
AuditTrail <- S7::new_class("AuditTrail", package = "StreamFind",
  
  properties = list(
    
    entries = S7::new_property(class = S7::class_list, default = list()),
    
    length = S7::new_property(class = S7::class_numeric, getter = function(self) length(self@entries), default = 0)
  ),
  
  constructor = function(engine_type = NA_character_) {
    S7::new_object(S7::S7_object(), entries = list())
  },
  
  validator = function(self) {
    NULL
  }
)

#' @export
#' @noRd
S7::method(length, AuditTrail) <- function(x) {
  length(x@entries)
}

#' @export
#' @noRd
S7::method(`$`, AuditTrail) <- function(x, i) {
  entries_list <- x@entries
  if (missing(i)) return(entries_list)
  if (is.character(i)) {
    return(entries_list[[i]])
  } else {
    stop("Invalid entries getter type")
  }
}

#' @export
#' @noRd
S7::method(add, AuditTrail) <- function(x, value) {
  
  time_stamp <- Sys.time()
  
  value_classes <- class(value)
  
  if (length(value_classes) > 1) {
    parent_class <- class(value)[2]
    value_class <- class(value)[1]
  } else {
    parent_class <- NA_character_
  }
  
  value_str <- capture.output(show(value))
  value_str <- value_str[!value_str %in% ""]
  value_str <- value_str[!grepl("<char>|<num>", value_str)]
  value_str <- paste(value_str, collapse = "\n")

  entry <- list(
    "time_stamp" = time_stamp,
    "class" = value_class,
    "parent" = parent_class,
    value = value_str
  )

  entries_list <- x@entries
  entries_list[[as.character(time_stamp)]] <- entry
  x@entries <- entries_list
  x
}

#' @export
#' @noRd
S7::method(`$<-`, AuditTrail) <- function(x, i, value) {
  entries_list <- x@entries
  if (missing(i)) return(entries_list)
  if (is.character(i)) {
    entries_list[[i]] <- value
    x@entries <- entries_list
    return(x)
  } else {
    stop("Invalid entries setter type")
  }
}

#' @export
#' @noRd
S7::method(as.list, AuditTrail) <- function(x) {
  x@entries
}

#' @export
#' @noRd
S7::method(as.data.frame, AuditTrail) <- function(x) {
  
  if (length(x) == 0) {
    return(data.table::data.table())
  }
  
  dt_list <- lapply(x, function(z) {
    data.table::data.table(
      time_stamp = z$time_stamp,
      class = z$class,
      parent = z$parent,
      value = z$value
    )
  })
  
  data.table::rbindlist(dt_list, fill = TRUE)
}

#' @export
#' @noRd
S7::method(save, AuditTrail) <- function(x, file = "entries.json") {
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
S7::method(show, AuditTrail) <- function(x, ...) {
  names <- names(x@entries)
  for (n in names) {
    cat(n, "\n")
    cat("Class:  ", x@entries[[n]]$class, "\n")
    cat("Parent: ", x@entries[[n]]$parent, "\n")
    cat("Value:   ")
    if (is.na(x@entries[[n]]$value) || "NA" %in% x@entries[[n]]$value) {
      cat("empty")
    } else if ("empty" %in% x@entries[[n]]$value) {
      cat("empty")
    } else {
      cat("\n")
      cat(x@entries[[n]]$value)
    }
    cat("\n")
    cat("\n")
  }
}
