# MARK: AuditTrailEntry
# AuditTrailEntry -----
#' @title Generic Entry for Audit Trail
#' 
#' @description The [StreamFind::AuditTrailEntry] class is used as an element in the
#' [StreamFind::AuditTrail] list.
#' 
#' @param time_stamp A POSIXct timestamp of the entry.
#' @param value_class A character string representing the class of the entry.
#' @param value_parent A character string representing the parent class of the entry.
#' @param value A character string representing the value of the entry.
#' 
#' @export
#'
AuditTrailEntry <- function(time_stamp, value_class, value_parent, value) {
  x <- structure(
    list(
      time_stamp = time_stamp, 
      value_class = value_class, 
      value_parent = value_parent, 
      value = value
    ),
    class = "AuditTrailEntry"
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid AuditTrailEntry object!")
  }
}

#' @describeIn AuditTrailEntry Validate the AuditTrailEntry object, returning NULL if valid.
#' @param x A `AuditTrailEntry` object.
#' @export
#' 
validate_object.AuditTrailEntry <- function(x) {
  checkmate::assert_class(x, "AuditTrailEntry")
  checkmate::assertNames(
    names(x), must.include = c("time_stamp", "value_class", "value_parent", "value")
  )
  checkmate::assert_posixct(x$time_stamp)
  checkmate::assert_character(x$value_class)
  checkmate::assert_character(x$value_parent)
  checkmate::assert_character(x$value)
  NULL
}

# MARK: AuditTrail
# AuditTrail -----
#' @title Audit Trail Register
#' 
#' @description The [StreamFind::AuditTrail] class is a list of [StreamFind::AuditTrailEntry] class
#' objects.
#' 
#' @export
#' 
AuditTrail <- function(entries = list()) {
  if (is.list(entries)) {
    if (length(entries) > 0) {
      entry_names <- vapply(entries, function(x) {
        if ("AuditTrailEntry" %in% class(x)) {
          if (is.null(validate_object(x))) {
            as.character(x$time_stamp)
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      }, NA_character_)
      entries <- entries[!is.na(entry_names)]
      entry_names <- entry_names[!is.na(entry_names)]
      names(entries) <- entry_names
    }
  } else {
    stop("Entries must be a list of AuditTrailEntry objects.")
  }
  x <- structure(entries, class = "AuditTrail")
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid AuditTrail object!")
  }
}

#' @describeIn AuditTrail Validate the AuditTrail object, returning NULL if valid.
#' @param x A `AuditTrail` object.
#' 
validate_object.AuditTrail <- function(x) {
  checkmate::assert_list(x)
  if (length(x) > 0) {
    for (i in seq_along(x)) {
      if (!is.null(validate_object(x[[i]]))) {
        stop(sprintf("Element %d is not a valid AuditTrailEntry object.", i))
      }
    }
  }
  NULL
}

#' @describeIn AuditTrail Add an entry to the `AuditTrail` list,
#' returning the updated `AuditTrail`.
#' @param x An `AuditTrail` object.
#' @param value An object to be added to the `AuditTrail` as an entry.
#' 
#' @export
#' 
add.AuditTrail <- function(x, value) {
  time_stamp <- as.POSIXct(Sys.time())
  value_classes <- class(value)
  value_class <- class(value)[1]
  if (length(value_classes) > 1) {
    value_parent <- class(value)[2]
  } else {
    value_parent <- NA_character_
  }
  value_str <- capture.output(show(value))
  value_str <- value_str[!value_str %in% ""]
  value_str <- value_str[!grepl("<char>|<num>", value_str)]
  value_str <- paste(value_str, collapse = "\n")
  entry <- AuditTrailEntry(
    time_stamp = time_stamp,
    value_class = value_class,
    value_parent = value_parent,
    value = value_str
  )
  x[[as.character(time_stamp)]] <- entry
  x
}

#' @describeIn AuditTrail Convert the `AuditTrail` object to a data frame.
#' @method as.data.table AuditTrail
#' @param x An `AuditTrail` object.
#' 
#' @export
#' 
as.data.table.AuditTrail <- function(x, ...) {
  if (length(x) == 0) return(data.table::data.table())
  dt_list <- lapply(x, function(z) {
    data.table::data.table(
      time_stamp = z$time_stamp,
      class = z$value_class,
      parent = z$value_parent,
      value = z$value
    )
  })
  data.table::rbindlist(dt_list, fill = TRUE)
}

#' @describeIn AuditTrail Save the `AuditTrail` object to a file.
#' @param x An `AuditTrail` object.
#' @param file A character string representing the file path where the `AuditTrail` should be saved.
#' The file format can be either JSON or RDS.
#' 
#' @export
#' 
save.AuditTrail <- function(x, file = "entries.json") {
  format <- tools::file_ext(file)
  if (format %in% "json") {
    x <- .convert_to_json(x)
    write(x, file)
  } else if (format %in% "rds") {
    saveRDS(x, file)
  } else {
    warning("Invalid format!")
  }
  invisible(NULL)
}

#' @describeIn AuditTrail Show the `AuditTrail` entries in a human-readable format.
#' @param x An `AuditTrail` object.
#' 
#' @export
#' 
show.AuditTrail <- function(x, ...) {
  names <- names(x)
  for (n in names) {
    cat(n, "\n")
    cat("Class:  ", x[[n]]$value_class, "\n")
    cat("Parent: ", x[[n]]$value_parent, "\n")
    cat("Value:   ")
    if (is.na(x[[n]]$value) || "NA" %in% x[[n]]$value) {
      cat("empty")
    } else if ("empty" %in% x[[n]]$value) {
      cat("empty")
    } else {
      cat("\n")
      cat(x[[n]]$value)
    }
    cat("\n")
    cat("\n")
  }
}
