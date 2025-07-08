# MARK: AuditTrailEntry
# AuditTrailEntry -----
#' @title Generic Entry for Audit Trail
#' 
#' @description The [StreamFind::AuditTrailEntry] class is used as an element in the
#' [StreamFind::AuditTrail] entries.
#' 
#' @param time_stamp A POSIXct timestamp of the entry.
#' @param value_class A character string representing the class of the entry.
#' @param value_parent A character string representing the parent class of the entry.
#' @param value A character string representing the value of the entry.
#' 
#' @export
#'
AuditTrailEntry <- S7::new_class(
  name = "AuditTrailEntry",
  package = "StreamFind",
  properties = list(
    time_stamp = S7::new_property(S7::class_POSIXct, default = as.POSIXct(Sys.time())),
    value_class = S7::new_property(S7::class_character, default = NA_character_),
    value_parent = S7::new_property(S7::class_character, default = NA_character_),
    value = S7::new_property(S7::class_character, default = NA_character_)
  ),
  constructor = function(time_stamp, value_class, value_parent, value) {
    S7::new_object(
      S7::S7_object(),
      time_stamp = as.POSIXct(time_stamp),
      value_class = value_class,
      value_parent = value_parent,
      value = value
    )
  },
  validator = function(self) {
    checkmate::assert_posixct(self@time_stamp)
    checkmate::assert_character(self@value_class)
    checkmate::assert_character(self@value_parent)
    checkmate::assert_character(self@value)
    NULL
  }
)

#' @export
#' @noRd
`$.AuditTrailEntry` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.AuditTrailEntry` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

# MARK: AuditTrail
# AuditTrail -----
#' @title Audit Trail Register
#' 
#' @description The [StreamFind::AuditTrail] class is used to store a list of entries, where each
#' entry is an instance of the [StreamFind::AuditTrailEntry] class.
#' 
#' @slot entries A list of entries, where each entry is an instance of the
#' [StreamFind::AuditTrailEntry] class.
#' 
#' @export
#' 
AuditTrail <- S7::new_class(
  name = "AuditTrail",
  package = "StreamFind",
  properties = list(
    entries = S7::new_property(class = S7::class_list, default = list())
  ),
  constructor = function() {
    S7::new_object(S7::S7_object(), entries = list())
  },
  validator = function(self) {
    checkmate::assert_list(self@entries)
    if (length(self@entries) > 0) {
      checkmate::assert_list(self@entries, types = c("StreamFind::AuditTrailEntry", "S7_object"))
    }
    NULL
  }
)

#' @export
#' @noRd
length.AuditTrail <- function(x) {
  length(x@entries)
}

#' @export
#' @noRd
`$.AuditTrail` <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
`$<-.AuditTrail` <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
`[.AuditTrail` <- function(x, i) {
  x@entries[i]
}

#' @export
#' @noRd
`[<-.AuditTrail` <- function(x, i, value) {
  x@entries[i] <- value
  x
}

#' @export
#' @noRd
`[[.AuditTrail` <- function(x, i) {
  x@entries[[i]]
}

#' @export
#' @noRd
`[[<-.AuditTrail` <- function(x, i, value) {
  x@entries[[i]] <- value
  x
}

#' @export
#' @noRd
S7::method(add, AuditTrail) <- function(x, value) {
  time_stamp <- as.POSIXct(Sys.time())
  value_classes <- class(value)
  
  if (length(value_classes) > 1) {
    value_parent <- class(value)[2]
    value_class <- class(value)[1]
  } else {
    parent_class <- NA_character_
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

  entries_list <- x@entries
  entries_list[[as.character(time_stamp)]] <- entry
  x@entries <- entries_list
  x
}

#' @export
#' @noRd
S7::method(as.list, AuditTrail) <- function(x, ...) {
  x@entries
}

#' @export
#' @noRd
S7::method(as.data.frame, AuditTrail) <- function(x, ...) {
  if (length(x) == 0) return(data.table::data.table())
  dt_list <- lapply(x@entries, function(z) {
    data.table::data.table(
      time_stamp = z@time_stamp,
      class = z@value_class,
      parent = z@value_parent,
      value = z@value
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
    cat("Class:  ", x@entries[[n]]$value_class, "\n")
    cat("Parent: ", x@entries[[n]]$value_parent, "\n")
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
