# MARK: Metadata
# Metadata -----
#' @title Metadata
#'
#' @description The `Metadata` S3 class holds information, such as name, date, author
#' and file, as a named list with elements of length one. The `Metadata` is essentially
#' a list therefore list methods are also applicable.
#'
#' @param entries A named list of metadata entries as elements. Default is an empty list.
#' Elements must be of type character, numeric or POSIXt and must have length 1.
#' 
#' @details If not given, elements name, author, date and file are set to `NA_character_`,
#' `NA_character_`, current system time and `NA_character_`, respectively.
#' 
#' @return A `Metadata` S3 class object.
#'
#' @export
#' 
Metadata <- function(entries = list()) {
  if (!is.list(entries)) {
    stop("Argument must be a named list!")
  }
  if (length(entries) > 0) {
    entries_names <- names(entries)
    if (is.null(entries_names)) {
      warning("List elements must have names!")
      entries <- list()
    }
    if (any(duplicated(entries_names))) {
      warning(" Duplicated names not permitted!")
      entries <- entries[!duplicated(entries_names)]
    }
    entries <- lapply(entries, function(h) {
      if (any(is.null(h))) h <- NA_character_
      if (any(is.na(h))) h <- NA_character_
      if (is.character(h) || is.numeric(h) || inherits(h, "POSIXt")) {
        if (length(h) != 1) {
          warning("All elements must have length 1!")
          h <- NA_character_
        }
      } else {
        warning("All elements must be of type character, numeric or POSIXt!")
        h <- NA_character_
      }
      h
    })
  }
  if ("date" %in% names(entries)) {
    entries[["date"]] <- as.POSIXct(entries[["date"]])
    attr(entries[["date"]], "tzone") <- NULL
  }
  if (!"name" %in% names(entries)) entries[["name"]] <- NA_character_
  if (!"author" %in% names(entries)) entries[["author"]] <- NA_character_
  if (!"date" %in% names(entries)) entries[["date"]] <- Sys.time()
  if (!"file" %in% names(entries)) entries[["file"]] <- NA_character_
  entries <- structure(entries, class = c("Metadata"))
  if (is.null(validate_object(entries))) {
    return(entries)
  } else {
    stop("Invalid Metadata object!")
  }
}

#' @describeIn Metadata Validates the `Metadata` object.
#' @param x A `Metadata` object.
#' @return `NULL` if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.Metadata <- function(x) {
  valid <- FALSE
  if (is.list(x)) {
    valid <- TRUE
    invalid_entries <- sapply(x[!names(x) %in% "date"], function(z) {
      !(is.character(z) || is.numeric(z)) || length(z) != 1
    })
    if (any(invalid_entries)) {
      warning("All elements must be of type character or numeric and of length 1!")
      valid <- FALSE
    }
    if (length(unique(names(x))) != length(x)) {
      warning("Metadata must have names and not permitted duplicated names!")
      valid <- FALSE
    }
    if (!all(c("name", "author", "date") %in% names(x))) {
      warning("Metadata must contain at least entries name, author, file and date!")
      valid <- FALSE
    }
    if ("name" %in% names(x)) {
      if (!is.character(x[["name"]])) {
        warning("Metadata entry name must be character length 1!")
        valid <- FALSE
      }
    }
    if ("author" %in% names(x)) {
      if (!is.character(x[["author"]])) {
        warning("Metadata entry author must be character length 1!")
        valid <- FALSE
      }
    }
    if ("date" %in% names(x)) {
      if (!all(grepl("POSIXct|POSIXt", class(x[["date"]])))) {
        warning("Metadata entry date class must be POSIXct or POSIXt length 1!")
        valid <- FALSE
      }
    }
    if ("file" %in% names(x)) {
      if (!is.character(x[["file"]])) {
        warning("Metadata entry file must be character length 1!")
        valid <- FALSE
      }
    }
  }
  if (!valid) return(FALSE)
  NULL
}

#' @describeIn Metadata Returns the length of the metadata entries.
#' @param x A `Metadata` object.
#' @export
#' 
length.Metadata <- function(x) {
  NextMethod()
}

#' @describeIn Metadata Returns the names of the metadata entries.
#' @param x A `Metadata` object.
#' @export
#' 
names.Metadata <- function(x) {
  NextMethod()
}

#' @describeIn Metadata Subsets the metadata entries.
#' @param x A `Metadata` object.
#' @param i A numeric or character vector with the indices or names of the entries to subset.
#' @export
#' 
`[.Metadata` <- function(x, i) {
  NextMethod()
}

#' @describeIn Metadata Replaces metadata entries.
#' @param x A `Metadata` object.
#' @param i A numeric or character vector with the indices or names of the entries to replace.
#' @param value A list with the new entries to replace.
#' @export
#' 
`[<-.Metadata` <- function(x, i, value) {
  NextMethod()
}

#' @describeIn Metadata Extracts a single metadata entry.
#' @param x A `Metadata` object.
#' @param i A numeric or character vector with the index or name of the entry to extract.
#' @export
#' 
`[[.Metadata` <- function(x, i) {
  NextMethod()
}

#' @describeIn Metadata Replaces a single metadata entry.
#' @param x A `Metadata` object.
#' @param i A numeric or character vector with the index or name of the entry to replace.
#' @param value The new value to replace the entry.
#' @export
#' 
`[[<-.Metadata` <- function(x, i, value) {
  NextMethod()
}

#' @describeIn Metadata Saves the metadata to a file.
#' @param x A `Metadata` object.
#' @param file A character string with the file name to save the metadata.
#' Default is "metadata.json". Supported formats are "json" and "rds".
#' @export
#' 
save.Metadata <- function(x, file = "metadata.json") {
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

#' @describeIn Metadata Reads metadata from a file.
#' @param x A `Metadata` object (not used).
#' @param file A character string with the file name to read the metadata from.
#' The file format should be either "json" or "rds".
#' @export
#' 
read.Metadata <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Metadata(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::Metadata")) return(res)
  }
  NULL
}

#' @describeIn Metadata Prints the metadata entries.
#' @param x A `Metadata` object.
#' @export
#' 
show.Metadata <- function(x) {
  if (length(x) > 0) {
    str <- NULL
    for (n in seq_len(length(x))) {
      str <- c(str, paste(names(x)[n], ": ", as.character(x[[n]]), sep = ""))
    }
    cat(str, sep = "\n")
  }
}

# MARK: EngineMetadata
# EngineMetadata -----
#' @title Engine Metadata
#' 
#' @description The `EngineMetadata` class is a subclass of the [StreamFind::Metadata] class.
#' It is used to store metadata of data processing engines.
#' 
#' @param entries A named list of metadata entries. Default is an empty list.
#' @param data_type A character string with the data type of the engine.
#' Default is `NA_character_`.
#' 
#' @seealso [StreamFind::Metadata]
#' 
#' @export
#' 
EngineMetadata <-  function(entries = list(), data_type = NA_character_) {
  entries <- Metadata(entries)
  entries <- structure(
    entries,
    class = c("EngineMetadata", "Metadata"),
    data_type = data_type
  )
  if (is.null(validate_object(entries))) {
    return(entries)
  } else {
    stop("Invalid EngineMetadata object!")
  }
}

#' @describeIn EngineMetadata Validates the `EngineMetadata` object.
#' @param x An `EngineMetadata` object.
#' @return `NULL` if the object is valid, otherwise an error is thrown.
#' @export
#' 
validate_object.EngineMetadata <- function(x) {
  checkmate::assert_character(attr(x, "data_type"))
  if ("file" %in% names(x)) {
    if (!is.na(x[["file"]])) {
      checkmate::assert_true(tools::file_ext(x[["file"]]) %in% c("sqlite", "rds"))
    }
  }
  NULL
}
