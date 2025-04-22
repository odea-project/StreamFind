# MARK: Metadata
# Metadata -----
#' @title Generic Metadata
#'
#' @description The `Metadata` S7 class holds information, such as name, date, author and file, as a named list with elements of length one.
#'
#' @template arg-Metadata-entries
#' @template slot-Metadata-entries
#' 
#' @return A `Metadata` S7 class object.
#'
#' @export
#' 
Metadata <- S7::new_class(
  name = "Metadata",
  package = "StreamFind",
  properties = list(
    entries = S7::new_property(class = S7::class_list, default = list())
  ),
  constructor = function(entries = list()) {
    
    if (length(entries) > 0) {
      
      entries_names <- names(entries)
      
      if (is.null(entries_names)) {
        warning("Entries list must have names!")
        entries <- list()
      }
      
      if (any(duplicated(entries_names))) {
        warning(" Duplicated names not permitted in entries list!")
        entries <- entries[!duplicated(entries_names)]
      }
      
      entries <- lapply(entries, function(h) {
        if (any(is.null(h))) h <- NA_character_
        if (any(is.na(h))) h <- NA_character_
        h
      })
      
      for (i in seq_len(length(entries))) {
        if (length(entries[[i]]) == 0) entries <- entries[-i]
      }
    }

    if ("date" %in% names(entries)) {
      entries[["date"]] <- as.POSIXct(entries[["date"]])
      attr(entries[["date"]], "tzone") <- NULL
    }
    
    if (!"name" %in% names(entries)) entries[["name"]] <- NA_character_
    if (!"author" %in% names(entries)) entries[["author"]] <- NA_character_
    if (!"date" %in% names(entries)) entries[["date"]] <- Sys.time()
    if (!"file" %in% names(entries)) entries[["file"]] <- NA_character_
    
    S7::new_object(S7::S7_object(), entries = entries)
  },
  
  validator = function(self) {
    valid <- FALSE
    
    if (is.list(self@entries)) {
      valid <- TRUE
      
      invalid_entries <- sapply(self@entries[!names(self@entries) %in% "date"], function(x) {
        !(is.character(x) || is.numeric(x)) || length(x) != 1
      })
      
      if (any(invalid_entries)) {
        warning("All entries must be of type character or numeric and of length 1!")
        valid <- FALSE
      }
      
      if (length(unique(names(self@entries))) != length(self@entries)) {
        warning("Metadata must have names and not permitted duplicated names!")
        valid <- FALSE
      }
      
      if (!all(c("name", "author", "date") %in% names(self@entries))) {
        warning("Metadata must contain at least entries name, author, file and date!")
        valid <- FALSE
      }
      
      if ("name" %in% names(self@entries)) {
        if (!is.character(self@entries[["name"]])) {
          warning("Metadata entry name must be character length 1!")
          valid <- FALSE
        }
      }
      
      if ("author" %in% names(self@entries)) {
        if (!is.character(self@entries[["author"]])) {
          warning("Metadata entry author must be character length 1!")
          valid <- FALSE
        }
      }
      
      if ("date" %in% names(self@entries)) {
        if (!all(grepl("POSIXct|POSIXt", class(self@entries[["date"]])))) {
          warning("Metadata entry date class must be POSIXct or POSIXt length 1!")
          valid <- FALSE
        }
      }
      
      if ("file" %in% names(self@entries)) {
        if (!is.character(self@entries[["file"]])) {
          warning("Metadata entry file must be character length 1!")
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
S7::method(length, Metadata) <- function(x) {
  length(x@entries)
}

#' @export
#' @noRd
S7::method(names, Metadata) <- function(x) {
  names(x@entries)
}

#' @export
#' @noRd
`[.StreamFind::Metadata` <- function(x, i) {
  x@entries[i]
}

#' @export
#' @noRd
`[<-.StreamFind::Metadata` <- function(x, i, value) {
  x@entries[i] <- value
  x
}

#' @export
#' @noRd
`[[.StreamFind::Metadata` <- function(x, i) {
  x@entries[[i]]
}

#' @export
#' @noRd
`[[<-.StreamFind::Metadata` <- function(x, i, value) {
  x@entries[[i]] <- value
  x
}

#' @export
#' @noRd
`$.StreamFind::Metadata` <- function(x, i) {
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
`$<-.StreamFind::Metadata` <- function(x, i, value) {
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
S7::method(as.list, Metadata) <- function(x, ...) {
  x@entries
}

#' @export
#' @noRd
S7::method(save, Metadata) <- function(x, file = "metadata.json") {
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
S7::method(read, Metadata) <- function(x, file) {
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

#' @export
#' @noRd
S7::method(show, Metadata) <- function(x) {
  if (length(x) > 0) {
    str <- NULL
    for (n in seq_len(length(x))) {
      str <- c(str, paste(names(x@entries)[n], ": ", as.character(x@entries[[n]]), sep = ""))
    }
    cat(str, sep = "\n")
  }
  
}

# MARK: EngineMetadata S7 class
# EngineMetadata -----
#' @title Engine Metadata
#' 
#' @description The `EngineMetadata` class is a subclass of the [StreamFind::Metadata] class. It is used to store metadata of data processing engines.
#' 
#' @template arg-Metadata-entries
#' @param engine A character string with the name of the engine. Default is "CoreEngine".
#' 
#' @template slot-Metadata-entries
#' @slot engine A character string with the name of the engine.
#' 
#' @seealso [StreamFind::Metadata]
#' 
#' @export
#' 
EngineMetadata <-  S7::new_class(
  name = "EngineMetadata",
  package = "StreamFind",
  parent = Metadata,
  
  properties = list(
    engine = S7::new_property(S7::class_character, default = "CoreEngine")
  ),
  
  constructor = function(entries = list(), engine = "CoreEngine") {
    S7::new_object(
      Metadata(entries),
      engine = engine
    )
  },
  
  validator = function(self) {
    checkmate::assert_character(self@engine)
    if ("file" %in% names(self@entries)) {
      if (!is.na(self@entries[["file"]])) {
        checkmate::assert_true(tools::file_ext(self@entries[["file"]]) %in% c("sqlite", "rds"))
      }
    }
    NULL
  }
)

