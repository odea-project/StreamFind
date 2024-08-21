
#' @export
#' @noRd
Analyses <- S7::new_class("Analyses", package = "StreamFind",
  
  properties = list(
    
    engine = S7::new_property(S7::class_character, default = NA_character_),
    
    possible_formats = S7::new_property(S7::class_character, default = NA_character_),
    
    analyses = S7::new_property(S7::class_list, default = list()),

    length = S7::new_property(S7::class_numeric, getter = function(self) {
      length(self@analyses)
    }, default = 0),

    names = S7::new_property(S7::class_character, getter = function(self) {
      names(self@analyses)
    }, default = character(0)),
    
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (self@length > 0) {
        df <- data.table::data.table(
          "analysis" = self@names,
          "class" = vapply(self@analyses, function(x) class(x)[1], "")
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    }, default = data.frame())
  ),
  
  constructor = function(analyses = list()) {
    S7::new_object(S7::S7_object(), engine = NA_character_, possible_formats = NA_character_, analyses = analyses)
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_choice(self@engine, choices = c(NA_character_, .get_available_engines())),
      checkmate::test_character(self@possible_formats)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(add, Analyses) <- function(x, analyses) {
  if (!is(analyses, "list")) {
    warning("Analysis must be a list!")
    return(x)
  }
  
  analyses_names <- names(analyses)
  
  if (length(analyses_names) == 0) {
    warning("Analysis names must be provided!")
    return(x)
  }
  
  if (any(vapply(analyses_names, function(a) a %in% x@names, FALSE))) {
    warning("Analysis names already exist!")
    return(x)
  }
  
  analyses <- c(x@analyses, analyses)
  analyses <- analyses[order(names(analyses))]
  x@analyses <- analyses
  x
}

#' @export
#' @noRd
S7::method(length, Analyses) <- function(x) {
  length(x@analyses)
}

#' @export
#' @noRd
S7::method(names, Analyses) <- function(x) {
  names(x@analyses)
}

#' @export
#' @noRd
S7::method(`[`, Analyses) <- function(x, i) {
  analyses_list <- x@analyses
  if (missing(i)) return(analyses_list)
  if (is.numeric(i)) {
    return(analyses_list[i])
  } else if (is.character(i)) {
    return(analyses_list[i])
  } else if (is.logical(i)) {
    return(analyses_list[i])
  } else {
    stop("Invalid analyses subset type")
  }
}

#' @export
#' @noRd
S7::method(`[[`, Analyses) <- function(x, i) {
  analyses_list <- x@analyses
  if (missing(i)) return(analyses_list)
  if (is.numeric(i)) {
    return(analyses_list[[i]])
  } else if (is.character(i)) {
    return(analyses_list[[i]])
  } else {
    stop("Invalid headers subset type")
  }
}

#' @export
#' @noRd
S7::method(`$`, Analyses) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, Analyses) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}

#' @export
#' @noRd
S7::method(as.list, Analyses) <- function(x) {
  x@analyses
}

#' @export
#' @noRd
S7::method(save, Analyses) <- function(x, format = "json", name = "settings", path = getwd()) {
  if (format %in% "json") x <- .convert_to_json(as.list(x))
  .save_data_to_file(x, format, name, path)
}

#' @export
#' @noRd
S7::method(read, Analyses) <- function(x, file) {
  if (grepl(".json", file)) {
    if (file.exists(file)) {
      return(Analyses(jsonlite::fromJSON(file)))
    }
  } else if (grepl(".rds", file)) {
    res <- readRDS(file)
    if (is(res, "StreamFind::Analyses")) return(res)
  }
  NULL
}

#' @export
#' @noRd
S7::method(show, Analyses) <- function(x, ...) {
  cat("\n")
  cat("Analyses")
  if (length(x) > 0) {
    cat("\n")
    overview <- x@info
    row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
    print(overview)
  } else {
    cat(" empty \n")
  }
}
