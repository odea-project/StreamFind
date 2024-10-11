#' @export
#' @noRd
EngineSaveFile <- S7::new_class("EngineSaveFile", package = "StreamFind",
  
  properties = list(
    
    path = S7::new_property(S7::class_character, default = NA_character_),
    
    name = S7::new_property(S7::class_character, getter = function(self) basename(self@path), default = NA_character_),
    
    dir = S7::new_property(S7::class_character, getter = function(self) dirname(self@path), default = NA_character_),
    
    format = S7::new_property(S7::class_character, getter = function(self) {
      if (is.na(self@path)) return(NA_character_)
      tools::file_ext(self@path)
    }),
    
    engine = S7::new_property(S7::class_character, getter = function(self) {
      if (is.na(self@path)) return(NA_character_)
      
      if (self$format %in% "sqlite") {
        db <- .openCacheDBScope(file = self@path)
        engine_name <- DBI::dbListTables(db)
        if (length(engine_name) == 0) return(NA_character_)
        engine_name
      } else if (self$format %in% "rds") {
        data <- readRDS(self@path)
        if (is.list(data)) if ("engine" %in% names(data)) return(data$engine)
        NA_character_
      } else {
        NA_character_
      }
    })
  ),
  
  constructor = function(file = NA_character_) {
    if (!is.na(file)) {
      
      if (!tools::file_ext(file) %in% c("sqlite", "rds")) {
        warning("File is not an sqlite or rds file!")
        file = NA_character_
        
      } else {
        file_format <- tools::file_ext(file)
        if (!file.exists(file)) {
          if (file_format %in% "sqlite") file.create(file)
          if (file_format %in% "rds") saveRDS(StreamFind::CoreEngine$new(), file)
        }
      }
    }
    
    S7::new_object(S7::S7_object(), path = file)
  },
  
  validator = function(self) {
    
    if (is.na(self@path)) return(NULL)
    
    if (!file.exists(self@path)) {
      warning("File does not exist!")
      return(FALSE)
    }
    
    if (!tools::file_ext(self@path) %in% c("sqlite", "rds")) {
      warning("File is not an sqlite or rds file!")
      return(FALSE)
    }
    
    if (!is.na(self$engine)) {
      if (!self$engine %in% .get_available_engines()) {
        warning("Saved engine name is not available!")
        return(FALSE)
      }
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(`$`, EngineSaveFile) <- function(x, i) {
  S7::prop(x, i)
}

#' @export
#' @noRd
S7::method(`$<-`, EngineSaveFile) <- function(x, i, value) {
  S7::prop(x, i) <- value
  x
}
