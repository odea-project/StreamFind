#' @export
#' @noRd
EngineSaveFile <- S7::new_class("EngineSaveFile", package = "StreamFind",
  
  properties = list(
    
    path = S7::new_property(S7::class_character, default = NA_character_),
    
    name = S7::new_property(S7::class_character, getter = function(self) basename(self@path), default = NA_character_),
    
    dir = S7::new_property(S7::class_character, getter = function(self) dirname(self@path), default = NA_character_),
    
    engine = S7::new_property(S7::class_character, getter = function(self) {
      if (is.na(self@path)) return(NA_character_)
      db <- .openCacheDBScope(file = self@path)
      engine_name <- DBI::dbListTables(db)
      if (length(engine_name) == 0) return(NA_character_)
      engine_name
    })
  ),
  
  constructor = function(file = NA_character_) {
    if (!is.na(file) && tools::file_ext(file) != "sqlite") stop("File is not an sqlite file!")
    if (!file.exists(file)) if (tools::file_ext(file) %in% "sqlite") file.create(file)
    
    if (!is.na(file)) {
      db <- .openCacheDBScope(file = file)
      engine_name <- DBI::dbListTables(db)
      
      if (length(engine_name) != 0) {
        if (length(engine_name) > 1) {
          stop("Engine name is not unique!")
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
    
    if (!tools::file_ext(self@path) %in% "sqlite") {
      warning("File is not an sqlite file!")
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
