
#' @export
#' @noRd
EngineSaveFile <- S7::new_class("EngineSaveFile", package = "StreamFind",
  
  properties = list(
    engine = S7::new_property(S7::class_character, default = NA_character_),
    path = S7::new_property(S7::class_character, default = NA_character_),
    name = S7::new_property(S7::class_character, getter = function(self) basename(self@file), default = NA_character_),
    dir = S7::new_property(S7::class_character, getter = function(self) dirname(self@file), default = NA_character_),
    is_saved = S7::new_property(S7::class_logical, getter = function(self) {
      if (is.na(self@path)) return(FALSE)
      db <- .openCacheDBScope(file = self@path)
      engine_name <- DBI::dbListTables(db)
      if (engine_name == self@engine) return(TRUE)
    }, default = FALSE)
  ),
  
  constructor = function(engine = NA_character_, file = NA_character_) {
    if (!is.na(file) && tools::file_ext(file) != "sqlite") stop("File is not an sqlite file!")
    if (!file.exists(file)) if (tools::file_ext(file) %in% "sqlite") file.create(file)
    
    if (!is.na(file)) {
      db <- .openCacheDBScope(file = file)
      engine_name <- DBI::dbListTables(db)
      
      if (length(engine_name) != 0) {
        
        if (!engine_name %in% engine) {
          stop("Engine name does not match the file!")
        }
        
        if (length(engine_name) > 1) {
          stop("Engine name is not unique!")
        }
      }
    }
    
    S7::new_object(S7::S7_object(), engine = engine, path = file)
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
    
    if (!self@engine %in% .get_available_engines()) {
      warning("Engine is not valid!")
      return(FALSE)
    }
    
    if (!is.na(self@path)) {
      db <- .openCacheDBScope(file = self@path)
      engine_name <- DBI::dbListTables(db)
      
      if (length(engine_name) != 0) {
        
        if (!engine_name %in% self@engine) {
          warning("Engine name does not match the file!")
          return(FALSE)
        }
        
        if (length(engine_name) > 1) {
          warning("Engine name is not unique!")
          return(FALSE)
        }
      }
    }
    
    NULL
  }
)
