
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_FindInternalStandards_StreamFind**
#'
#' @description Settings for finding internal standards using a data.frame.
#'
#' @param database A data.table with at least the columns name, mass, and rt indicating the name, neutral monoisotopic 
#' mass and retention time of the internal standards, respectively.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#'
#' @return A `MassSpecSettings_FindInternalStandards_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_FindInternalStandards_StreamFind <- S7::new_class("MassSpecSettings_FindInternalStandards_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(database = data.table::data.table(
                           name = character(),
                           formula = character(),
                           mass = numeric(),
                           rt = numeric()
                         ),
                         ppm = 5,
                         sec = 10) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "FindInternalStandards",
      algorithm = "StreamFind",
      parameters = list(
        database = data.table::as.data.table(database),
        ppm = as.numeric(ppm),
        sec = as.numeric(sec)
      ),
      number_permitted = 1,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "FindInternalStandards")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$ppm)
    checkmate::assert_number(self@parameters$sec)
    checkmate::assert_data_table(self@parameters$database)
    checkmate::assert_true(
      all(c("name", "neutralMass", "rt") %in% colnames(self@parameters$database)) ||
      all(c("name", "mass", "rt") %in% colnames(self@parameters$database)) ||
      all(c("name", "mz", "rt") %in% colnames(self@parameters$database))
    )
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_FindInternalStandards_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }
  
  nts <- engine$nts
  
  if (!nts$has_features) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }
  
  cache <- .load_chache("find_internal_standards", nts$features, x)

  if (!is.null(cache$data)) {
    tryCatch({
      features <- nts$feature_list
      features <- Map(function(x, y) {
        x[["istd"]] <- y
        x
      }, features, cache$data)
      nts$feature_list <- features
      engine$nts <- nts
      message("\U2139 Internal standards annotation loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  database <- x$parameters$database
  
  database <- data.table::as.data.table(database)
  
  if (nrow(database) == 0) {
    warning("Database is empty!")
    return(FALSE)
  }
  
  internal_standards <- engine$get_suspects(
    database = database,
    ppm = x$parameters$ppm,
    sec = x$parameters$sec,
    filtered = TRUE,
    onGroups = FALSE
  )
  
  if (nrow(internal_standards) == 0) {
    warning("Internal standards were not found!")
    return(FALSE)
  }
  
  if ("intensity" %in% colnames(database)) {
    intensity <- database$intensity
    names(intensity) <- database$name
    
    internal_standards$rec <- round((internal_standards$intensity / intensity[internal_standards$istd_name]) * 100, digits = 1)
    
  } else if ("area" %in% colnames(database)) {
    area <- database$area
    names(area) <- database$name
    
    internal_standards$rec <- round(
      (internal_standards$area / area[internal_standards$istd_name]) * 100, 
      digits = 1
    )
    
  } else {
    
    blks <- engine$analyses$blanks
    
    if (any(!is.na(blks)) & nts$has_groups) {
      
      rpls <- engine$analyses$replicates
      
      internal_standards$replicate <- rpls[internal_standards$analysis]
      
      internal_standards$rec <- vapply(seq_len(nrow(internal_standards)),
        function(x, internal_standards, blks) {
         
          feat <- internal_standards[x, ]
          
          if (feat$replicate %in% blks) return(1)
          
          feat_area <- feat$area
          
          blk <- blks[feat$analysis]
          
          blk_feats <- internal_standards[
           internal_standards$replicate %in% blk &
             internal_standards$group %in% feat$group, ]
          
          if (nrow(blk_feats) > 0) {
            blk_area <- mean(blk_feats$area, na.rm = TRUE)
            return(round(feat_area / blk_area, digits = 2))
          } else {
           NA_real_
          }
        }, 
        internal_standards = internal_standards,
        blks = blks,
        NA_real_
      )
    } else {
      internal_standards$rec <- NA_real_  
    }
  }
  
  if (nrow(internal_standards) > 0) {
    
    internal_standards_l <- split(internal_standards, internal_standards$analysis)
    
    features <- nts$feature_list
    
    istd_col <- lapply(names(features), function(x, features, internal_standards_l) {
      
      istd <- internal_standards_l[[x]]
      
      fts <- features[[x]]
      
      if (!is.null(istd)) {
        
        istd_l <- lapply(fts$feature, function(z, istd) {
          
          istd_idx <- which(istd$feature %in% z)
          
          if (length(istd_idx) > 0) {
            istd_temp <- istd[istd_idx, ]
            istd_temp <- istd_temp[, c("name", "error_mass", "error_rt", "rec"), with = FALSE]
            
            if (nrow(istd_temp) > 0) {
              istd_temp
            } else {
              NULL
            }
          } else {
            NULL
          }
          
        }, istd = istd)
        
        istd_l
        
      } else {
        lapply(fts$feature, function(x) NULL)
      }
      
    }, features = features, internal_standards_l = internal_standards_l)
    
    names(istd_col) <- names(features)
    
    if (!is.null(cache$hash)) {
      .save_cache("find_internal_standards", istd_col, cache$hash)
      message("\U1f5ab Internal standards annotation cached!")
    }
    
    features <- Map(function(x, y) {
      x[["istd"]] <- y
      x
    }, features, istd_col)
    
    nts$feature_list <- features
    
    engine$nts <- nts
    
    message("\U2713 ", length(unique(internal_standards$name)), " internal standards found and tagged!")
    
    TRUE
    
  } else {
    FALSE
  }
}
