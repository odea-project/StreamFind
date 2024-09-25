
# ______________________________________________________________________________________________________________________
# minmax -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_NormalizeSpectra_minmax**
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A RamanSettings_NormalizeSpectra_minmax object.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_minmax <- S7::new_class("RamanSettings_NormalizeSpectra_minmax",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "NormalizeSpectra",
      algorithm = "minmax",
      parameters = list(),
      number_permitted = Inf,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "NormalizeSpectra"),
      checkmate::test_choice(self@algorithm, "minmax")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_NormalizeSpectra_minmax) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          max_int <- max(z$intensity)
          min_int <- min(z$intensity)
          z$intensity <- (z$intensity - min_int) / (max_int - min_int + abs(min_int))
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        max_int <- max(z$intensity)
        min_int <- min(z$intensity)
        z$intensity <- (z$intensity - min_int) / (max_int - min_int)
      }
    }
    
    z
    
  })
  
  engine$spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# ______________________________________________________________________________________________________________________
# snv -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_NormalizeSpectra_snv**
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#' 
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A RamanSettings_NormalizeSpectra_snv object.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_snv <- S7::new_class("RamanSettings_NormalizeSpectra_snv",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(liftTozero = FALSE) {
  
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "NormalizeSpectra",
      algorithm = "snv",
      parameters = list(liftTozero = liftTozero),
      number_permitted = Inf,
      version = as.character(packageVersion("StreamFind")),
      software = NA_character_,
      developer = "J\u00FCrgen Schram",
      contact = "schram@hsnr.de",
      link = NA_character_,
      doi = "10.1016/j.trac.2018.12.004"
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "NormalizeSpectra"),
      checkmate::test_choice(self@algorithm, "snv"),
      checkmate::test_logical(self@parameters$liftTozero, max.len = 1)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_NormalizeSpectra_snv) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  liftTozero <- x$parameters$liftTozero
  
  spec_list <- engine$spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          sd_int <- sd(z$intensity)
          z$intensity <- (z$intensity - mean_int) / sd_int
          if (liftTozero) z$intensity <- z$intensity + abs(min(z$intensity))
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        mean_int <- mean(z$intensity)
        sd_int <- sd(z$intensity)
        z$intensity <- (z$intensity - mean_int) / sd_int
        if (liftTozero) z$intensity <- z$intensity + abs(min(z$intensity))
      }
    }
    
    z
    
  })
  
  self$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# ______________________________________________________________________________________________________________________
# scale -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_NormalizeSpectra_scale**
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A RamanSettings_NormalizeSpectra_scale object.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_scale <- S7::new_class("RamanSettings_NormalizeSpectra_scale",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "NormalizeSpectra",
      algorithm = "scale",
      parameters = list(),
      number_permitted = Inf,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "NormalizeSpectra"),
      checkmate::test_choice(self@algorithm, "scale")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_NormalizeSpectra_scale) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          sd_int <- sd(z$intensity)
          z$intensity <- z$intensity / sd_int
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        sd_int <- sd(z$intensity)
        z$intensity <- z$intensity / sd_int
      }
    }
    
    z
    
  })
  
  engine$spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# ______________________________________________________________________________________________________________________
# blockweight -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_NormalizeSpectra_blockweight**
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A RamanSettings_NormalizeSpectra_blockweight object.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_blockweight <- S7::new_class("RamanSettings_NormalizeSpectra_blockweight",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "NormalizeSpectra",
      algorithm = "blockweight",
      parameters = list(),
      number_permitted = Inf,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "NormalizeSpectra"),
      checkmate::test_choice(self@algorithm, "blockweight")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_NormalizeSpectra_blockweight) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          z$intensity <- z$intensity / sqrt(length(z$intensity))
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        z$intensity <- z$intensity / sqrt(length(z$intensity))
      }
    }
    
    z
    
  })
  
  engine$spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}

# ______________________________________________________________________________________________________________________
# meancenter -----
# ______________________________________________________________________________________________________________________

#' **RamanSettings_NormalizeSpectra_meancenter**
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A RamanSettings_NormalizeSpectra_meancenter object.
#'
#' @export
#'
RamanSettings_NormalizeSpectra_meancenter <- S7::new_class("RamanSettings_NormalizeSpectra_meancenter",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(ProcessingSettings(
      engine = "Raman",
      method = "NormalizeSpectra",
      algorithm = "meancenter",
      parameters = list(),
      number_permitted = Inf,
      version = as.character(packageVersion("StreamFind")),
      software = "StreamFind",
      developer = "Ricardo Cunha",
      contact = "cunha@iuta.de",
      link = "https://odea-project.github.io/StreamFind",
      doi = NA_character_
    ))
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_choice(self@engine, "Raman"),
      checkmate::test_choice(self@method, "NormalizeSpectra"),
      checkmate::test_choice(self@algorithm, "meancenter")
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, RamanSettings_NormalizeSpectra_meancenter) <- function(x, engine = NULL) {
  
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$has_spectra()) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$spectra$spectra
  
  spec_list <- lapply(spec_list, function(z) {
    
    if (nrow(z) > 0) {
      
      if (("rt" %in% colnames(z)) && ("shift" %in% colnames(z))) {
        temp_x <- split(z, z$rt)
        
        temp_x <- lapply(temp_x, function(z) {
          mean_int <- mean(z$intensity)
          z$intensity <- z$intensity - mean_int
          z
        })
        
        z <- rbindlist(temp_x)
        
      } else {
        mean_int <- mean(z$intensity)
        z$intensity <- z$intensity / mean_int
      }
    }
    
    z
    
  })
  
  engine$spectra$spectra <- spec_list
  message(paste0("\U2713 ", "Spectra normalized!"))
  invisible(TRUE)
}
