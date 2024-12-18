#' **MassSpecSettings_NormalizeSpectra_minmax**
#'
#' @description Normalizes spectra using the min-max algorithm.
#'
#' @return A MassSpecSettings_NormalizeSpectra_minmax object.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_minmax <- S7::new_class(
  name = "MassSpecSettings_NormalizeSpectra_minmax",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "NormalizeSpectra",
        required = "LoadSpectra",
        algorithm = "minmax",
        parameters = list(),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "NormalizeSpectra")
    checkmate::assert_choice(self@algorithm, "minmax")
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_NormalizeSpectra_minmax) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
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
  TRUE
}

#' **MassSpecSettings_NormalizeSpectra_snv**
#'
#' @description Normalizes spectra using the Standard Normal Variate (SNV) algorithm.
#' 
#' @param liftTozero Logical (length 1) indicating if the spectra should be lifted to zero.
#'
#' @return A MassSpecSettings_NormalizeSpectra_snv object.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_snv <- S7::new_class(
  name = "MassSpecSettings_NormalizeSpectra_snv",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(liftTozero = FALSE) {
  
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "NormalizeSpectra",
        required = "LoadSpectra",
        algorithm = "snv",
        parameters = list(liftTozero = liftTozero),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = NA_character_,
        developer = "J\u00FCrgen Schram",
        contact = "schram@hsnr.de",
        link = NA_character_,
        doi = "10.1016/j.trac.2018.12.004"
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "NormalizeSpectra")
    checkmate::assert_choice(self@algorithm, "snv")
    checkmate::assert_logical(self@parameters$liftTozero, max.len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_NormalizeSpectra_snv) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
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
  
  TRUE
  
  
}

#' **MassSpecSettings_NormalizeSpectra_scale**
#'
#' @description Normalizes spectra using scaling based on the standard deviation.
#'
#' @return A MassSpecSettings_NormalizeSpectra_scale object.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_scale <- S7::new_class(
  name = "MassSpecSettings_NormalizeSpectra_scale",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "NormalizeSpectra",
        required = "LoadSpectra",
        algorithm = "scale",
        parameters = list(),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "NormalizeSpectra")
    checkmate::assert_choice(self@algorithm, "scale")
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_NormalizeSpectra_scale) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
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
  TRUE
  
  
}

#' **MassSpecSettings_NormalizeSpectra_blockweight**
#'
#' @description Normalizes spectra using block weighting for downstream data evaluation.
#'
#' @return A MassSpecSettings_NormalizeSpectra_blockweight object.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_blockweight <- S7::new_class(
  name = "MassSpecSettings_NormalizeSpectra_blockweight",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "NormalizeSpectra",
        required = "LoadSpectra",
        algorithm = "blockweight",
        parameters = list(),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "NormalizeSpectra")
    checkmate::assert_choice(self@algorithm, "blockweight")
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_NormalizeSpectra_blockweight) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
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
  TRUE
}

#' **MassSpecSettings_NormalizeSpectra_meancenter**
#'
#' @description Normalizes spectra using mean centering.
#'
#' @return A MassSpecSettings_NormalizeSpectra_meancenter object.
#'
#' @export
#'
MassSpecSettings_NormalizeSpectra_meancenter <- S7::new_class(
  name = "MassSpecSettings_NormalizeSpectra_meancenter",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function() {
    
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "NormalizeSpectra",
        required = "LoadSpectra",
        algorithm = "meancenter",
        parameters = list(),
        number_permitted = Inf,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "NormalizeSpectra")
    checkmate::assert_choice(self@algorithm, "meancenter")
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_NormalizeSpectra_meancenter) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
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
  TRUE
}
