#' **MassSpecSettings_CalculateSpectraCharges_StreamFind**
#'
#' @description Calculates spectral charges from multi-charged compounds (e.g. proteins and
#' monoclonal antibodies) for mass deconvolution.
#' 
#' @param roundVal Numeric (length 1) with the rounding value for the m/z values before applying
#' charge clustering.
#' @param relLowCut Numeric (length 1) with the relative low cut for the charge clustering.
#' @param absLowCut Numeric (length 1) with the absolute low cut for the charge clustering.
#' @param onlyMaxScan Logical (length 1) to consider only the scan with the highest intensity.
#' @param top Numeric (length 1) with the number of top charges to be considered.
#'
#' @return A MassSpecSettings_CalculateSpectraCharges_StreamFind object.
#'
#' @export
#'
MassSpecSettings_CalculateSpectraCharges_StreamFind <- S7::new_class(
  name = "MassSpecSettings_CalculateSpectraCharges_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(roundVal = 35,
                         relLowCut = 0.2,
                         absLowCut = 300,
                         onlyMaxScan = FALSE,
                         top = 5) {
    S7::new_object(
      ProcessingSettings(
        engine = "MassSpec",
        method = "CalculateSpectraCharges",
        required = "LoadSpectra",
        algorithm = "StreamFind",
        parameters = list(
          roundVal = as.numeric(roundVal),
          relLowCut = as.numeric(relLowCut),
          absLowCut = as.numeric(absLowCut),
          onlyMaxScan = as.logical(onlyMaxScan),
          top = as.integer(top)
        ),
        number_permitted = 1,
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
    checkmate::assert_choice(self@method, "CalculateSpectraCharges")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$roundVal)
    checkmate::assert_number(self@parameters$relLowCut)
    checkmate::assert_number(self@parameters$absLowCut)
    checkmate::assert_logical(self@parameters$onlyMaxScan)
    checkmate::assert_integer(self@parameters$top)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_CalculateSpectraCharges_StreamFind) <- function(x, engine = NULL) {
  
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
  
  parameters <- x$parameters
  roundVal <- parameters$roundVal
  relLowCut <- parameters$relLowCut
  absLowCut <- parameters$absLowCut
  onlyMaxScan <- parameters$onlyMaxScan
  top_charges <- parameters$top
  
  charges <- lapply(spec_list, function(z,
                                        roundVal,
                                        relLowCut,
                                        absLowCut,
                                        top_charges,
                                        onlyMaxScan) {
    
    mz <- NULL
    . <- NULL
    
    if (nrow(z) > 0) {
      
      intensity <- NULL
      
      if (onlyMaxScan)  {
        highest_intensity_scan <- z[which.max(z$intensity), ][["rt"]]
      } else {
        highest_intensity_scan <- unique(z$rt)
      }
      
      z$cluster <- round(z$mz / roundVal) * roundVal
      
      sp <- data.table::copy(z)
      
      data.table::setorder(sp, -intensity)
      
      sp$mzLow <- NA_real_
      sp$mzHigh <- NA_real_
      sp$intLow <- NA_real_
      sp$intHigh <- NA_real_
      
      clusters <- unique(sp$cluster)
      
      for (i in seq_len(length(clusters))) {
        temp <- sp[sp$cluster == clusters[i] & sp$rt %in% highest_intensity_scan, ]
        if (FALSE) {
          plot(temp$mz[order(temp$mz)], temp$intensity[order(temp$mz)], type = 'p')
        }
        sp$mzLow[sp$cluster == clusters[i]] <- min(temp$mz)
        sp$mzHigh[sp$cluster == clusters[i]] <- max(temp$mz)
        sp$intLow[sp$cluster == clusters[i]] <- min(temp$intensity)
        sp$intHigh[sp$cluster == clusters[i]] <- max(temp$intensity)
        mzMax <- temp$mz[temp$intensity == max(temp$intensity)]
        sp$mz[sp$cluster == clusters[i]] <- mzMax[1]
      }
      
      sp2 <- sp[, .(intensity = max(intensity)), by = c("mz", "cluster")]
      
      data.table::setorder(sp2, -mz)
      
      for (i in seq_len(nrow(sp2))) {
        
        if (is.na(sp2$mz[i + 1])) next
        
        if (sp2$mz[i] - sp2$mz[i + 1] < roundVal) {
          
          if (sp2$intensity[i] >= sp2$intensity[i + 1]) {
            sel <- sp2$mz %in% sp2$mz[i + 1]
            sp2$mz[sel] <- sp2$mz[i]
            sp2$cluster[sel] <- sp2$cluster[i]
            sp2$intensity[sel] <- sp2$intensity[i]
            
          } else {
            sel <- sp2$mz %in% sp2$mz[i]
            sp2$mz[sel] <- sp2$mz[i + 1]
            sp2$cluster[sel] <- sp2$cluster[i + 1]
            sp2$intensity[sel] <- sp2$intensity[i + 1]
          }
        }
      }
      
      sp2 <- unique(sp2)
      
      data.table::setorder(sp2, -intensity)
      
      if (!is.null(absLowCut)) {
        sp2 <- sp2[sp2$intensity > absLowCut, ]
      } else {
        sp2 <- sp2[sp2$intensity / sp2$intensity[1] > relLowCut, ]
      }
      
      if (nrow(sp2) == 0) return(data.table::data.table())
      
      plot_charges_temp <- function(z,
                                    sp,
                                    sp2,
                                    absLowCut,
                                    relLowCut,
                                    roundVal,
                                    highest_intensity_scan) {
        
        sel <- z$rt %in% highest_intensity_scan
        
        plot(
          z$mz[sel], z$intensity[sel], type = 'l',
          main = "Clusters, overlap window and low intensity threshold (lowCut)"
        )

        aboveLowCut <- sp$cluster %in% sp2$cluster
        aboveLowCut <- sp[aboveLowCut, ]

        rect(
          aboveLowCut$mzLow,
          aboveLowCut$intLow,
          aboveLowCut$mzHigh,
          aboveLowCut$intHigh,
          border = "lightgreen",
          lty = 2,
          lwd = 0.8,
          angle = 30
        )

        belowLowCut <- !sp$cluster %in% sp2$cluster
        belowLowCut <- sp[belowLowCut, ]

        rect(
          belowLowCut$mzLow,
          belowLowCut$intLow,
          belowLowCut$mzHigh,
          belowLowCut$intHigh,
          border = "darkred",
          lty = 2,
          lwd = 0.8,
          angle = 30
        )

        if (!is.null(absLowCut)) {
          abline(h = absLowCut, col = "darkred")

        } else {
          abline(h = sp2$intensity[1] * relLowCut, col = "darkred")
        }

        text(
          sp2$mz,
          sp2$intensity,
          labels = seq_len(nrow(sp2)),
          adj = c(0.2, 0),
          col = "darkgreen"
        )

        rect(
          sp2$mz - roundVal,
          0,
          sp2$mz + roundVal,
          sp2$intensity,
          border = "darkgreen",
          lty = 1,
          lwd = 1,
          angle = 30
        )

        legend(
          x = "topright",
          legend = c("Clusters", "lowCut", "Excluded clusters", "Final clusters & window"),
          col = c("lightgreen", "darkred", "darkred", "darkgreen"),
          lwd = 2,
          lty = c(2, 1, 2, 1),
          cex = 0.7,
          bty = "n"
        )
      }

      if (FALSE) {
        plot_charges_temp(z, sp, sp2, absLowCut, relLowCut, roundVal, highest_intensity_scan)
      }
      
      res <- z[z$mz == 0, ]
      
      for (i in seq_len(nrow(sp2))) {
        temp <- z[z$cluster == sp2$cluster[i] & z$mz == sp2$mz[i], ]
        res <- rbind(res, temp)
      }
      
      res <- unique(res)
      
      data.table::setorder(res, mz)
      
      res$z <- NA_real_
      res$z_left <- NA_real_
      res$z_right <- NA_real_

      # Based on the relationship m/z_i * Z = m/z_{i+1} * (Z-1) giving
      # Z = -m/z_{i+1} / (m/z_i - m/z_{i+1}) and m/z_i * Z = m/z_{i-1} * (Z+1)
      # giving Z = m/z_{i-1} / (m/z_i - m/z_{i-1})
      for (i in seq_len(nrow(res))) {
        res$z_right[i] <- (-res$mz[i + 1]) / (res$mz[i] - res$mz[i + 1])
        if (i == 1) {
          res$z_left[i] <- NA_real_
        } else {
          res$z_left[i] <- (res$mz[i - 1]) / (res$mz[i] - res$mz[i - 1])
        }
      }
      
      res$z_left <- round(res$z_left, digits = 0)
      res$z_right <- round(res$z_right, digits = 0)
      
      if (nrow(res) <= 2) return(data.table::data.table())

      res$z_step_left_b <- 0
      res$z_step_right_b <- 0
      res$z_step_left_f <- 0
      res$z_step_right_f <- 0
        
      for (i in seq_len(nrow(res))[c(-1, -nrow(res))]) {
        res$z_step_left_b[i] <- res$z_left[i - 1] - res$z_left[i]
        res$z_step_right_b[i] <- res$z_right[i - 1] - res$z_right[i]
        res$z_step_left_f[i] <- res$z_left[i] - res$z_left[i + 1]
        res$z_step_right_f[i] <- res$z_right[i] - res$z_right[i + 1]
      }
      
      res$outlier <- apply(
        res[, c("z_step_left_b", "z_step_right_b", "z_step_left_f", "z_step_right_f")],
        1,
        function(x) !all(x == 1)
      )
      
      res <- res[!res$outlier, ]
      
      res <- res[order(res$intensity, decreasing = TRUE), ]
      
      res <- res[1:top_charges, ]
      
      res$z <- round((res$z_left + res$z_right) / 2, digits = 0)
      
      res$mass <- NA_real_
      
      calculate_mass <- function(mz, z, H = 1.007276) {
        M <- z * (mz - H)
        return(M)
      }
      
      for (i in seq_len(nrow(res))) res$mass[i] <- calculate_mass(res$mz[i], res$z[i])
      
      plot_charges_annotated <- function(z, res) {
        plot(z$mz, z$intensity, type = 'l', ylim = c(0, max(z$intensity) * 1.4))

        no_outlier <- !res$outlier
        is_outlier <- res$outlier

        text(
          x = res$mz[no_outlier], y = res$intensity[no_outlier], adj = c(-0.1, 0.25),
          labels = paste0(round(res$mz[no_outlier], digits = 3), " (+", res$z[no_outlier], ")"),
          vfont = NULL, cex = 0.6, col = "darkgreen", font = NULL, srt = 90
        )

        if (any(res$outlier)) {
          text(
            x = res$mz[is_outlier], y = res$intensity[is_outlier], adj = c(-0.1, 0.25),
            labels = paste0(round(res$mz[is_outlier], digits = 3), " (+", res$z[is_outlier], ")"),
            vfont = NULL, cex = 0.6, col = "darkred", font = NULL, srt = 90
          )
        }
      }
      
      if (FALSE) plot_charges_annotated(z, res)
      
      res <- res[,
        c("id", "polarity", "level", "rt", "mz", "intensity", "cluster", "z", "mass"),
        with = FALSE
      ]
      
      res
      
    } else {
      data.table::data.table()
    }
    
  },
  roundVal = roundVal,
  relLowCut = relLowCut,
  absLowCut = absLowCut,
  top_charges = top_charges,
  onlyMaxScan = onlyMaxScan
  )
  
  names(charges) <- names(spec_list)
  
  engine$spectra$charges <- charges
  message(paste0("\U2713 ", "Spectra charges calculated!"))
  TRUE
}
