
#' @noRd
.process.MassSpecSettings_CalculateSpectraCharges_StreamFind <- function(settings, self, private) {

  parameters <- settings$parameters
  
  roundVal <- parameters$roundVal
  
  relLowCut <- parameters$relLowCut
  
  absLowCut <- parameters$absLowCut
  
  if (!self$has_spectra()) {
    warning("Spectra not found! Not done.")
    return(FALSE)
  }
  
  spec_list <- self$spectra
  
  charges <- lapply(spec_list, function(x, roundVal, relLowCut, absLowCut) {
    
    mz <- NULL
    . <- NULL
    
    if (nrow(x) > 0) {
      
      intensity <- NULL
      
      x$cluster <- round(x$mz / roundVal) * roundVal
      
      sp <- data.table::copy(x)
      
      data.table::setorder(sp, -intensity)
      
      sp$mzLow <- NA_real_
      sp$mzHigh <- NA_real_
      sp$intLow <- NA_real_
      sp$intHigh <- NA_real_
      
      clusters <- unique(sp$cluster)
      
      for (i in seq_len(length(clusters))) {
        temp <- sp[sp$cluster == clusters[i], ]
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
      
      # plot_charges_temp <- function(x, sp, sp2, absLowCut, relLowCut, roundVal) {
      #   
      #   plot(x$mz, x$intensity, type = 'l', main = "Clusters, overlap window and low intensity threshold (lowCut)")
      #   
      #   aboveLowCut <- sp$cluster %in% sp2$cluster
      #   aboveLowCut <- sp[aboveLowCut, ]
      #   
      #   rect(
      #     aboveLowCut$mzLow,
      #     aboveLowCut$intLow,
      #     aboveLowCut$mzHigh,
      #     aboveLowCut$intHigh,
      #     border = "lightgreen",
      #     lty = 2,
      #     lwd = 0.8,
      #     angle = 30
      #   )
      #   
      #   belowLowCut <- !sp$cluster %in% sp2$cluster
      #   belowLowCut <- sp[belowLowCut, ]
      #   
      #   rect(
      #     belowLowCut$mzLow,
      #     belowLowCut$intLow,
      #     belowLowCut$mzHigh,
      #     belowLowCut$intHigh,
      #     border = "darkred",
      #     lty = 2,
      #     lwd = 0.8,
      #     angle = 30
      #   )
      #   
      #   if (!is.null(absLowCut)) {
      #     abline(h = absLowCut, col = "darkred")
      #     
      #   } else {
      #     abline(h = sp2$intensity[1] * relLowCut, col = "darkred")
      #   }
      #   
      #   text(
      #     sp2$mz,
      #     sp2$intensity,
      #     labels = seq_len(nrow(sp2)),
      #     adj = c(0.2, 0),
      #     col = "darkgreen"
      #   )
      #   
      #   rect(
      #     sp2$mz - roundVal,
      #     0,
      #     sp2$mz + roundVal,
      #     sp2$intensity,
      #     border = "darkgreen",
      #     lty = 1,
      #     lwd = 1,
      #     angle = 30
      #   )
      #   
      #   legend(
      #     x = "topright",
      #     legend = c("Clusters", "lowCut", "Excluded clusters", "Final clusters & window"),
      #     col = c("lightgreen", "darkred", "darkred", "darkgreen"),
      #     lwd = 2,
      #     lty = c(2, 1, 2, 1),
      #     cex = 0.7,
      #     bty = "n"
      #   )
      # }
      # 
      # if (FALSE) plot_charges_temp(x, sp, sp2, absLowCut, relLowCut, roundVal)
      
      res <- x[x$mz == 0, ]
      
      for (i in seq_len(nrow(sp2))) {
        temp <- x[x$cluster == sp2$cluster[i] & x$mz == sp2$mz[i], ]
        res <- rbind(res, temp)
      }
      
      res <- unique(res)
      
      data.table::setorder(res, mz)
      
      res$z <- NA_integer_
      
      # Based on the relationship m/z_iZ = m/z_{i+1}(Z-1) giving Z = \frac{-m/z_{i+1}}{m/z_i-m/z_{i+1}}
      for (i in seq_len(nrow(res))) {
        res$z[i] <- round((-res$mz[i + 1]) / (res$mz[i] - res$mz[i + 1]), digits = 1)
      }
      
      res$z <- round(res$z, digits = 0)
      
      res <- res[!is.na(res$z), ]
      
      if (nrow(res) == 0) return(data.table::data.table())
      
      res <- res[-1, ] # removes the first as mass estimation might be affected by incomplete cluster
      
      if (nrow(res) == 0) return(data.table::data.table())
      
      res$mass <- NA_real_
      
      for (i in seq_len(nrow(res))) res$mass[i] <- res$mz[i] * res$z[i]
      
      res$outlier <- FALSE
      
      if (length(seq_len(nrow(res))[-1]) >= 2) {
        
        res$z_step <- 0
        
        for (i in seq_len(nrow(res))[-1]) res$z_step[i - 1] <- res$z[i - 1] - res$z[i]
        
        res$outlier <- res$z_step != 1
        
      }
      
      mass_vec <- res$mass[!res$outlier]
      
      res$outlier[!res$outlier] <- mass_vec < (mean(mass_vec) - sd(mass_vec)) | mass_vec > (mean(mass_vec) + sd(mass_vec))
      
      # plot_charges_annotated <- function(x, res) {
      #   plot(x$mz, x$intensity, type = 'l', ylim = c(0, max(x$intensity) * 1.4))
      #   
      #   no_outlier <- !res$outlier
      #   is_outlier <- res$outlier
      #   
      #   text(
      #     x = res$mz[no_outlier], y = res$intensity[no_outlier], adj = c(-0.1, 0.25),
      #     labels = paste0(round(res$mz[no_outlier], digits = 3), " (+", res$z[no_outlier], ")"),
      #     vfont = NULL, cex = 0.6, col = "darkgreen", font = NULL, srt = 90
      #   )
      #   
      #   if (any(res$outlier)) {
      #     text(
      #       x = res$mz[is_outlier], y = res$intensity[is_outlier], adj = c(-0.1, 0.25),
      #       labels = paste0(round(res$mz[is_outlier], digits = 3), " (+", res$z[is_outlier], ")"),
      #       vfont = NULL, cex = 0.6, col = "darkred", font = NULL, srt = 90
      #     )
      #   }
      # }
      # 
      # if (FALSE) plot_charges_annotated(x, res)
      
      res <- res[!res$outlier, ]
      
      res$outlier <- NULL
      
      res

    } else {
      data.table::data.table()
    }
    
  }, roundVal = roundVal, relLowCut = relLowCut, absLowCut = absLowCut)

  names(charges) <- names(spec_list)
  
  self$spectra_charges <- charges

  message(paste0("\U2713 ", "Spectra charges calculated!"))
  
  TRUE
}
