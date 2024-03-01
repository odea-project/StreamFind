
#' @title .s3_deconvolute_spectra_charges.Settings_deconvolute_spectra_charges_StreamFind
#'
#' @description Deconvolutes spectra charges from multi-charged compounds.
#'
#' @noRd
#'
.s3_deconvolute_spectra_charges.Settings_deconvolute_spectra_charges_StreamFind <- function(settings, self) {
  
  intensity <- NULL
  
  rtmin = 315 - 2.5
  rtmax = 315 + 2.5
  mzmin = 2400
  mzmax = 3800
  presence = 0.1 # presence can be adjusted to remove noise
  mzClust = 0.001 # critical for resolution, in Da
  minIntensity = 50
  roundVal = 35
  lowCut = 0.2
  mzClustAverage = 0.1
  smoothing = TRUE
  windowSize = 25
  baseline = TRUE
  baseline_method = "als"
  baseline_args = list(lambda = 9, p = 0.02, maxit = 10)
  merge = TRUE
  closeByThreshold = 45
  valeyThreshold = 0.5
  minPeakHeight = 100
  minPeakDistance = 50
  maxPeakWidth = 250
  minPeakWidth = 50
  
  plotLevel = 4
  
  xlab = expression(italic("m/z"))
  ylab = "Intensity / counts"
  title = "Charges and mass annotation"

  ms1 <- self$get_ms1(
    mz = data.table("rtmin" = rtmin, "rtmax" = rtmax, "mzmin" = mzmin, "mzmax" = mzmax),
    presence = presence,
    mzClust = mzClust,
    minIntensity = minIntensity
  )
  
  # .plot_spectra_interactive(ms1)
  
  ms1_list <- split(ms1, ms1$analysis)
  
  ms1_list <- lapply(ms1_list, function(x) {
    
    if (nrow(x) == 0) return(data.table())
    
    if ("BVCZ_Blank" %in% unique(x$analysis)) return(list())
    
    x$cluster <- round(x$mz / roundVal) * roundVal
    
    sp <- copy(x)
    setorder(sp, -intensity)
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
    
    setorder(sp2, -mz)
    
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
    
    setorder(sp2, -intensity)
    
    sp2 <- sp2[sp2$intensity / sp2$intensity[1] > lowCut, ]
    
    if (plotLevel > 1) {
      
      plot(x$mz, x$intensity, type = 'l', ylab = ylab, xlab = xlab,
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
      
      abline(h = sp2$intensity[1] * lowCut, col = "darkred")
      
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
    
    res <- x[x$mz == 0, ]
    
    for (i in seq_len(nrow(sp2))) {
      temp <- x[x$cluster == sp2$cluster[i] & x$mz == sp2$mz[i], ]
      res <- rbind(res, temp)
    }
    
    res <- unique(res)
    
    setorder(res, mz)
    
    res$z <- NA_integer_
    
    # Based on the relationship
    #
    # m/z_iZ = m/z_{i+1}(Z-1)
    #
    # giving
    #
    # Z = \frac{-m/z_{i+1}}{m/z_i-m/z_{i+1}}
    
    for (i in seq_len(nrow(res))) {
      res$z[i] <- round((-res$mz[i + 1]) / (res$mz[i] - res$mz[i + 1]), digits = 0)
    }
    
    res <- res[!is.na(res$z), ]
    
    res <- res[-1, ] # removes the first as mass estimation might be affected by not complete cluster
    
    res$mass <- NA_real_
    
    for (i in seq_len(nrow(res))) res$mass[i] <- res$mz[i] * res$z[i]
    
    res_outliers <- res[res$mass < (mean(res$mass) - sd(res$mass)) | res$mass > (mean(res$mass) + sd(res$mass))]
    
    if (nrow(res_outliers) > 0) {
      res <- res[res$cluster != res_outliers$cluster, ]
    }
    
    if (plotLevel > 0) {
      plot(
        x$mz, x$intensity, type = 'l',
        ylab = ylab,
        xlab = xlab,
        main = title,
        ylim = c(0, max(x$intensity) * 1.4)
      )
      
      text(
        x = res$mz, y = res$intensity, adj = c(-0.1, 0.25),
        labels = paste0(round(res$mz, digits = 3), " (+", res$z, ")"),
        vfont = NULL, cex = 0.6, col = "darkgreen", font = NULL, srt = 90
      )
      
      if (nrow(res_outliers) > 0) {
        text(
          x = res_outliers$mz, y = res_outliers$intensity, adj = c(-0.1, 0.25),
          labels = paste0(round(res_outliers$mz, digits = 3), " (+", res_outliers$z, ")"),
          vfont = NULL, cex = 0.6, col = "darkred", font = NULL, srt = 90
        )
      }
    }
    
    profiles <- lapply(seq_len(nrow(res)), function(j) {
      
      window <- (res$mz[j + 1] - res$mz[j]) / 2
      
      sel <- x$mz >= (res$mz[j] - window) & x$mz <= (res$mz[j] + window)
      
      prfl <- x[sel, ]
      
      if (max(res$intensity) == res$intensity[j] & plotLevel > 1) {
        plot(prfl$mz, prfl$intensity, type = 'l',
             xlab = expression(italic("m/z")),
             ylab = ylab,
             main = paste0("Raw profile for charge +", res$z[j])
        )
      }
      
      decon_prfl <- prfl
      
      decon_prfl$mz <- prfl$mz * res$z[j]
      
      if (max(res$intensity) == res$intensity[j] & plotLevel > 1) {
        plot(
          decon_prfl$mz, decon_prfl$intensity, type = 'l',
          xlab = xlab,
          ylab = ylab,
          main = paste0("Deconvoluted bvz spectra for charge +", res$z[j]),
          ylim = c(0, max(decon_prfl$intensity) * 1.4)
        )
      }
      
      decon_prfl
    })
    
    profiles <- profiles[vapply(profiles, function(x) nrow(x) > 0, FALSE)]
    
    profiles_dt <- rbindlist(profiles)
    
    profiles_dt$unique_id <- "var"
    
    av_profile <- rcpp_ms_cluster_spectra(profiles_dt, mzClust = mzClustAverage, presence = 0)[[1]]
    
    av_profile <- as.data.table(av_profile)
    
    setnames(av_profile, "mz", "mass")
    
    if (plotLevel > 1) {
      plot(av_profile$mass, av_profile$intensity, type = 'l', xlab = xlab, ylab = ylab,
        main = paste0("Merged spectra by weighted average with mzClust ", mzClustAverage)
      )
    }
    
    if (smoothing || baseline) av_profile$raw <- av_profile$intensity
    
    if (smoothing) {
      av_profile$smoothed <- .moving_average(av_profile$intensity, windowSize)
      av_profile$intensity <- av_profile$smoothed
    }
    
    
    
    if (plotLevel > 1) {
      plot(av_profile$mass, av_profile$intensity, type = 'l', xlab = xlab, ylab = ylab,
        main = paste0("Smoothed profile")
      )
    }
    
    if (baseline) {
      baseline_out <- .baseline_correction(av_profile$intensity, baseline_method, baseline_args)
      av_profile$baseline <- baseline_out$baseline
      av_profile$corrected <- baseline_out$corrected
      av_profile$intensity <- av_profile$corrected
    }

    setorder(av_profile, "mass")
    
    if (plotLevel > 1) {
      plot(av_profile$mass, av_profile$intensity, type = "l", xlab = xlab, ylab = ylab, main = "Baseline correction")
      if (smoothing) lines(av_profile$mass, av_profile$smoothed, col = "darkgreen")
      if (baseline) lines(av_profile$mass, baseline_out$baseline, col = "darkred")
    }

    pks <- .find_peaks(av_profile, "mass", merge, closeByThreshold, valeyThreshold, minPeakHeight, minPeakDistance, maxPeakWidth, minPeakWidth)

    
    
    
    
    
    
    
    
    
    
    
    
    list(
      "raw" = x,
      "charges" = res,
      "average" = list()
    )
  })
  
  
  
  
  
  ms1_list
  
  
  
  
  
  
}
