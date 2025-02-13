#' **MassSpecMethod_CalculateSpectraCharges_StreamFind**
#'
#' @description Calculates spectral charges from multi-charged compounds (e.g. proteins and
#' monoclonal antibodies) for mass deconvolution.
#' 
#' @param onlyTopScans Logical (length 1) to consider only the *n* `TopScans` with the highest 
#' intensity.
#' @param topScans Numeric (length 1) with the number of top scans to be considered.
#' @param roundVal Numeric (length 1) with the rounding value for the m/z values before applying
#' charge clustering.
#' @param relLowCut Numeric (length 1) with the relative low cut for the charge clustering.
#' @param absLowCut Numeric (length 1) with the absolute low cut for the charge clustering.
#' @param top Numeric (length 1) with the number of top charges to be considered.
#'
#' @return A MassSpecMethod_CalculateSpectraCharges_StreamFind object.
#'
#' @export
#'
MassSpecMethod_CalculateSpectraCharges_StreamFind <- S7::new_class(
  name = "MassSpecMethod_CalculateSpectraCharges_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  
  constructor = function(onlyTopScans = FALSE,
                         topScans = 1,
                         roundVal = 35,
                         relLowCut = 0.2,
                         absLowCut = 300,
                         top = 5) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "CalculateSpectraCharges",
        required = "LoadSpectra",
        algorithm = "StreamFind",
        parameters = list(
          onlyTopScans = as.logical(onlyTopScans),
          topScans = as.numeric(topScans),
          roundVal = as.numeric(roundVal),
          relLowCut = as.numeric(relLowCut),
          absLowCut = as.numeric(absLowCut),
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
    checkmate::assert_logical(self@parameters$onlyTopScans, max.len = 1)
    checkmate::assert_numeric(self@parameters$topScans, max.len = 1)
    checkmate::assert_number(self@parameters$roundVal)
    checkmate::assert_number(self@parameters$relLowCut)
    checkmate::assert_number(self@parameters$absLowCut)
    checkmate::assert_integer(self@parameters$top)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_CalculateSpectraCharges_StreamFind) <- function(x, engine = NULL) {
  
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }
  
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  
  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }
  
  spec_list <- engine$Spectra$spectra
  
  onlyTopScans <- x$parameters$onlyTopScans
  topScans <- x$parameters$topScans
  parameters <- x$parameters
  roundVal <- parameters$roundVal
  relLowCut <- parameters$relLowCut
  absLowCut <- parameters$absLowCut
  top_charges <- parameters$top
  
  number_analyses <- length(spec_list)
  processed <- 1
  
  charges <- lapply(spec_list, function(z,
                                        roundVal,
                                        relLowCut,
                                        absLowCut,
                                        top_charges,
                                        onlyTopScans) {
    
    mz <- NULL
    . <- NULL
    
    if (nrow(z) > 0) {
      
      intensity <- NULL
      intensity_low <- NULL
      intensity_high <- NULL
      mz_low <- NULL
      mz_high <- NULL
      cluster <- NULL
      
      if (onlyTopScans)  {
        z <- z[order(z$intensity, decreasing = TRUE), ]
        highest_intensity_scans <- unique(z$rt)
        if (topScans > 1) {
          highest_intensity_scan <- highest_intensity_scans[1:topScans]
        } else {
          highest_intensity_scan <- highest_intensity_scans[1]
        }
        z <- z[z$rt %in% highest_intensity_scan, ]
      }
      
      z$mz <- round(z$mz, digits = 4)
      cols_merge <- c("mz", "polarity")
      if ("id" %in% colnames(z)) cols_merge <- c(cols_merge, "id")
      z <- z[, .(rt = mean(rt), mz = mean(mz), intensity = sum(intensity)), by = cols_merge]
      z <- unique(z)
      z <- z[order(mz), ]
      z$cluster <- round(z$mz / roundVal) * roundVal
      
      sp <- data.table::copy(z)
      sp$mz_low <- NA_real_
      sp$mz_high <- NA_real_
      sp$intensity_low <- NA_real_
      sp$intensity_high <- NA_real_
      sp$mz_at_max <- NA_real_
      
      clusters <- unique(sp$cluster)
      
      for (i in seq_len(length(clusters))) {
        temp <- sp[sp$cluster == clusters[i], ]
        sn <- max(temp$intensity) / min(temp$intensity)
        if (sn < 15) {
          temp$intensity <- .moving_average(temp$intensity, 5)
        }
        if (FALSE) {
          plot(temp$mz, temp$intensity, type = 'l')
        }
        sp$intensity_low[sp$cluster == clusters[i]] <- min(temp$intensity)
        sp$intensity_high[sp$cluster == clusters[i]] <- max(temp$intensity)
        sp$intensity[sp$cluster == clusters[i]] <- temp$intensity
        mzMax <- temp$mz[temp$intensity == max(temp$intensity)]
        sp$mz_at_max[sp$cluster == clusters[i]] <- mzMax[1]
        sp$mz_low[sp$cluster == clusters[i]] <- mzMax[1] - roundVal
        sp$mz_high[sp$cluster == clusters[i]] <- mzMax[1] + roundVal
      }
      
      sp2 <- sp[, .(
        intensity = max(intensity),
        intensity_low = min(intensity_low),
        intensity_high = max(intensity_high),
        mz_low = min(mz_low),
        mz_high = max(mz_high)
      ), by = c("mz_at_max", "cluster")]
      data.table::setnames(sp2, "mz_at_max", "mz")
      data.table::setorder(sp2, -mz)
      
      # merge clusters with overlapping mz values
      for (i in seq_len(nrow(sp2))) {
        if (is.na(sp2$mz[i + 1])) next
        if (sp2$mz[i] - sp2$mz[i + 1] < roundVal) {
          if (sp2$intensity[i] >= sp2$intensity[i + 1]) {
            sel <- sp2$mz %in% sp2$mz[i + 1]
            sp2$mz[sel] <- sp2$mz[i]
            sp2$mz_low[sel] <- sp2$mz_low[i]
            sp2$mz_high[sel] <- sp2$mz_high[i]
            sp2$cluster[sel] <- sp2$cluster[i]
            sp2$intensity[sel] <- sp2$intensity[i]
            sp2$intensity_low[sel] <- sp2$intensity_low[i]
            sp2$intensity_high[sel] <- sp2$intensity_high[i]
          } else {
            sel <- sp2$mz %in% sp2$mz[i]
            sp2$mz[sel] <- sp2$mz[i + 1]
            sp2$mz_low[sel] <- sp2$mz_low[i + 1]
            sp2$mz_high[sel] <- sp2$mz_high[i + 1]
            sp2$cluster[sel] <- sp2$cluster[i + 1]
            sp2$intensity[sel] <- sp2$intensity[i + 1]
            sp2$intensity_low[sel] <- sp2$intensity_low[i + 1]
            sp2$intensity_high[sel] <- sp2$intensity_high[i + 1]
          }
        }
      }
      sp2 <- unique(sp2)
      
      data.table::setorder(sp2, -intensity)
      
      if (!is.null(absLowCut) && absLowCut > 0) {
        sp2 <- sp2[sp2$intensity > absLowCut, ]
      } else {
        absLowCut <- max(sp2$intensity) * relLowCut
        sp2 <- sp2[sp2$intensity > absLowCut, ]
      }
      if (nrow(sp2) < 3) {
        warning("Not enough clusters in analysis ", processed, " to calculate charges!")
        return(data.table::data.table())
      }
      
      plot_charges_temp <- function(sp,
                                    sp2,
                                    absLowCut,
                                    highest_intensity_scan) {
        
        sel <- sp$rt %in% highest_intensity_scan
        
        plot(
          sp$mz[sel], sp$intensity[sel], type = 'l',
          main = "Clusters, overlap window and low intensity threshold (lowCut)"
        )

        belowLowCut <- !sp$cluster %in% sp2$cluster
        belowLowCut <- sp[belowLowCut, ]

        rect(
          belowLowCut$mz_low,
          belowLowCut$intensity_low,
          belowLowCut$mz_high,
          belowLowCut$intensity_high,
          border = "darkred",
          lty = 2,
          lwd = 0.8,
          angle = 30
        )
        
        abline(h = absLowCut, col = "darkred")

        text(
          sp2$mz,
          sp2$intensity,
          labels = seq_len(nrow(sp2)),
          adj = c(0.2, 0),
          col = "darkgreen"
        )

        rect(
          sp2$mz_low,
          sp2$intensity_low,
          sp2$mz_high,
          sp2$intensity,
          border = "darkgreen",
          lty = 1,
          lwd = 1,
          angle = 30
        )

        legend(
          x = "topright",
          legend = c("lowCut", "Excluded clusters", "Final clusters & window"),
          col = c("darkred", "darkred", "darkgreen"),
          lwd = 2,
          lty = c(1, 2, 1),
          cex = 0.7,
          bty = "n"
        )
      }

      if (FALSE) {
        plot_charges_temp(sp, sp2, absLowCut, highest_intensity_scan)
      }
      
      plot_clusters <- function(sp, sp2, clusters = 1) {
        ylim = c(0, max(sp$intensity) * 1.1)
        sp_list  <- lapply(clusters, function(k, sp2, sp) {
          sel_first <- sp$mz >= sp2$mz_low[k]
          sel_first <- sel_first & sp$mz <= sp2$mz_high[k]
          temp <- sp[sel_first, ]
          temp <- temp[order(mz), ]
          temp
        }, sp2 = sp2, sp = sp)
        sp_lengths <- sapply(sp_list, nrow)
        xlim = c(1, max(sp_lengths))
        plot(sp_list[[1]]$intensity, type = 'l', col = 1, xlim = xlim, ylim = ylim)
        if (length(sp_list) > 1) {
          for (i in 2:length(sp_list)) {
            lines(sp_list[[i]]$intensity, col = i)
          }
        }
        legend(
          x = "topright",
          legend = sp2$cluster[clusters],
          col = 1:length(clusters),
          lwd = 2,
          lty = 1,
          cex = 0.7,
          bty = "n"
        )
      }
      
      if (FALSE) {
        plot_clusters(sp, sp2, clusters = c(18, 17, 16))
      }
      
      top_3_mz <- sp2$mz[1:3]
      
      res <- z[z$mz == 0, ]
      res$mz_low <- numeric()
      res$mz_high <- numeric()
      data.table::setorder(sp2, mz)
      for (i in seq_len(nrow(sp2))) {
        temp <- z[z$mz >= sp2$mz_low[i] & z$mz <= sp2$mz_high[i], ]
        max_intensity <- max(temp$intensity)
        temp <- temp[temp$mz == sp2$mz[i], ]
        temp$intensity <- max_intensity
        temp$mz_low <- sp2$mz_low[i]
        temp$mz_high <- sp2$mz_high[i]
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
      if (nrow(res) < 3) {
        warning("Not enough clusters in analysis ", processed, " to calculate charges!")
        return(data.table::data.table())
      }
      
      find_outliers <- function(res, main_cluster_position, main_mz_charge) {
        expected_z <- rep(NA_integer_, nrow(res))
        expected_z[main_cluster_position] <- main_mz_charge
        clusters_left_side <- seq_len(main_cluster_position - 1)
        clusters_right_side <- main_cluster_position + seq_len(nrow(res) - main_cluster_position)
        if (length(clusters_left_side) != 0) {
          counter <- 1
          for (i in rev(clusters_left_side)) {
            expected_z[i] <- main_mz_charge + counter
            counter <- counter + 1
          }
        }
        if (length(clusters_right_side) != 0) {
          counter <- 1
          for (i in clusters_right_side) {
            expected_z[i] <- main_mz_charge - counter
            counter <- counter + 1
          }
        }
        res$expected_z <- expected_z
        for (i in seq_len(nrow(res))) {
          if (res$z_left[i] %in% res$expected_z[i]) {
            res$z[i] <- res$z_left[i]
          } else if (res$z_right[i] %in% res$expected_z[i]) {
            res$z[i] <- res$z_right[i]
          } else {
            res$z[i] <- NA_real_
          }
        }
        res$outlier <- is.na(res$z)
        res
      }
      
      for (main_mz in top_3_mz) {
        sel_main_mz <- res$mz %in% main_mz
        main_cluster_position <- which(sel_main_mz)
        main_charge_right <- res$z_right[sel_main_mz]
        main_charge_left <- res$z_left[sel_main_mz]
        
        if (main_charge_right != main_charge_left) {
          res_left <- find_outliers(res, main_cluster_position, main_charge_left)
          res_right <- find_outliers(res, main_cluster_position, main_charge_right)
          if (sum(res_left$outlier) < sum(res_right$outlier)) {
            res_temp <- res_left
          } else {
            res_temp <- res_right
          }
        } else {
          res_temp <- find_outliers(res, main_cluster_position, main_charge_right)
        }
        res_temp$outlier <- is.na(res_temp$z)
        
        number_NA_z <- sum(is.na(res_temp$z))
        
        if (number_NA_z < sum(is.na(res$z))) {
          res <- res_temp
        }
      }
      
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
      
      if (FALSE) {
        plot_charges_annotated(z, res)
      }
      
      res <- res[!res$outlier, ]
      res <- res[order(res$intensity, decreasing = TRUE), ]
      if (nrow(res) > top_charges) res <- res[1:top_charges, ]
      res$mass <- NA_real_
      
      calculate_mass <- function(mz, z, H = 1.007276) {
        M <- z * (mz - H)
        return(M)
      }
      
      for (i in seq_len(nrow(res))) res$mass[i] <- calculate_mass(res$mz[i], res$z[i])
      
      res <- res[,
        c("id", "polarity", "rt", "mz", "intensity", "cluster", "z", "mass"),
        with = FALSE
      ]
      
      message(paste0("\U2713 ", "Processed ", processed, " of ", number_analyses, " analyses!"))
      processed <<- processed + 1
      list("spectra" = z, "charges" = res)
    } else {
      warning("No data in analysis ", processed, " to calculate charges!")
      processed <<- processed + 1
      list("spectra" = z, "charges" = data.table::data.table())
    }
  },
  roundVal = roundVal,
  relLowCut = relLowCut,
  absLowCut = absLowCut,
  top_charges = top_charges,
  onlyTopScans = onlyTopScans
  )
  
  names(charges) <- names(spec_list)
  engine$Spectra$spectra <- lapply(charges, function(z) z$spectra)
  engine$Spectra$charges <- lapply(charges, function(z) z$charges)
  message(paste0("\U2713 ", "Spectra charges calculated!"))
  TRUE
}
