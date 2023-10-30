#' @title .s3_ms_calculate_quality.Settings_calculate_quality_StreamFind
#'
#' @description Calculates quality parameters of features.
#'
#' @noRd
#'
.s3_ms_calculate_quality.Settings_calculate_quality_StreamFind <- function(settings, self) {
  
  if (!validate(settings)) return(FALSE)
  
  if (!any(self$has_features())) {
    warning("Features were not found! Quality not calculated.")
    return(invisible())
  }
  
  parameters <- settings$parameters
  
  eic <- self$get_features_eic(
    rtExpand = 120,
    mzExpand = 0,
    filtered = parameters$filtered,
    loaded = TRUE
  )
  
  eic[["group"]] <- NULL
  
  eic[["polarity"]] <- NULL
  
  fts <- self$get_features(filtered = parameters$filtered)
  
  fts <- fts[, c("analysis", "feature", "rt", "rtmin", "rtmax", "mz", "mzmin", "mzmax", "intensity"), with = FALSE]
  
  if (nrow(eic) > 0) {
    
    analyses <- unique(eic$analysis)
    
    cached_analyses <- FALSE
    
    if (.caches_data()) {
      hash <- patRoon::makeHash(analyses, eic, fts, settings)
      quality <- patRoon::loadCacheData("calculate_quality", hash)
      
      if (!is.null(quality)) {
        check <- vapply(names(quality),
          function(x, quality, fts) {
            temp_i <- quality[[x]]$feature
            temp_f <- fts[fts$analysis %in% x, ]
            temp_f <- temp_f$feature
            s_id <- all(temp_i %in% temp_f)
            all(s_id)
          },
          quality = quality,
          fts = fts,
          FALSE
        )
        
        if (all(check)) {
          cached_analyses <- TRUE
        } else {
          quality <- NULL
        }
      }
    } else {
      hash <- NULL
      quality <- NULL
    }
    
    if (parameters$runParallel & length(analyses) > 1 & !cached_analyses) {
      workers <- parallel::detectCores() - 1
      if (length(analyses) < workers) workers <- length(analyses)
      par_type <- "PSOCK"
      if (parallelly::supportsMulticore()) par_type <- "FORK"
      cl <- parallel::makeCluster(workers, type = par_type)
      doParallel::registerDoParallel(cl)
    } else {
      registerDoSEQ()
    }
    
    if (!cached_analyses) {
      message("\U2699 Calculating features quality from ",
        length(self$get_analyses()),
        " analyses...",
        appendLF = FALSE
      )
      
      fts_eics <- lapply(analyses, function(x, eic, fts) {
        
        eic_t <- eic[eic$analysis %in% x, ]
        eic_t[["analysis"]] <- NULL
        
        fts_t <- fts[fts$analysis %in% x, ]
        fts_t[["analysis"]] <- NULL
        
        list("a" = x, "e" = eic_t, "f" = fts_t)
        
      }, eic = eic, fts = fts)
      
      i <- NULL
      
      quality <- foreach(i = fts_eics, .packages = c("data.table", "StreamFind")) %dopar% {
        
        ana <- i$a
        
        ft_df <- as.data.table(i$f)
        
        et_df <- as.data.table(i$e)
        
        if (!(is.data.frame(ft_df) && is.data.frame(et_df))) {
          return(NULL)
        }
        
        if (!(nrow(ft_df) > 0 && nrow(et_df) > 0)) {
          return(NULL)
        }
        
        .gaussian <- function(x, A, mu, sigma) {
          A * exp(-(x - mu)^2 / (2 * sigma^2))
        }
        
        # used for smoothing
        .moving_average <- function(vec, window_size) {
          output <- numeric(length(vec))
          for (z in 1:length(vec)) {
            start <- max(1, z - window_size)
            end <- min(length(vec), z + window_size)
            output[z] <- mean(vec[start:end])
          }
          return(output)
        }
        
        results <- data.table("analysis" = ana, "feature" = ft_df$feature)
        
        results$qlt_noise <- NA_real_
        results$qlt_sn <- NA_real_
        results$qlt_traces <- 0
        results$qlt_gaufit <- NA_real_
        results$qlt_fwhm <- NA_real_
        results$qlt_A <- NA_real_
        results$qlt_mu <- NA_real_
        results$qlt_sigma <- NA_real_
        results$qlt_warning <- FALSE
        results$qlt_model <- list(NULL)
        
        its <- seq_len(nrow(ft_df))
        
        for (it in its) {
          
          ft <- ft_df[it, ]
          
          # message(paste0("\n Processing ", ft$feature, " from ", ana, " ... \n"))
          
          et <- et_df[et_df$feature %in% ft$feature, ]
          
          width <- ft$rtmax - ft$rtmin
          
          pk_ft <- et[et$rt >= ft$rtmin - width & et$rt <= ft$rtmax + width, ]
          
          if (nrow(pk_ft) < 4) {
            
            results$qlt_gaufit[it] <- 0
            
            results[["qlt_model"]][it] <- list(warning = "Not enough traces in EIC!")
            
            results$qlt_warning[it] <- TRUE
            
            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has not enough EIC traces! \n"))
            
            next
          }
          
          traces_not_noise <- nrow(et[et$rt >= ft$rtmin & et$rt <= ft$rtmax, ])
          
          noise_traces <- nrow(pk_ft) - traces_not_noise
          
          if (noise_traces == 0) noise_traces <- 2
          
          noise <- mean(sort(pk_ft$intensity)[seq_len(noise_traces)])
          
          results$qlt_sn[it] <- round(ft$intensity / noise, digits = 1)
          
          results$qlt_noise[it] <- round(noise, digits = 0)
          
          pk_eic <- et[et$rt >= ft$rtmin - width / 4 & et$rt <= ft$rtmax + width / 4, ]

          pk_eic <- pk_eic[pk_eic$intensity >= noise, ]
          
          results$qlt_traces[it] <- nrow(pk_eic)
          
          id_T <- NA_character_
          ana_T <- NA_character_
          # id_T <- "mz279.1_rt1244_f524"
          # ana_T <- "02_tof_ww_is_neg_influent-r001"
          if (ft$feature %in% id_T & ana == ana_T) {
            browser()
            
            plot(pk_ft$intensity)
            plot(pk_eic$intensity)
            
            self$plot_features(
              analyses = ana_T,
              features = id_T,
              filtered = TRUE,
              mzExpand = 0.001,
              loaded = F
            )
          }
          
          if (nrow(pk_eic) < 4) {
            
            results$qlt_gaufit[it] <- 0
            
            results[["qlt_model"]][it] <- list(
              warning = "Not enough traces after noise correction!"
            )
            
            results$qlt_warning[it] <- TRUE
            
            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: \n", "Not enough traces after noise correction!"))
            
            next
          }
          
          pk_max <- which(pk_eic$intensity == max(pk_eic$intensity))

          right_size <- nrow(pk_eic) - pk_max
          
          left_size <- pk_max - 1

          if (right_size < left_size & right_size > 2) {
            window_size <- right_size
            
          } else {
            window_size <- left_size
          }
          
          if (window_size < 2) { # when window size is too small gets all traces
            pk_ints <- pk_eic$intensity
            
          } else {
            left_traces <- seq_len(window_size) + (pk_max - window_size)
            right_traces <-  seq_len(window_size) + pk_max
            traces <- c(left_traces, pk_max, right_traces)
            traces <- traces[traces > 0]
            pk_eic <- pk_eic[traces, ]
            pk_eic <- pk_eic[!is.na(pk_eic$intensity), ]
            pk_ints <- pk_eic$intensity
          }
          
          derivative01 <- diff(pk_ints)
          
          sign_changes <- 0
          
          # Iterate through the vector
          for (i in 2:length(derivative01)) {
            if (sign(derivative01[i - 1]) != sign(derivative01[i])) {
              sign_changes <- sign_changes + 1
            }
          }
          
          if (sign_changes > 3) {
            window_size <- 2  # Adjust the window size as needed
            corrected_ints <- .moving_average(pk_ints, window_size)
            
          } else {
            corrected_ints <- pk_ints
          }

          pk_x <- seq_along(corrected_ints)
          
          pk_model_init = list(
            "A" = max(corrected_ints), #1 / sqrt(2 * pi * sd(pk_ints)^2) but is not a good initial guess
            "mu" = mean(pk_x),
            "sigma" = sd(pk_x) # alternative as a fraction: (max(pk_x) - min(pk_x)) / 4
          )
          
          fit_warning <- NULL
          
          predicted_ints <- NULL
          
          model <- NULL

          model <- withCallingHandlers({
            
            model <- nls(
              corrected_ints ~ .gaussian(pk_x, A, mu, sigma),
              start = pk_model_init,
              control = list(warnOnly = TRUE)
            )

            model

          }, warning = function(w) {
            
            fit_warning <<- w
            
            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: \n", w$message))
            
            invokeRestart("muffleWarning")
            
          }, error = function(e) {
            
            fit_warning <<- e
            
            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit error: \n", e$message))
          })
          
          if (is.null(model)) {
            
            results$qlt_gaufit[it] <- 0
            
            results[["qlt_model"]][it] <- list(
              warning = fit_warning$message
            )
            
            results$qlt_warning[it] <- TRUE
            
            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit error! \n"))
            
            next
          }

          predicted_ints <- .gaussian(pk_x, coef(model)["A"], coef(model)["mu"], coef(model)["sigma"])

          SS_residuals <- sum((corrected_ints - predicted_ints)^2)
          
          SS_total <- sum((corrected_ints - mean(corrected_ints))^2)
          
          R_squared <- 1 - (SS_residuals / SS_total)
          
          fwhm <- 2 * coef(model)["sigma"] * sqrt(2 * log(2))
          
          pk_data <- data.table(
            "rt" = pk_eic$rt,
            "original" = pk_ints,
            "corrected" = corrected_ints,
            "predicted" = predicted_ints,
            "derivative" = c(derivative01, NA_real_)
          )
          
          pk_model <- list(
            "model" = model,
            "coefficients" = coef(model),
            "data" = pk_data,
            "R_square" = R_squared,
            "warning" = NA_character_
          )
          
          if (!is.null(fit_warning)) {
            pk_model$warning <- fit_warning$message
            results$qlt_warning[it] <- TRUE
          }
          
          results$qlt_gaufit[it] <- R_squared
          
          results$qlt_fwhm[it] <- round(fwhm, digits = 1)
          
          results$qlt_A[it] <- round(coef(model)["A"], digits = 0)
          
          results$qlt_mu[it] <- round(coef(model)["mu"], digits = 1)
          
          results$qlt_sigma[it] <- round(coef(model)["sigma"], digits = 1)
          
          results[["qlt_model"]][it] <- list(pk_model)
          
          id_T <- NA_character_
          ana_T <- NA_character_
          # id_T = "mz268.19_rt916_f50"
          # ana_T = "02_tof_ww_is_pos_influent-r003"
          # id_T = "mz279.12_rt1007_f279"
          # ana_T = "02_tof_ww_is_neg_influent-r002"
          if (ft$feature %in% id_T & ana == ana_T) {
            browser()
            
            plotly::plot_ly() %>%
              plotly::add_trace(x = pk_ft$rt, y = pk_ft$intensity, name = 'all', type = 'scatter', mode = 'markers', marker = list(color = "gray")) %>%
              plotly::add_trace(x = pk_eic$rt, y = pk_ints, name = 'used', type = 'scatter', mode = 'markers', marker = list(color = "black")) %>%
              plotly::add_trace(x = pk_eic$rt, y = corrected_ints, name = 'corrected', type = 'scatter', mode = 'markers', marker = list(color = "blue")) %>%
              plotly::add_trace(x = pk_eic$rt, y = predicted_ints, type = 'scatter', name = 'predicted', mode = 'lines', line = list(color = "red"))
            
            self$plot_features(
              analyses = ana_T,
              features = id_T,
              filtered = TRUE,
              mzExpand = 0.001,
              loaded = F
            )
          }
        }
        results
      }
      
      if (parameters$runParallel & length(analyses) > 1 & !cached_analyses) {
        stopCluster(cl)
      }
      
      names(quality) <- analyses
      
      message(" Done!")
      
      if (!is.null(hash)) {
        patRoon::saveCacheData("calculate_quality", quality, hash)
        message("\U1f5ab Calculated features quality parameters cached!")
      }
    } else {
      message("\U2139 Calculated features quality parameters loaded from cache!")
    }

    quality <- rbindlist(quality, fill = TRUE)
    
    cols_quality <- colnames(quality)
    cols_quality <- cols_quality[!cols_quality %in% c("feature", "analysis")]
    
    features <- self$get_features(filtered = TRUE)
    
    cols_features <- colnames(features)
    cols_features <- cols_features[!cols_features %in% cols_quality]
    
    features <- features[, cols_features, with = FALSE]
    
    features_w_quality <- merge(features, quality, all.x = TRUE)
    
    self$add_features(features_w_quality, replace = TRUE)
    
    TRUE
    
  } else {
    warning("EIC traces from features not found! Not done.")
    
    FALSE
  }
}

# .fit_model <- function(corrected_ints, pk_model_init) {
#   tryCatch(
#     {
#       model <- nls(corrected_ints ~ gaussian(pk_x, A, mu, sigma),
#         start = pk_model_init,
#         control = list(warnOnly = TRUE)
#       )
#     },
#     warning = function(w) {
#       warning(paste0("Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: ", conditionMessage(w)))
#       fit_warning <<- TRUE
#       
#       model <- nls(corrected_ints ~ gaussian(pk_x, A, mu, sigma),
#         start = pk_model_init,
#         control = list(warnOnly = TRUE)
#       )
#       
#       return(model)
#     }
#   )
# }
# 
# model <- .fit_model(corrected_ints, pk_model_init)

# baseline_correction <- function(pk_ints, window_size) {
#   smoothed <- filter(pk_ints, rep(1 / window_size, window_size), sides = 2)
#   # baseline_corrected <- y - smoothed
#   return(smoothed)
# }
# window_size <- 3
# pk_ints <- baseline_correction(pk_ints, window_size)
# pk_ints <- pk_ints[!is.na(pk_ints)]
# plot(pk_ints)

# bc <- baseline::baseline(matrix(pk_ints, nrow = 1), method = "als")
# plot(baseline::getBaseline(bc)[1, ])
# plot(pk_ints - baseline::getBaseline(bc)[1, ])
# pk_ints <- pk_ints - baseline::getBaseline(bc)[1, ]

# degree <- 4
# poly_formula <- as.formula(paste("pk_ints ~ poly(pk_x,", degree, ")"))
# model <- lm(poly_formula, data = data.frame(pk_x, pk_ints))
# predicted_values <- predict(model, newdata = data.frame(pk_x))

#summary_model <- summary(model)

# half_width <- (ft$rtmax - ft$rtmin) / 2 # adds slight extra width to the feature
# 
# out_ft <- et[et$rt <= ft$rtmin & et$rt >= ft$rtmin - half_width | et$rt >= ft$rtmax & et$rt <= ft$rtmax + half_width, ]
# 
# if (nrow(out_ft) > 0) {
#   other_ft <- ft_df_all[
#     ft_df_all$rtmax >= min(out_ft$rt) & ft_df_all$mz >= ft$mzmin & ft_df_all$mz <= ft$mzmax  |
#     ft_df_all$rtmin <= max(out_ft$rt) & ft_df_all$mz >= ft$mzmin & ft_df_all$mz <= ft$mzmax
#   ]
#   
#   if (nrow(other_ft) > 0) {
#     for (it2 in seq_len(nrow(other_ft))) {
#       out_ft <- out_ft[!(out_ft$rt >= other_ft$rtmin[it2] & out_ft$rt <= other_ft$rtmax[it2]), ]
#     }
#   }
# }
# #estimated sn based on lower ends of the pk
# if (TRUE | nrow(out_ft) == 0) {
#   noise <- mean(sort(pk_ft$intensity)[seq_len(nrow(pk_ft) - nrow(pk_eic))])
#   results$qlt_sn[it] <- round(ft$intensity / noise, digits = 1)
#   results$qlt_noise[it] <- round(noise, digits = 0)
#   
# #estimated sn from outer traces
# } else {
#   noise <- quantile(out_ft$intensity, probs = seq(0, 1, 0.25))[3]
#   # noise <- mean(out_ft$intensity)
#   results$qlt_sn[it] <- round(ft$intensity / noise, digits = 1)
#   results$qlt_noise[it] <- round(noise, digits = 0)
# }
