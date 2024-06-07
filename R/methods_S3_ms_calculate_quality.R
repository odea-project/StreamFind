#' @title .s3_ms_calculate_quality.Settings_calculate_quality_StreamFind
#'
#' @description Calculates quality parameters of features.
#'
#' @noRd
#'
.s3_ms_calculate_quality.Settings_calculate_quality_StreamFind <- function(settings, self, private) {

  if (!any(self$has_features())) {
    warning("Features not found! Not calculated.")
    return(FALSE)
  }
  
  if (!validate(settings)) return(FALSE)

  if (!any(self$has_features())) {
    warning("Features were not found! Quality not calculated.")
    return(FALSE)
  }

  parameters <- settings$parameters

  eic <- self$get_features_eic(
    rtExpand = parameters$rtExpand,
    mzExpand = parameters$mzExpand,
    filtered = parameters$filtered,
    loaded = TRUE
  )

  eic[["group"]] <- NULL

  eic[["polarity"]] <- NULL

  fts <- self$feature_list
  
  if (nrow(eic) > 0) {

    analyses <- self$get_analysis_names()
    
    cache <- .load_chache("calculate_quality", analyses, eic, fts, settings)
    
    if (!is.null(cache$data)) {
      
      quality <- cache$data
      
      check <- vapply(names(quality), function(x, quality, fts) {
        temp_i <- names(quality[[x]])
        temp_f <- fts[[x]]$feature
        s_id <- all(temp_i %in% temp_f)
        all(s_id)
        FALSE
      }, quality = quality, fts = fts, FALSE)
      
      if (!all(check)) quality <- NULL
      
    } else {
      quality <- NULL
    }

    if (is.null(quality)) {
      
      message("\U2699 Calculating features quality from ", length(self$get_analyses()), " analyses...", appendLF = FALSE)

      fts_eics <- lapply(analyses, function(x, eic, fts) {
        eic_t <- eic[eic$analysis %in% x, ]
        eic_t[["analysis"]] <- NULL
        list("a" = x, "e" = eic_t, "f" = fts[[x]])
      }, eic = eic, fts = fts)

      quality <- lapply(fts_eics, function(i) {
        
        ana <- i$a

        ft_df <- as.data.frame(i$f)

        et_df <- as.data.frame(i$e)
        
        results <- rep(list(NULL), nrow(ft_df))

        if (nrow(et_df) == 0) return(results)

        if (nrow(ft_df) == 0) return(results)

        .gaussian <- function(x, A, mu, sigma) {
          A * exp(-(x - mu)^2 / (2 * sigma^2))
        }

        # used for smoothing
        .moving_average <- function(vec, window_size) {
          output <- numeric(length(vec))
          for (z in seq_len(length(vec))) {
            start <- max(1, z - window_size)
            end <- min(length(vec), z + window_size)
            output[z] <- mean(vec[start:end])
          }
          return(output)
        }

        its <- seq_len(nrow(ft_df))
        
        for (it in its) {

          ft <- ft_df[it, ]
          
          if (ft$filtered && !parameters$filtered) next
          
          pk_model <- list()
          pk_model$traces <- 0
          pk_model$noise <- NA_real_
          pk_model$sn <- 0
          pk_model$gaufit <- NA_real_
          pk_model$fwhm <- NA_real_
          pk_model$A <- NA_real_
          pk_model$mu <- NA_real_
          pk_model$sigma <- NA_real_
          pk_model$rt <- NA_real_
          pk_model$original <- NA_real_
          pk_model$corrected <- NA_real_
          pk_model$predicted <- NA_real_
          pk_model$derivative <- NA_real_
          pk_model$model <- NULL
          pk_model$warning <- FALSE

          # message(paste0("\n Processing ", ft$feature, " from ", ana, " ... \n"))

          et <- et_df[et_df$feature %in% ft$feature, ]

          width <- ft$rtmax - ft$rtmin

          pk_ft <- et[et$rt >= (ft$rtmin - width) & et$rt <= (ft$rtmax + width), ]

          if (nrow(pk_ft) < parameters$minTraces) {
            pk_model$traces <- nrow(pk_ft)
            pk_model$warning <- "Not enough traces in EIC!"

            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has not enough EIC traces! \n"))

            next
          }

          traces_not_noise <- nrow(et[et$rt >= ft$rtmin & et$rt <= ft$rtmax, ])

          noise_traces <- nrow(pk_ft) - traces_not_noise

          if (noise_traces == 0) noise_traces <- 2

          noise <- mean(sort(pk_ft$intensity)[seq_len(noise_traces)])

          pk_model$sn <- round(ft$intensity / noise, digits = 1)

          pk_model$noise <- round(noise, digits = 0)

          pk_eic <- et[et$rt >= (ft$rtmin - (width / 4)) & et$rt <= (ft$rtmax + (width / 4)), ]

          pk_eic <- pk_eic[pk_eic$intensity > noise, ]

          pk_model$traces <- nrow(pk_eic)

          id_T <- NA_character_
          ana_T <- NA_character_
          # id_T <- "mz287.12_rt913_f24"
          # ana_T <- "03_tof_ww_is_pos_o3sw_effluent-r003"
          if (ft$feature %in% id_T && ana == ana_T) {
            browser()

            plot(pk_ft$intensity)
            plot(pk_eic$intensity)

            self$plot_features(
              analyses = ana_T,
              features = id_T,
              filtered = TRUE,
              mzExpand = 0.001,
              loaded = FALSE
            )
          }

          if (nrow(pk_eic) < parameters$minTraces) {
            pk_model$warning <- "Not enough traces after noise correction!"

            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: \n", "Not enough traces after noise correction!"))

            next
          }

          pk_max <- which(pk_eic$intensity == max(pk_eic$intensity))

          right_size <- nrow(pk_eic) - pk_max[1]

          left_size <- pk_max - 1
          
          pk_simetry  <- right_size / left_size
          
          if (pk_simetry < 0.5 || pk_simetry > 2) {
            
            if (right_size < left_size && right_size > 2) {
              window_size <- right_size
              
            } else {
              window_size <- left_size
            }
            
            if (window_size + 1 < (parameters$minTraces / 2)) { # when window size is too small gets all traces
              pk_ints <- pk_eic$intensity
              
            } else {
              left_traces <- sort(pk_max - seq_len(window_size))
              right_traces <-  seq_len(window_size) + pk_max
              traces <- c(left_traces, pk_max, right_traces)
              traces <- traces[traces > 0]
              baseline_intensity <- min(pk_eic$intensity[traces], na.rm = TRUE)
              pk_eic <- pk_eic[pk_eic$intensity >= baseline_intensity, ]
              pk_ints <- pk_eic$intensity
            }
            
          } else {
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

          if (sign_changes > 4) {
            window_size <- 2  # Adjust the window size as needed
            corrected_ints <- .moving_average(pk_ints, window_size)

          } else {
            corrected_ints <- pk_ints
          }

          pk_x <- seq_along(corrected_ints)

          pk_model_init <- list(
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
            pk_model$warning <- fit_warning$message

            # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit error! \n"))

            next
          }

          predicted_ints <- .gaussian(pk_x, coef(model)["A"], coef(model)["mu"], coef(model)["sigma"])

          SS_residuals <- sum((corrected_ints - predicted_ints)^2)

          SS_total <- sum((corrected_ints - mean(corrected_ints))^2)

          R_squared <- 1 - (SS_residuals / SS_total)

          fwhm <- 2 * coef(model)[["sigma"]] * sqrt(2 * log(2))

          pk_model$gaufit <- round(R_squared, 4)
          
          pk_model$fwhm <- round(fwhm, digits = 1)
          
          pk_model$A <- round(coef(model)[["A"]], digits = 0)
          
          pk_model$mu <- round(coef(model)[["mu"]], digits = 1)
          
          pk_model$sigma <- round(coef(model)[["sigma"]], digits = 1)
          
          pk_model$rt <- pk_eic$rt
          
          pk_model$original <- pk_ints
          
          pk_model$corrected <- corrected_ints
          
          pk_model$predicted <- predicted_ints
          
          pk_model$derivative <- c(derivative01, NA_real_)
          
          # pk_model$model <- model
          
          # pk_data <- data.table(
          #   "rt" = pk_eic$rt,
          #   "original" = pk_ints,
          #   "corrected" = corrected_ints,
          #   "predicted" = predicted_ints,
          #   "derivative" = c(derivative01, NA_real_)
          # )
          # 
          # pk_model <- list(
          #   "model" = model,
          #   "coefficients" = coef(model),
          #   "data" = pk_data,
          #   "R_square" = R_squared,
          #   "warning" = NA_character_
          # )
          # 
          # if (!is.null(fit_warning)) {
          #   pk_model$warning <- fit_warning$message
          #   results$qlt_warning[it] <- TRUE
          # }

          

          results[[it]] <- pk_model

          # id_T <- NA_character_
          # ana_T <- NA_character_
          # # id_T = "mz326.23_rt959_f132"
          # # ana_T = "03_tof_ww_is_pos_o3sw_effluent-r001"
          # # id_T <- "mz287.12_rt913_f24"
          # # ana_T <- "03_tof_ww_is_pos_o3sw_effluent-r003"
          # if (ft$feature %in% id_T && ana == ana_T) {
          #   browser()
          # 
          #   plotly::plot_ly() %>%
          #     plotly::add_trace(x = pk_ft$rt, y = pk_ft$intensity, name = "all", type = "scatter", mode = "markers", marker = list(color = "gray")) %>%
          #     plotly::add_trace(x = pk_eic$rt, y = pk_ints, name = "used", type = "scatter", mode = "markers", marker = list(color = "black")) %>%
          #     plotly::add_trace(x = pk_eic$rt, y = corrected_ints, name = "corrected", type = "scatter", mode = "markers", marker = list(color = "blue")) %>%
          #     plotly::add_trace(x = pk_eic$rt, y = predicted_ints, type = "scatter", name = "predicted", mode = "lines", line = list(color = "red"))
          # 
          #   self$plot_features(
          #     analyses = ana_T,
          #     features = id_T,
          #     filtered = TRUE,
          #     mzExpand = 0.001,
          #     loaded = FALSE
          #   )
          # }
        }# end for loop
        
        results
      })

      names(quality) <- analyses

      message(" Done!")
      
      if (!is.null(cache$hash)) {
        .save_cache("calculate_quality", quality, cache$hash)
        message("\U1f5ab Calculated features quality parameters cached!")
      }
      
    } else {
      message("\U2139 Calculated features quality parameters loaded from cache!")
    }
    
    if (private$.add_features_column("quality", quality)) {
      
      TRUE
      
    } else {
      FALSE
    }

  } else {
    warning("EIC traces from features not found! Not done.")
    FALSE
  }
}
