
# ______________________________________________________________________________________________________________________
# StreamFind -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_CalculateFeaturesQuality_StreamFind**
#'
#' @description Settings for calculating quality parameters of features (e.g., signal-to-noise (sn) ratio).
#'
#' @template arg-ms-filtered
#' @template arg-ms-rtExpand 
#' @template arg-ms-mzExpand 
#' @param minTraces Numeric of length 1 with the minimum number traces for calculating feature quality.
#' @param minIntensity Numeric of length 1 with the minimum intensity of spectra traces for calculating feature quality.
#' @param baseCut Numeric of length 1 with the base cut for calculating feature Gaussian fit.
#'
#' @return A `MassSpecSettings_CalculateFeaturesQuality_StreamFind` object.
#'
#' @export
#'
MassSpecSettings_CalculateFeaturesQuality_StreamFind <- S7::new_class("MassSpecSettings_CalculateFeaturesQuality_StreamFind",
  parent = ProcessingSettings,
  package = "StreamFind",
  
  constructor = function(filtered = FALSE, rtExpand = 0, mzExpand = 0, minTracesIntensity = 0, minNumberTraces = 6, baseCut = 0) {
    
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "CalculateFeaturesQuality",
      algorithm = "StreamFind",
      parameters = list(
        "filtered" = filtered,
        "rtExpand" = rtExpand,
        "mzExpand" = mzExpand,
        "minTracesIntensity" = minTracesIntensity,
        "minNumberTraces" = minNumberTraces,
        "baseCut" = baseCut
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
    valid <- all(
      checkmate::test_choice(self@engine, "MassSpec"),
      checkmate::test_choice(self@method, "CalculateFeaturesQuality"),
      checkmate::test_choice(self@algorithm, "StreamFind"),
      checkmate::test_logical(self@parameters$filtered, max.len = 1),
      checkmate::test_number(self@parameters$rtExpand),
      checkmate::test_number(self@parameters$mzExpand),
      checkmate::test_integer(as.integer(self@parameters$minNUmberTraces)),
      checkmate::test_number(self@parameters$minTracesIntensity),
      checkmate::test_number(self@parameters$baseCut)
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecSettings_CalculateFeaturesQuality_StreamFind) <- function(x, engine = NULL) {
  
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
  
  if (!nts@has_features) {
    warning("NTS object does not have features! Not done.")
    return(FALSE)
  }
  
  feature_list <- nts$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!"quality" %in% colnames(z)) z$quality <- rep(list(), nrow(z))
    if (!"eic" %in% colnames(z)) z$eic <- rep(list(), nrow(z))
    z
  })
  
  cache <- .load_chache("calculate_quality", feature_list, x)
  
  if (!is.null(cache$data)) {
    feature_list <- cache$data
    tryCatch({
      engine$nts$feature_list <- feature_list
      message("\U2139 Calculated features quality loaded from cache!")
      return(TRUE)
    }, error = function(e) {
      warning(e)
      return(FALSE)
    })
  }
  
  parameters <- x$parameters
  analyses_list <- engine$analyses$analyses
  
  res <- rcpp_ms_calculate_features_quality(
    analyses_list,
    feature_list,
    parameters$filtered,
    parameters$rtExpand,
    parameters$mzExpand,
    parameters$minTracesIntensity,
    parameters$minNumberTraces,
    parameters$baseCut
  )
  
  if (!is.null(cache$hash)) {
    .save_cache("calculate_quality", res, cache$hash)
    message("\U1f5ab Calculated features quality cached!")
  }
  
  tryCatch({
    engine$nts$feature_list <- feature_list
    return(TRUE)
  }, error = function(e) {
    warning(e)
    return(FALSE)
  })

  # parameters <- x$parameters
  # 
  # eic <- engine$get_features_eic(
  #   rtExpand = parameters$rtExpand,
  #   mzExpand = parameters$mzExpand,
  #   filtered = parameters$filtered,
  #   useLoadedData = TRUE
  # )
  # 
  # eic[["group"]] <- NULL
  # 
  # eic[["polarity"]] <- NULL
  # 
  # fts <- nts$feature_list
  # 
  # if (nrow(eic) > 0) {
  #   
  #   analyses <- engine$get_analysis_names()
  #   
  #   cache <- .load_chache("calculate_quality", analyses, eic, fts, x)
  #   
  #   if (!is.null(cache$data)) {
  #     quality <- cache$data
  #     tryCatch({
  #       nts <- .add_features_column(nts, "quality", cache$data)
  #       engine$nts <- nts
  #       message("\U2139 Calculated features quality parameters loaded from cache!")
  #       return(TRUE)
  #     }, error = function(e) {
  #       warning(e)
  #       return(FALSE)
  #     })
  #   }
  #   
  #   message("\U2699 Calculating features quality from ", length(engine$analyses), " analyses...", appendLF = FALSE)
  #   
  #   fts_eics <- lapply(analyses, function(x, eic, fts) {
  #     eic_t <- eic[eic$analysis %in% x, ]
  #     eic_t[["analysis"]] <- NULL
  #     list("a" = x, "e" = eic_t, "f" = fts[[x]])
  #   }, eic = eic, fts = fts)
  #   
  #   quality <- lapply(fts_eics, function(i) {
  #     
  #     ana <- i$a
  #     
  #     ft_df <- as.data.frame(i$f)
  #     
  #     et_df <- as.data.frame(i$e)
  #     
  #     results <- rep(list(NULL), nrow(ft_df))
  #     
  #     if (nrow(et_df) == 0) return(results)
  #     
  #     if (nrow(ft_df) == 0) return(results)
  #     
  #     .gaussian <- function(x, A, mu, sigma) {
  #       A * exp(-(x - mu)^2 / (2 * sigma^2))
  #     }
  #     
  #     # used for smoothing
  #     .moving_average <- function(vec, window_size) {
  #       output <- numeric(length(vec))
  #       for (z in seq_len(length(vec))) {
  #         start <- max(1, z - window_size)
  #         end <- min(length(vec), z + window_size)
  #         output[z] <- mean(vec[start:end])
  #       }
  #       return(output)
  #     }
  #     
  #     its <- seq_len(nrow(ft_df))
  #     
  #     for (it in its) {
  #       
  #       ft <- ft_df[it, ]
  #       
  #       if (ft$filtered && !parameters$filtered) next
  #       
  #       pk_model <- list()
  #       pk_model$traces <- 0
  #       pk_model$noise <- NA_real_
  #       pk_model$sn <- 0
  #       pk_model$gaufit <- NA_real_
  #       pk_model$fwhm <- NA_real_
  #       pk_model$A <- NA_real_
  #       pk_model$mu <- NA_real_
  #       pk_model$sigma <- NA_real_
  #       pk_model$rt <- NA_real_
  #       pk_model$original <- NA_real_
  #       pk_model$corrected <- NA_real_
  #       pk_model$predicted <- NA_real_
  #       pk_model$derivative <- NA_real_
  #       pk_model$model <- NULL
  #       pk_model$warning <- FALSE
  #       
  #       # message(paste0("\n Processing ", ft$feature, " from ", ana, " ... \n"))
  #       
  #       et <- et_df[et_df$feature %in% ft$feature, ]
  #       
  #       width <- ft$rtmax - ft$rtmin
  #       
  #       pk_ft <- et[et$rt >= (ft$rtmin - width) & et$rt <= (ft$rtmax + width), ]
  #       
  #       if (nrow(pk_ft) < parameters$minTraces) {
  #         pk_model$traces <- nrow(pk_ft)
  #         pk_model$warning <- "Not enough traces in EIC!"
  #         
  #         # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has not enough EIC traces! \n"))
  #         
  #         next
  #       }
  #       
  #       traces_not_noise <- nrow(et[et$rt >= ft$rtmin & et$rt <= ft$rtmax, ])
  #       
  #       noise_traces <- nrow(pk_ft) - traces_not_noise
  #       
  #       if (noise_traces == 0) noise_traces <- 2
  #       
  #       noise <- mean(sort(pk_ft$intensity)[seq_len(noise_traces)])
  #       
  #       pk_model$sn <- round(ft$intensity / noise, digits = 1)
  #       
  #       pk_model$noise <- round(noise, digits = 0)
  #       
  #       pk_eic <- et[et$rt >= (ft$rtmin - (width / 4)) & et$rt <= (ft$rtmax + (width / 4)), ]
  #       
  #       pk_eic <- pk_eic[pk_eic$intensity > noise, ]
  #       
  #       pk_model$traces <- nrow(pk_eic)
  #       
  #       id_T <- NA_character_
  #       ana_T <- NA_character_
  #       # id_T <- "mz287.12_rt913_f24"
  #       # ana_T <- "03_tof_ww_is_pos_o3sw_effluent-r003"
  #       if (ft$feature %in% id_T && ana == ana_T) {
  #         browser()
  #         
  #         plot(pk_ft$intensity)
  #         plot(pk_eic$intensity)
  #         
  #         engine$plot_features(
  #           analyses = ana_T,
  #           features = id_T,
  #           filtered = TRUE,
  #           mzExpand = 0.001,
  #           loaded = FALSE
  #         )
  #       }
  #       
  #       if (nrow(pk_eic) < parameters$minTraces) {
  #         pk_model$warning <- "Not enough traces after noise correction!"
  #         
  #         # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: \n", "Not enough traces after noise correction!"))
  #         
  #         next
  #       }
  #       
  #       pk_max <- which(pk_eic$intensity == max(pk_eic$intensity))
  #       
  #       right_size <- nrow(pk_eic) - pk_max[1]
  #       
  #       left_size <- pk_max - 1
  #       
  #       pk_simetry  <- right_size / left_size
  #       
  #       if (pk_simetry < 0.5 || pk_simetry > 2) {
  #         
  #         if (right_size < left_size && right_size > 2) {
  #           window_size <- right_size
  #           
  #         } else {
  #           window_size <- left_size
  #         }
  #         
  #         if (window_size + 1 < (parameters$minTraces / 2)) { # when window size is too small gets all traces
  #           pk_ints <- pk_eic$intensity
  #           
  #         } else {
  #           left_traces <- sort(pk_max - seq_len(window_size))
  #           right_traces <-  seq_len(window_size) + pk_max
  #           traces <- c(left_traces, pk_max, right_traces)
  #           traces <- traces[traces > 0]
  #           baseline_intensity <- min(pk_eic$intensity[traces], na.rm = TRUE)
  #           pk_eic <- pk_eic[pk_eic$intensity >= baseline_intensity, ]
  #           pk_ints <- pk_eic$intensity
  #         }
  #         
  #       } else {
  #         pk_ints <- pk_eic$intensity
  #       }
  #       
  #       derivative01 <- diff(pk_ints)
  #       
  #       sign_changes <- 0
  #       
  #       # Iterate through the vector
  #       for (i in 2:length(derivative01)) {
  #         if (sign(derivative01[i - 1]) != sign(derivative01[i])) {
  #           sign_changes <- sign_changes + 1
  #         }
  #       }
  #       
  #       if (sign_changes > 4) {
  #         window_size <- 2  # Adjust the window size as needed
  #         corrected_ints <- .moving_average(pk_ints, window_size)
  #         
  #       } else {
  #         corrected_ints <- pk_ints
  #       }
  #       
  #       pk_x <- seq_along(corrected_ints)
  #       
  #       pk_model_init <- list(
  #         "A" = max(corrected_ints), #1 / sqrt(2 * pi * sd(pk_ints)^2) but is not a good initial guess
  #         "mu" = mean(pk_x),
  #         "sigma" = sd(pk_x) # alternative as a fraction: (max(pk_x) - min(pk_x)) / 4
  #       )
  #       
  #       fit_warning <- NULL
  #       
  #       predicted_ints <- NULL
  #       
  #       model <- NULL
  #       
  #       model <- withCallingHandlers({
  #         
  #         model <- nls(
  #           corrected_ints ~ .gaussian(pk_x, A, mu, sigma),
  #           start = pk_model_init,
  #           control = list(warnOnly = TRUE)
  #         )
  #         
  #         model
  #         
  #       }, warning = function(w) {
  #         
  #         fit_warning <<- w
  #         
  #         # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit warning: \n", w$message))
  #         
  #         invokeRestart("muffleWarning")
  #         
  #       }, error = function(e) {
  #         
  #         fit_warning <<- e
  #         
  #         # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit error: \n", e$message))
  #       })
  #       
  #       if (is.null(model)) {
  #         pk_model$warning <- fit_warning$message
  #         
  #         # message(paste0("\n Feature ", ft$feature, " in analysis ", ana, " has gaussian fit error! \n"))
  #         
  #         next
  #       }
  #       
  #       predicted_ints <- .gaussian(pk_x, coef(model)["A"], coef(model)["mu"], coef(model)["sigma"])
  #       
  #       SS_residuals <- sum((corrected_ints - predicted_ints)^2)
  #       
  #       SS_total <- sum((corrected_ints - mean(corrected_ints))^2)
  #       
  #       R_squared <- 1 - (SS_residuals / SS_total)
  #       
  #       fwhm <- 2 * coef(model)[["sigma"]] * sqrt(2 * log(2))
  #       
  #       pk_model$gaufit <- round(R_squared, 4)
  #       
  #       pk_model$fwhm <- round(fwhm, digits = 1)
  #       
  #       pk_model$A <- round(coef(model)[["A"]], digits = 0)
  #       
  #       pk_model$mu <- round(coef(model)[["mu"]], digits = 1)
  #       
  #       pk_model$sigma <- round(coef(model)[["sigma"]], digits = 1)
  #       
  #       pk_model$rt <- pk_eic$rt
  #       
  #       pk_model$original <- pk_ints
  #       
  #       pk_model$corrected <- corrected_ints
  #       
  #       pk_model$predicted <- predicted_ints
  #       
  #       pk_model$derivative <- c(derivative01, NA_real_)
  #       
  #       # pk_model$model <- model
  #       
  #       # pk_data <- data.table(
  #       #   "rt" = pk_eic$rt,
  #       #   "original" = pk_ints,
  #       #   "corrected" = corrected_ints,
  #       #   "predicted" = predicted_ints,
  #       #   "derivative" = c(derivative01, NA_real_)
  #       # )
  #       # 
  #       # pk_model <- list(
  #       #   "model" = model,
  #       #   "coefficients" = coef(model),
  #       #   "data" = pk_data,
  #       #   "R_square" = R_squared,
  #       #   "warning" = NA_character_
  #       # )
  #       # 
  #       # if (!is.null(fit_warning)) {
  #       #   pk_model$warning <- fit_warning$message
  #       #   results$qlt_warning[it] <- TRUE
  #       # }
  #       
  #       
  #       
  #       results[[it]] <- pk_model
  #       
  #       # id_T <- NA_character_
  #       # ana_T <- NA_character_
  #       # # id_T = "mz326.23_rt959_f132"
  #       # # ana_T = "03_tof_ww_is_pos_o3sw_effluent-r001"
  #       # # id_T <- "mz287.12_rt913_f24"
  #       # # ana_T <- "03_tof_ww_is_pos_o3sw_effluent-r003"
  #       # if (ft$feature %in% id_T && ana == ana_T) {
  #       #   browser()
  #       # 
  #       #   plotly::plot_ly() %>%
  #       #     plotly::add_trace(x = pk_ft$rt, y = pk_ft$intensity, name = "all", type = "scatter", mode = "markers", marker = list(color = "gray")) %>%
  #       #     plotly::add_trace(x = pk_eic$rt, y = pk_ints, name = "used", type = "scatter", mode = "markers", marker = list(color = "black")) %>%
  #       #     plotly::add_trace(x = pk_eic$rt, y = corrected_ints, name = "corrected", type = "scatter", mode = "markers", marker = list(color = "blue")) %>%
  #       #     plotly::add_trace(x = pk_eic$rt, y = predicted_ints, type = "scatter", name = "predicted", mode = "lines", line = list(color = "red"))
  #       # 
  #       #   engine$plot_features(
  #       #     analyses = ana_T,
  #       #     features = id_T,
  #       #     filtered = TRUE,
  #       #     mzExpand = 0.001,
  #       #     loaded = FALSE
  #       #   )
  #       # }
  #     }# end for loop
  #     
  #     results
  #   })
  #   
  #   names(quality) <- analyses
  #   
  #   message(" Done!")
  #   
  #   if (!is.null(cache$hash)) {
  #     .save_cache("calculate_quality", quality, cache$hash)
  #     message("\U1f5ab Calculated features quality parameters cached!")
  #   }
  #   
  #   nts <- .add_features_column(nts, "quality", quality)
  #   engine$nts <- nts
  #   TRUE
  #   
  # } else {
  #   warning("EIC traces from features not found! Not done.")
  #   FALSE
  # }
}
