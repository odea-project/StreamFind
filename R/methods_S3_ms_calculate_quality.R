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
    
    if (.caches_data() & FALSE) {
      hash <- patRoon::makeHash(analyses, eic, fts, settings)
      quality <- patRoon::loadCacheData("calculate_quality", hash)
      
      if (!is.null(quality)) {
        
        check <- vapply(names(quality),
          function(x, quality, fts) {
            temp_i <- quality[[x]]$feature
            temp_f <- fts[fts$analysis %in% x, ]
            temp_f <- temp_f$feature
            s_length <- length(temp_i) == length(temp_f)
            s_id <- all(temp_i %in% temp_f)
            all(c(s_length, s_id))
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
    
    if (FALSE & parameters$runParallel & length(analyses) > 1 & !cached_analyses) {
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
        
        list("e" = eic_t, "f" = fts_t)
        
      }, eic = eic, fts = fts)
      
      names(fts_eics) <- analyses
      
      i <- NULL
      
      quality <- foreach(i = analyses, .packages = "StreamFind", .export = "fts_eics") %dopar% { 
        
        ft_df <- fts_eics[[i]][["f"]]

        et_df <- fts_eics[[i]][["e"]]
        
        # ft_df <- i[["f"]]
        # 
        # et_df <- i[["e"]]
        
        if (!(is.data.frame(ft_df) && is.data.frame(et_df))) {
          return(NULL)
        }
        
        if (!(nrow(ft_df) > 0 && nrow(et_df) > 0)) {
          return(NULL)
        }
        
        gaussian <- function(x, A, mu, sigma) {
          A * exp(-(x - mu)^2 / (2 * sigma^2))
        }
        
        results <- data.table("feature" = ft_df$feature)
        results$qlt_noise <- NA_real_
        results$qlt_sn <- NA_real_
        results$qlt_gaufit <- NA_real_
        results$qlt_model <- list()
        
        its <- seq_len(nrow(ft_df))
        
        for (it in its) {
          
          ft <- ft_df[it, ]
          
          et <- et_df[et_df$feature %in% ft$feature, ]
          
          pk_ft <- et[et$rt >= ft$rtmin & et$rt <= ft$rtmax, ]
          
          out_ft <- et[et$rt <= ft$rtmin | et$rt >= ft$rtmax, ]
          
          if (nrow(out_ft) > 0) {
            other_ft <- ft_df[
              ft_df$rtmin >= min(out_ft$rt) & ft_df$rtmax < ft$rtmin & ft_df$mzmin >= ft$mzmin & ft_df$mzmax <= ft$mzmax  |
              ft_df$rtmax <= max(out_ft$rt) & ft_df$rtmin > ft$rtmax & ft_df$mzmin >= ft$mzmin & ft_df$mzmax <= ft$mzmax
            ]
            
            if (nrow(other_ft) > 0) {
              for (it2 in seq_len(nrow(other_ft))) {
                out_ft <- out_ft[!(out_ft$rt >= other_ft$rtmin[it2] & out_ft$rt <= other_ft$rtmax[it2]), ]
              }
            }
          }
          
          # estimated sn based on lower ends of the pk
          if (nrow(out_ft) == 0) {
            noise <- mean(sort(pk_ft$intensity)[1:2])
            results$qlt_sn[it] <- round(ft$intensity / noise, digits = 1)
            results$qlt_noise[it] <- round(noise, digits = 0)
            
          # estimated sn from outer traces
          } else {
            noise <- mean(out_ft$intensity)
            results$qlt_sn[it] <- round(ft$intensity / noise, digits = 1)
            results$qlt_noise[it] <- round(noise, digits = 0)
          }
          
          pk_ints <- pk_ft$intensity
          
          pk_x <- seq_along(pk_ints)

          pk_model_init = list(
            "A" = max(pk_ints), #1 / sqrt(2 * pi * sd(pk_ints)^2) but is not a good initial guess
            "mu" = mean(pk_x),
            "sigma" = (max(pk_x) - min(pk_x)) / 4#sd(pk_x) # alternative as a fraction: (max(pk_x) - min(pk_x)) / 4
          )
          
          tryCatch(
            {
              fit <- nls(pk_ints ~ gaussian(pk_x, A, mu, sigma),
                start = pk_model_init,
                control = list(warnOnly = TRUE)
              ) # TODO set trycatch for warning and give more information
            },
            warning = function(warn) {
              warning(paste0("Feature ", ft$feature, " in analysis ", i, " exceeded 50 iterations to converge!"))
              return(fit)
            }
          )
          
          predicted_values <- gaussian(pk_x, coef(fit)["A"], coef(fit)["mu"], coef(fit)["sigma"])
          
          SS_residuals <- sum((pk_ints - predicted_values)^2)
          
          SS_total <- sum((pk_ints - mean(pk_ints))^2)
          
          R_squared <- 1 - (SS_residuals / SS_total)
          
          results$qlt_gaufit[it] <- R_squared
          
          pk_model <- list(
            "model" = fit,
            "coefficients" = coef(fit),
            "predicted_values" = predicted_values,
            "real_values" = pk_ints,
            "R_square" = R_squared
          )
          
          results[["qlt_model"]][it] <- list(pk_model)

          # plotly::plot_ly() %>%
          #   plotly::add_trace(x = pk_x, y = pk_ints, name = 'real', type = 'scatter', mode = 'markers', marker = list(color = "black")) %>%
          #   plotly::add_trace(x = pk_x, y = predicted_values, type = 'scatter', name = 'predicted', mode = 'lines', line = list(color = "red"))
        }
        
        results
      }
      
      names(quality) <- analyses
    }
    
    quality <- rbindlist(quality, idcol = "analysis")
    
    return(quality)
  }
  
}