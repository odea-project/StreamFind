#' @export
#' @noRd
Chromatograms <- S7::new_class(
  name = "Chromatograms",
  package = "StreamFind",
  parent = Results,
  
  properties = list(
    chromatograms = S7::new_property(S7::class_list, default = list()),
    replicates = S7::new_property(S7::class_character, default = character()),
    is_averaged = S7::new_property(S7::class_logical, default = FALSE),
    peaks = S7::new_property(S7::class_list, default = list()),
    has_peaks = S7::new_property(S7::class_logical, getter = function(self) length(self@peaks) > 0),
    calibration_model = S7::new_property(S7::class_list, default = list())
  ),
  
  # MARK: constructor
  ## __constructor -----
  constructor = function(chromatograms = list(),
                         replicates = character(),
                         is_averaged = FALSE,
                         peaks = list(),
                         calibration_model = list()) {
    S7::new_object(
      Results(), 
      name = "Chromatograms",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      chromatograms = chromatograms,
      replicates = replicates,
      is_averaged = is_averaged,
      peaks = peaks,
      calibration_model = list()
    )
  },
  
  # MARK: validator
  ## __validator -----
  validator = function(self) {
    checkmate::assert_true(self@name == "Chromatograms")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_list(self@chromatograms)
    checkmate::assert_character(self@replicates)
    checkmate::assert_list(self@peaks)
    checkmate::assert_logical(self@is_averaged, len = 1)
    checkmate::assert_list(self@calibration_model)
    if (length(self@chromatograms) > 0) {
      for (chromatogram in self@chromatograms) {
        checkmate::assert_data_frame(chromatogram)
      }
    }
    if (length(self@peaks) > 0) {
      for (peak in self@peaks) {
        checkmate::assert_data_frame(peak)
      }
    }
    NULL
  }
)

# MARK: Methods
# Methods ------

# MARK: length
## length -----
#' @export
#' @noRd
S7::method(length, Chromatograms) <- function(x) {
  length(x@chromatograms)
}

# MARK: names
## names -----
#' @export
#' @noRd
S7::method(names, Chromatograms) <- function(x) {
  names(x@chromatograms)
}

#' @export
#' @noRd
S7::method(show, Chromatograms) <- function(x) {
  if (length(x@chromatograms) > 0) {
    cat("Number chromatograms: ", length(x@chromatograms), "\n")
    cat("Averaged: ", x@is_averaged, "\n")
    if (x@has_peaks) {
      cat("Number peaks: ", vapply(x@peaks, nrow, 0), "\n")
    } else {
      cat("Number peaks: ", 0, "\n")
    }
    if (length(x@calibration_model) > 0) {
      cat("Calibration model: ", class(x@calibration_model), "\n")
    }
  } else {
    cat("Number chromatograms: ", 0, "\n")
  }
}

# MARK: `[`
## `[` -----
#' @export
#' @noRd
S7::method(`[`, Chromatograms) <- function(x, i) {
  x@chromatograms <- x@chromatograms[i]
  if (x@has_peaks) {
    x@peaks <- x@peaks[i]
  }
  if (x@is_averaged) {
    x@replicates <- x@replicates[i]
  } else {
    x@replicates <- x@replicates[names(x@spectra)]
  }
  x
}

# MARK: get_chromatograms
## get_chromatograms -----
#' @export
#' @noRd
S7::method(get_chromatograms, Chromatograms) <- function(x,
                                                         analyses = NULL,
                                                         chromatograms = NULL,
                                                         rtmin = 0,
                                                         rtmax = 0,
                                                         minIntensity = NULL) {
  
  if (length(x@chromatograms) == 0) {
    warning("No chromatograms results available!")
    return(list())
  }
  
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) return(list())
  
  if (x$is_averaged) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    x$chromatograms <- x$chromatograms[names(x$chromatograms) %in% unname(rpl)]
    x$chromatograms <- Map( function(z, y) {
      if (nrow(z) > 0) {
        z$replicate <- y
        data.table::setcolorder(z, c("replicate"))
      }
      z
    }, x$chromatograms, names(x$chromatograms))
  } else {
    rpl <- x$replicates[analyses]
    x$chromatograms <- x$chromatograms[analyses]
    x$chromatograms <- Map( function(z, y) {
      if (nrow(z) > 0) {
        z$analysis <- y
        z$replicate <- rpl[y]
        data.table::setcolorder(z, c("analysis", "replicate"))
      }
      z
    }, x$chromatograms, names(x$chromatograms))
  }
  
  chroms <- x$chromatograms
  
  chrom_list <- lapply(x$chromatograms, function(z, chromatograms, rtmin, rtmax, minIntensity) {
    if (nrow(z) == 0) {
      return(data.table::data.table())
    }
    
    if (is.numeric(chromatograms)) {
      which_chroms <- z$index %in% chromatograms
      z <- z[which_chroms, ]
    } else if (is.character(chromatograms)) {
      which_chroms <- z$id %in% chromatograms
      z <- z[which_chroms, ]
    } else if (!is.null(chromatograms)) {
      return(data.table::data.table())
    }
    
    if (is.numeric(minIntensity)) z <- z[z$intensity > minIntensity, ]
    
    if (is.numeric(rtmin) && is.numeric(rtmax)) {
      if (rtmax > 0) z <- z[z$rt >= rtmin & z$rt <= rtmax]
    }
    
    z
  }, chromatograms = chromatograms, rtmin = rtmin, rtmax = rtmax, minIntensity = minIntensity)
  
  names(chrom_list) <- names(x$chromatograms)
  
  chrom_list
}

# MARK: plot_chromatograms
## plot_chromatograms -----
#' @export
#' @noRd
S7::method(plot_chromatograms, Chromatograms) <- function(x,
                                                          analyses = NULL,
                                                          chromatograms = NULL,
                                                          rtmin = 0,
                                                          rtmax = 0,
                                                          minIntensity = NULL,
                                                          xLab = NULL,
                                                          yLab = NULL,
                                                          title = NULL,
                                                          colorBy = "analyses+targets",
                                                          legendNames = NULL,
                                                          interactive = TRUE,
                                                          renderEngine = "webgl") {
  chroms <- StreamFind::get_chromatograms(
    x, analyses,
    chromatograms,
    rtmin, rtmax,
    minIntensity
  )
  
  chroms <- data.table::rbindlist(chroms)
  
  if (nrow(chroms) == 0) {
    message("\U2717 Chromatograms not found for the analyses!")
    return(NULL)
  }
  
  pol_key <- c("positive", "negative", "nd")
  names(pol_key) <- c("1", "-1", "0")
  chroms$polarity <- as.character(chroms$polarity)
  chroms$polarity <- pol_key[chroms$polarity]
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames)
  
  chroms$loop <- paste0(chroms$analysis, chroms$replicate, chroms$id, chroms$var)
  
  cl <- .get_colors(unique(chroms$var))
  
  if (!interactive) {
    ggplot2::ggplot(chroms, ggplot2::aes(x = rt, y = intensity, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(color = var)) + 
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- chroms %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines+markers",
        line = list(width = 0.5),
        marker = list(size = 2),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>index: ", index,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>pre_mz: ", pre_mz,
          "<br>pre_ce: ", pre_ce,
          "<br>pro_mz: ", pro_mz,
          "<br>rt: ", rt,
          "<br>intensity: ", intensity
        ),
        hoverinfo = "text"
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: plot_chromatograms_baseline
## plot_chromatograms_baseline -----
#' @export
#' @noRd
S7::method(plot_chromatograms_baseline, Chromatograms) <- function(x,
                                                                   analyses = NULL,
                                                                   chromatograms = NULL,
                                                                   xLab = NULL,
                                                                   yLab = NULL,
                                                                   title = NULL,
                                                                   colorBy = "analyses",
                                                                   interactive = TRUE,
                                                                   renderEngine = "webgl") {
  chroms <- get_chromatograms(x, analyses, chromatograms, minIntensity = 0)
  
  chroms <- data.table::rbindlist(chroms)
  
  if (nrow(chroms) == 0) {
    message("\U2717 Chromatograms not found for the analyses!")
    return(NULL)
  }
  
  if (!("baseline" %in% colnames(chroms) && "raw" %in% colnames(chroms))) {
    warning("Baseline not found!")
    return(NULL)
  }
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity / counts"
  
  chroms <- .make_colorBy_varkey(chroms, colorBy, legendNames = NULL)
  
  chroms$loop <- paste0(chroms$analysis, chroms$replicate, chroms$id, chroms$var)
  
  cl <- .get_colors(unique(chroms$var))
  
  if (!interactive) {
    ggplot2::ggplot(chroms, ggplot2::aes(x = rt, group = loop)) + 
      ggplot2::geom_line(ggplot2::aes(y = raw, color = var)) +
      ggplot2::geom_line(ggplot2::aes(y = baseline, color = var), linetype = "dashed") +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    loop <- NULL
    
    plot <- chroms %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~rt,
        y = ~raw,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(width = 1),
        text = ~paste(
          "<br>analysis: ", analysis,
          "<br>replicate: ", replicate,
          "<br>index: ", index,
          "<br>id: ", id,
          "<br>polarity: ", polarity,
          "<br>pre_mz: ", pre_mz,
          "<br>pre_ce: ", pre_ce,
          "<br>pro_mz: ", pro_mz,
          "<br>rt: ", rt,
          "<br>intensity: ", raw
        ),
        hoverinfo = "text",
        name = ~var,
        legendgroup = ~var
      ) %>% plotly::add_trace(
        x = ~rt,
        y = ~baseline,
        type = "scatter",
        color = ~var,
        colors = cl,
        mode = "lines",
        line = list(dash = 'dash', width = 0.5),
        name = ~var,
        legendgroup = ~var,
        showlegend = FALSE
      ) %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: get_chromatograms_peaks
## get_chromatograms_peaks -----
#' @export
#' @noRd
S7::method(get_chromatograms_peaks, Chromatograms) <- function(x,
                                                               analyses = NULL,
                                                               chromatograms = NULL) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  if (!x$has_peaks) {
    return(data.table::data.table())
  }
  
  pks <- x$peaks
  if (length(pks) == 0) {
    return(data.table::data.table())
  }
  
  if (x$is_averaged) {
    pks <- data.table::rbindlist(x$chromatograms$peaks, idcol = "replicate", fill = TRUE)
  } else {
    pks <- data.table::rbindlist(x$chromatograms$peaks, idcol = "analysis", fill = TRUE)
  }
  
  if ("analysis" %in% colnames(pks)) {
    pks <- pks[pks$analysis %in% analyses, ]
  } else if ("replicate" %in% colnames(pks)) {
    rpl <- x$replicates
    rpl <- rpl[analyses]
    pks <- pks[pks$replicate %in% unname(rpl)]
    
    if (!"analysis" %in% colnames(pks)) {
      pks$analysis <- pks$replicate
      data.table::setcolorder(pks, c("analysis", "replicate"))
    }
  }
  
  if (is.numeric(chromatograms)) {
    which_pks <- pks$index %in% chromatograms
    pks <- pks[which_pks, ]
  } else if (is.character(chromatograms)) {
    which_pks <- pks$id %in% chromatograms
    pks <- pks[which_pks, ]
  } else if (!is.null(chromatograms)) {
    return(data.table::data.table())
  }
  
  if (nrow(pks) == 0) {
    message("\U2717 Peaks not found for the targets!")
    return(data.table::data.table())
  }
  
  pks
}

