#' @export
#' @noRd
NTS <- S7::new_class(
  # MARK: NTS
  # NTS ----
  name = "NTS",
  package = "StreamFind",
  parent = Results,
  
  properties = list(

    # MARK: analyses_info
    ## analyses_info -----
    analyses_info = S7::new_property(S7::class_data.frame, default = data.table::data.table()),
    
    # MARK: spectra_headers
    ## spectra_headers -----
    spectra_headers = S7::new_property(S7::class_list, default = list()),
    
    # MARK: feature_list
    ## feature_list -----
    feature_list = S7::new_property(S7::class_list, default = list()),
    
    # MARK: spectra_polarity
    ## spectra_polarity -----
    spectra_polarity = S7::new_property(
      S7::class_character,
      getter = function(self) {
        vapply(self@spectra_headers, function(x) {
          if (nrow(x) == 0) return(NA_character_)
          polarity <- unique(x$polarity)
          if (length(polarity) > 1) {
            # tries to infer short polarity switching from scans
            polarities <- x$polarity
            scans_pos <- length(polarities[polarities == 1])
            scans_neg <- length(polarities[polarities == -1])
            ratio <- scans_pos / scans_neg
            if (ratio > 1.5) {
              polarity <- 1
            } else if (ratio < 0.5) {
              polarity <- -1
            }
          }
          polarity[polarity == 0] <- "unkown"
          polarity[polarity == 1] <- "positive"
          polarity[polarity == -1] <- "negative"
          if (length(polarity) > 1) polarity <- paste(polarity, collapse = ", ")
          polarity
        }, NA_character_)
      }
    ),
    
    # MARK: replicates
    ## replicates -----
    replicates = S7::new_property(
      S7::class_character,
      getter = function(self) {
        if (nrow(self@analyses_info) > 0) {
          rpl <- self@analyses_info$replicate
          names(rpl) <- self@analyses_info$analysis
          rpl
        } else {
          character()
        }
      }
    ),
    
    # MARK: blanks
    ## blanks -----
    blanks = S7::new_property(
      S7::class_character,
      getter = function(self) {
        if (nrow(self@analyses_info) > 0) {
          bln <- self@analyses_info$blank
          names(bln) <- self@analyses_info$analysis
          bln
        } else {
          character()
        }
      }
    ),

    # MARK: number_analyses
    ## number_analyses -----
    number_analyses = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        nrow(self@analyses_info)
      }
    ),

    # MARK: number_features
    ## number_features -----
    number_features = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        if (length(self@feature_list) > 0) {
          vapply(self@feature_list, function(x) {
            if (nrow(x) == 0) return(0)
            nrow(x[!x$filtered, ])
          }, 0)
        } else {
          0
        }
      }
    ),

    # MARK: has_features
    ## has_features -----
    has_features = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        any(self@number_features > 0)
      }
    ),

    # MARK: has_filtered_features
    ## has_filtered_features -----
    has_filtered_features = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          any(vapply(self@feature_list, function(x) any(x$filtered), FALSE))
        } else {
          FALSE
        }
      }
    ),

    # MARK: number_filtered_features
    ## number_filtered_features -----
    number_filtered_features = S7::new_property(
      S7::class_integer,
      getter = function(self) {
        if (self@has_features) {
          vapply(self@feature_list, function(x) sum(x$filtered), 0)
        } else {
          0
        }
      }
    ),
    
    # MARK: has_groups
    ## has_groups -----
    has_groups = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          any(vapply(self@feature_list, function(x) any(!is.na(x$group)), FALSE))
        } else {
          FALSE
        }
      }
    ),
    
    # MARK: number_groups
    ## number_groups -----
    number_groups = S7::new_property(S7::class_integer,
      getter = function(self) {
        if (self@has_groups) {
          vapply(self@feature_list, function(x) {
            if (nrow(x) == 0) return(0)
            length(x$group[!x$filtered & !is.na(x$group)])
          }, 0)
        } else {
          0
        }
      }
    ),
    
    # MARK: group_names
    ## group_names -----
    group_names = S7::new_property(S7::class_character,
      getter = function(self) {
        if (self@has_groups) {
          grps <- lapply(self@feature_list, function(x) {
            if (nrow(x) > 0) {
              unique(x$group)
            } else {
              character()
            }
          })
          return(unique(unlist(grps)))
        }
        character()
      }
    ),
    
    # MARK: has_features_ms1
    ## has_features_ms1 -----
    has_features_ms1 = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          return(any(vapply(self$feature_list, function(x) {
            if ("ms1" %in% colnames(x)) {
              any(vapply(x$ms1, function(z) length(z) > 0, FALSE))
            } else {
              FALSE
            }
          }, FALSE)))
        }
        FALSE
      }
    ),
    
    # MARK: has_features_ms2
    ## has_features_ms2 -----
    has_features_ms2 = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          return(any(vapply(self$feature_list, function(x) {
            if ("ms2" %in% colnames(x)) {
              any(vapply(x$ms2, function(z) length(z) > 0, FALSE))
            } else {
              FALSE
            }
          }, FALSE)))
        }
        FALSE
      }
    ),
    
    # MARK: has_features_eic
    ## has_features_eic -----
    has_features_eic = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          return(any(vapply(self$feature_list, function(x) {
            if ("eic" %in% colnames(x)) {
              any(vapply(x$eic, function(z) length(z) > 0, FALSE))
            } else {
              FALSE
            }
          }, FALSE)))
        }
        FALSE
      }
    ),
    
    # MARK: has_features_suspects
    ## has_features_suspects -----
    has_features_suspects = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@has_features) {
          return(any(vapply(self$feature_list, function(x) {
            if ("suspects" %in% colnames(x)) {
              any(vapply(x$suspects, function(z) length(z) > 0, FALSE))
            } else {
              FALSE
            }
          }, FALSE)))
        }
        FALSE
      }
    )
  ),
  
  # MARK: constructor
  ## constructor -----
  constructor = function(analyses_info = data.table::data.table(),
                         spectra_headers = list(),
                         feature_list = list()) {
    S7::new_object(
      StreamFind::Results(),
      name = "NTS",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      analyses_info = analyses_info,
      spectra_headers = spectra_headers,
      feature_list = feature_list
    )
  },
  
  # MARK: validator
  ## validator -----
  validator = function(self) {
    checkmate::assert_true(self@name == "NTS")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_character(self@version, len = 1)
    if (length(self@has_features) > 0) {
      checkmate::assert_true(identical(self$analyses_info$analysis, names(self@feature_list)))
      checkmate::assert_true(identical(self$analyses_info$analysis, names(self@spectra_headers)))
      fp <- c(
        "feature", "rt", "mz", "area", "intensity",
        "rtmin", "rtmax", "mzmin", "mzmax", "mass",
        "polarity", "adduct", "filtered", "filter", "filled", "group",
        "quality", "annotation", "istd", "ms1", "ms2", "eic",
        "suspects", "formulas", "compounds"
      )
      for (x in self@feature_list) {
        checkmate::assert_data_table(x)
        checkmate::assert_true(all(fp %in% colnames(x)))
      }
    }
    NULL
  }
)

# MARK: Methods
# Methods -----

# MARK: show
## show ----
#' @export
#' @noRd
S7::method(show, NTS) <- function(x) {
  cat("\n")
  cat(is(x))
  cat("\n")
  if (!x$has_features) {
    cat("No features found!")
    return()
  }
  info <- data.table::data.table(
    "analysis" = x@analyses_info$analysis,
    "features" = x@number_features,
    "filtered" = x@number_filtered_features,
    "groups" = x@number_groups
  )
  print(info)
}

# MARK: names
## names -----
#' @export
#' @noRd
S7::method(names, NTS) <- function(x) {
  names(x@feature_list)
}

# MARK: `[`
## `[` -----
#' @export
#' @noRd
S7::method(`[`, NTS) <- function(x, i, j) {
  if (!x$has_features) {
    warning("No features found to subset!")
    return(x)
  }
  if (!missing(j)) {
    x@analyses_info <- x@analyses_info[i, ]
    x@feature_list <- x@feature_list[i]
    return(x)
  }
  if (!missing(j)) {
    if (!x$has_groups) {
      warning("No feature groups found to subset!")
    } else {
      x@feature_list <- lapply(x@feature_list, function(z) {
        z <- z[z$group %in% j, ]
        z
      })
    }
  }
  x
}

# MARK: `[[`
## `[[` -----
#' @export
#' @noRd
S7::method(`[[`, NTS) <- function(x, i) {
  if (!missing(i)) {
    if (!x$has_groups) {
      warning("No feature groups found to subset!")
    } else if (length(i) == 1) {
      x@feature_list <- lapply(x@feature_list, function(z) {
        z <- z[z$group %in% i, ]
        z
      })
    } else {
      warning("Only one group can be selected!")
    }
  }
  x
}

# MARK: get_features_count
## get_features_count -----
#' @export
#' @noRd
S7::method(get_features_count, NTS) <- function(x, analyses = NULL, filtered = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  info <- data.table::data.table()
  if (x$has_features) {
    info <- data.table::data.table(
      "analysis" = x@analyses_info$analysis,
      "replicate" = x$replicates,
      "features" = x@number_features,
      "filtered" = x@number_filtered_features,
      "groups" = x@number_groups
    )
    if (filtered) {
      info$features <- info$filtered + info$features
    }
    info <- info[info$analysis %in% analyses, ]
  }
  info
}

# MARK: plot_features_count
## plot_features_count -----
#' @export
#' @noRd
S7::method(plot_features_count, NTS) <- function(x,
                                                 analyses = NULL,
                                                 filtered = FALSE,
                                                 yLab = NULL,
                                                 title = NULL,
                                                 colorBy = "analyses",
                                                 showLegend = TRUE,
                                                 showHoverText = TRUE) {
  info <- get_features_count(x, analyses, filtered)
  
  if ("replicates" %in% colorBy) info$analysis <- info$replicate
  
  features <- NULL
  
  info <- info[, .(
    features = round(mean(features), digits = 0),
    features_sd = round(sd(features), digits = 0),
    n_analysis = length(features)
  ), by = c("analysis")]
  
  info$features_sd[is.na(info$features_sd)] <- 0
  
  info <- unique(info)
  
  if (showHoverText) {
    info$hover_text <- paste(
      info$analysis, "<br>",
      "N.: ", info$n_analysis, "<br>",
      "Features: ", info$features, " (SD: ", info$features_sd, ")"
    )
  } else {
    info$hover_text <- ""
  }
  
  info <- info[order(info$analysis), ]
  
  colors_tag <- StreamFind:::.get_colors(info$analysis)
  
  if (is.null(yLab)) yLab <- "Number of features"
  
  plot <- plotly::plot_ly(
    x = info$analysis,
    y = info$features,
    marker = list(color = unname(colors_tag)),
    type = "bar",
    text = info$hover_text,
    hoverinfo = "text",
    error_y = list(
      type = "data",
      array = info$features_sd,
      color = "darkred",
      symmetric = FALSE,
      visible = TRUE
    ),
    name = names(colors_tag),
    showlegend = showLegend
  ) %>% plotly::layout(
    xaxis = list(title = NULL),
    yaxis = list(title = yLab)
  )
  
  plot
}

# MARK: get_features
## get_features -----
#' @export
#' @noRd
S7::method(get_features, NTS) <- function(x,
                                          analyses = NULL,
                                          features = NULL,
                                          mass = NULL,
                                          mz = NULL,
                                          rt = NULL,
                                          mobility = NULL,
                                          ppm = 20,
                                          sec = 60,
                                          millisec = 5,
                                          filtered = FALSE) {
  analyses <- .check_analyses_argument(x, analyses)
  if (is.null(analyses)) {
    return(data.table::data.table())
  }
  
  fts <- NULL
  
  if (x$has_features) fts <- x$feature_list[analyses]
  
  if (is.null(fts)) {
    return(data.table::data.table())
  }
  
  fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  if (!filtered) fts <- fts[!fts$filtered, ]
  
  fts$feature <- as.character(fts$feature)
  
  if (!is.null(features)) {
    target_id <- features
    
    if (is.character(target_id)) {
      if ("group" %in% colnames(fts)) {
        fts <- fts[fts$feature %in% target_id | fts$group %in% target_id, ]
      } else {
        fts <- fts[fts$feature %in% target_id, ]
      }
      
      fts$replicate <- x$replicates[fts$analysis]
      
      return(fts)
    } else if (is.numeric(target_id)) {
      fts <- fts[target_id, ]
      
      fts$replicate <- x$replicates[fts$analysis]
      
      return(fts)
    }
    
    if (is.data.frame(target_id)) {
      if (all(colnames(fts) %in% colnames(target_id))) {
        return(target_id)
      }
      
      if ("analysis" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        
        for (i in seq_len(nrow(target_id))) {
          sel[(fts$feature %in% target_id$feature[i] &
                 fts$analysis %in% target_id$analysis[i]) |
                fts$group %in% target_id$group] <- TRUE
        }
        
        fts <- fts[sel, ]
        
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$feature
          fts$name <- ids[fts$feature]
        }
        
        return(fts)
      } else if ("group" %in% colnames(target_id)) {
        sel <- rep(FALSE, nrow(fts))
        
        for (i in seq_len(nrow(target_id))) {
          sel[fts$feature %in% target_id$feature[i] |
                fts$group %in% target_id$group] <- TRUE
        }
        
        fts <- fts[sel, ]
        
        if ("name" %in% colnames(target_id)) {
          ids <- target_id$name
          names(ids) <- target_id$group
          ids <- ids[!duplicated(names(ids))]
          fts$name <- ids[fts$group]
        }
        
        fts$replicate <- x$replicates[fts$analysis]
        
        return(fts)
      }
    }
    
    return(data.table::data.table())
  }
  
  polarities <- unique(fts$polarity)
  polarities[polarities == 0] <- "unkown"
  polarities[polarities == 1] <- "positive"
  polarities[polarities == -1] <- "negative"
  
  id <- NULL
  
  targets <- MassSpecTargets(mass, mz, rt, mobility, ppm, sec, millisec, id, analyses, polarities)
  
  targets <- targets@targets
  
  if (nrow(targets) > 0) {
    for (i in seq_len(nrow(targets))) {
      if (targets$rtmax[i] == 0) targets$rtmax[i] <- max(fts$rtmax)
      if (targets$mzmax[i] == 0) targets$mzmax[i] <- max(fts$mzmax)
      
      if ("mobility" %in% colnames(fts)) {
        if (targets$mobilitymax[i] == 0) targets$mobilitymax[i] <- max(fts$mobility)
      }
    }
    
    sel <- rep(FALSE, nrow(fts))
    
    ids <- rep(NA_character_, nrow(fts))
    
    if ("polarity" %in% colnames(targets) && nrow(targets) > 0) {
      for (i in seq_len(nrow(targets))) {
        if (targets$polarity[i] == "positive") targets$polarity[i] <- 1
        if (targets$polarity[i] == "negative") targets$polarity[i] <- -1
      }
    }
    
    for (i in seq_len(nrow(targets))) {
      if ("mobility" %in% colnames(fts)) {
        sel[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
              data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              data.table::between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- TRUE
        
        ids[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
              data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i]) &
              data.table::between(fts$mobility, targets$mobilitymin[i], targets$mobilitymax[i])] <- targets$id[i]
      } else {
        sel[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
              data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- TRUE
        
        ids[fts$analysis == targets$analysis[i] & fts$polarity == targets$polarity[i] &
              data.table::between(fts$mz, targets$mzmin[i], targets$mzmax[i]) &
              data.table::between(fts$rt, targets$rtmin[i], targets$rtmax[i])] <- targets$id[i]
      }
    }
    
    fts$name <- ids
    
    fts$replicate <- x$replicates[fts$analysis]
    
    return(fts[sel])
  }
  
  browser()
  browser()
  
  fts$replicate <- x$replicates[fts$analysis]
  
  fts
}

# MARK: map_features
## map_features -----
#' @export
#' @noRd
S7::method(map_features, NTS) <- function(x,
                                          analyses = NULL,
                                          features = NULL,
                                          mass = NULL,
                                          mz = NULL,
                                          rt = NULL,
                                          mobility = NULL,
                                          ppm = 20,
                                          sec = 60,
                                          millisec = 5,
                                          neutral_mass = TRUE,
                                          filtered = FALSE,
                                          legendNames = NULL,
                                          xLab = NULL,
                                          yLab = NULL,
                                          title = NULL,
                                          colorBy = "replicates+targets",
                                          showLegend = TRUE,
                                          interactive = TRUE,
                                          renderEngine = "webgl") {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)
  
  cl <- .get_colors(unique(fts$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)
  
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- "Retention time / seconds"
    if (is.null(yLab)) {
      if (neutral_mass) {
        yLab <- expression(italic("m/z ") / " Da")
      } else {
        yLab <- "Mass / Da"
      }
    }
    
    if (neutral_mass) {
      fts$mzmin <- fts$mass - (fts$mz - fts$mzmin)
      fts$mzmax <- fts$mass + (fts$mzmax - fts$mz)
      fts$mz <- fts$mass
    }
    
    ggplot2::ggplot(fts, aes(x = rt, y = mz)) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = rtmin, xmax = rtmax,
          ymin = mzmin, ymax = mzmax,
          fill = factor(var)
        ), alpha = 0.7
      ) +
      ggplot2::geom_point(ggplot2::aes(color = factor(var)), size = 2) +
      ggplot2::scale_color_manual(values = cl) + 
      ggplot2::scale_fill_manual(values = cl, guide = FALSE) + 
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
  } else {
    if (is.null(xLab)) xLab <- "Retention time / seconds"
    if (is.null(yLab)) {
      if (neutral_mass) {
        yLab <- "<i>m/z</i> / Da"
      } else {
        yLab <- "Mass / Da"
      }
    }
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    plotlegend <- rep(TRUE, length(cl))
    
    names(plotlegend) <- names(cl)
    
    plot <- plot_ly()
    
    for (i in seq_len(nrow(fts))) {
      ft <- fts[i, ]
      
      if (neutral_mass) {
        ft$mzmin <- ft$mass - (ft$mz - ft$mzmin)
        ft$mzmax <- ft$mass + (ft$mzmax - ft$mz)
        ft$mz <- ft$mass
      }
      
      x0 <- ft$rtmin
      x1 <- ft$rtmax
      y0 <- ft$mzmin
      y1 <- ft$mzmax
      ft_var <- ft$var
      
      plot <- plot %>% add_trace(
        x = c(x0, x1, x1, x0, x0),
        y = c(y0, y0, y1, y1, y0),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = paste0(cl[ft_var], "70"),
        line = list(color = cl[ft_var]),
        opacity = 0.2,
        name = ft_var,
        legendgroup = ft_var,
        showlegend = FALSE
      )
    }
    
    for (i in seq_len(nrow(fts))) {
      ft <- fts[i, ]
      
      hT <- .make_features_hover_string(ft)
      
      if (neutral_mass) ft$mz <- ft$mass
      
      plot <- plot %>% add_trace(
        x = ft$rt,
        y = ft$mz,
        type = "scatter", mode = "markers",
        marker = list(size = 8, color = cl[ft$var]),
        name = ft$var,
        legendgroup = ft$var,
        showlegend = plotlegend[ft$var],
        text = hT,
        hoverinfo = "text"
      )
      
      if (isTRUE(plotlegend[ft$var])) {
        plotlegend[ft$var] <- FALSE
      }
    }
    
    if (showLegend) {
      plot <- plot %>% plotly::layout(
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    } else {
      plot <- plot %>% plotly::layout(
        legend = NULL,
        xaxis = xaxis,
        yaxis = yaxis,
        title = title
      )
    }
    
    if (renderEngine %in% "webgl") {
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: map_features_intensity
## map_features_intensity -----
#' @export
#' @noRd
S7::method(map_features_intensity, NTS) <- function(x,
                                                    analyses = NULL,
                                                    features = NULL,
                                                    mass = NULL,
                                                    mz = NULL,
                                                    rt = NULL,
                                                    mobility = NULL,
                                                    ppm = 20,
                                                    sec = 60,
                                                    millisec = 5,
                                                    filtered = FALSE,
                                                    legendNames = NULL,
                                                    xLab = NULL,
                                                    yLab = NULL,
                                                    title = NULL,
                                                    colorBy = "replicates+targets",
                                                    renderEngine = "webgl") {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)
  
  if (is.null(xLab)) xLab <- "Retention time / seconds"
  if (is.null(yLab)) yLab <- "Intensity (Counts)"
  
  title <- list(text = title, font = list(size = 12, color = "black"))
  xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
  yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
  
  hT <- .make_features_hover_string(fts)
  
  plot <- plotly::plot_ly(
    data = fts,
    x = ~rt,
    y = ~intensity,
    color = ~var,
    type = "scatter",
    mode = "markers",
    colors = StreamFind:::.get_colors(unique(fts$var)),
    text = ~hT,
    hoverinfo = "text"
  ) %>%
  plotly::layout(
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )
  
  if (renderEngine %in% "webgl") {
    plot <- plot %>% plotly::toWebGL()
  }
  
  plot
}

# MARK: get_features_eic
## get_features_eic -----
#' @export
#' @noRd
S7::method(get_features_eic, NTS) <- function(x,
                                              analyses = NULL,
                                              features = NULL,
                                              mass = NULL,
                                              mz = NULL,
                                              rt = NULL,
                                              mobility = NULL,
                                              ppm = 20,
                                              sec = 60,
                                              millisec = 5,
                                              rtExpand = 0,
                                              mzExpand = 0,
                                              filtered = FALSE,
                                              useLoadedData = TRUE) {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  if (useLoadedData) {
    if (x$has_features_eic) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$analyses_info[x$analyses_info$analysis %in% names(fts_list), ]
    
    fts <- rcpp_ms_load_features_eic(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$spectra_headers[names(fts_list)],
      features = fts_list,
      filtered = filtered,
      rtExpand = rtExpand,
      mzExpand = mzExpand,
      minTracesIntensity = 0
    )
    
    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  } else {
    sel <- vapply(fts$eic, function(z) {
      if (length(z) == 0) {
        return(FALSE)
      }
      if (is.data.frame(z)) if (nrow(z) == 0) {
        return(FALSE)
      }
      TRUE
    }, TRUE)
    fts_without_eic <- fts[!sel, ]
    fts_with_eic <- fts[sel, ]
    
    if (nrow(fts_without_eic) > 0) {
      fts_without_eic_ana_split_vector <- fts_without_eic$analysis
      fts_without_eic$analysis <- NULL
      fts_without_eic_list <- split(fts_without_eic, fts_without_eic_ana_split_vector)
      ana_info <- x$analyses_info[x$analyses_info$analysis %in% names(fts_without_eic_list), ]
      
      fts_without_eic <- rcpp_ms_load_features_eic(
        analyses_names = ana_info$analysis,
        analyses_files = ana_info$file,
        headers = x$spectra_headers[names(fts_without_eic_list)],
        features = fts_without_eic_list,
        filtered = filtered,
        rtExpand = rtExpand,
        mzExpand = mzExpand,
        minTracesIntensity = 0
      )
      
      fts_without_eic <- data.table::rbindlist(fts_without_eic, idcol = "analysis", fill = TRUE)
      
      fts <- data.table::rbindlist(list(fts_without_eic, fts_with_eic), fill = TRUE)
    }
  }
  
  eic_list <- lapply(seq_len(nrow(fts)), function(z, fts) {
    temp <- fts[z, ]
    temp_ms <- temp[["eic"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    if (!is.data.frame(temp_ms)) temp_ms <- data.table::as.data.table(temp_ms)
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)
  
  eic <- data.table::rbindlist(eic_list, fill = TRUE)
  eic$replicate <- x$replicates[eic$analysis]
  data.table::setcolorder(eic, c("analysis", "replicate", "feature"))
  
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_eic_id <- paste0(eic$analysis, "-", eic$feature)
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    eic$group <- fgs[unique_eic_id]
    data.table::setcolorder(eic, c("analysis", "replicate", "group"))
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    eic$name <- tar_ids[unique_eic_id]
    data.table::setcolorder(eic, c("analysis", "replicate", "name"))
  }
  
  eic
}

# MARK: plot_features
## plot_features -----
#' @export
#' @noRd
S7::method(plot_features, NTS) <- function(x,
                                           analyses = NULL,
                                           features = NULL,
                                           mass = NULL,
                                           mz = NULL,
                                           rt = NULL,
                                           mobility = NULL,
                                           ppm = 20,
                                           sec = 60,
                                           millisec = 5,
                                           rtExpand = 120,
                                           mzExpand = 0.001,
                                           useLoadedData = TRUE,
                                           filtered = FALSE,
                                           legendNames = NULL,
                                           xLab = NULL,
                                           yLab = NULL,
                                           title = NULL,
                                           colorBy = "targets",
                                           interactive = TRUE,
                                           renderEngine = "webgl") {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  eic <- get_features_eic(
    x,
    analyses = unique(fts$analysis),
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  intensity <- NULL
  cols_by <- c("analysis", "replicate", "polarity", "feature", "rt")
  eic <- eic[, `:=`(intensity = max(intensity)), by = cols_by]
  eic <- unique(eic)
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces not found for the targets!")
    return(NULL)
  }
  
  fts <- .make_colorBy_varkey(fts, colorBy, legendNames)
  
  cl <- .get_colors(unique(fts$var))
  cl50 <- paste(cl, "50", sep = "")
  names(cl50) <- names(cl)
  
  if (!interactive) {
    plot <- ggplot2::ggplot(eic, ggplot2::aes(x = rt))
    
    for (i in seq_len(nrow(fts))) {
      ft_analysis <- fts[["analysis"]][i]
      ft_replicate <- fts[["replicate"]][i]
      ft_id <- fts[["feature"]][i]
      ft_var <- fts[["var"]][i]
      ft_min <- fts[["min"]][i]
      ft_max <- fts[["max"]][i]
      
      temp <- dplyr::filter(
        eic,
        analysis %in% ft_analysis & replicate %in% ft_replicate & feature %in% ft_id
      )
      
      temp$var <- ft_var
      
      plot <- plot + ggplot2::geom_line(
        data = temp,
        ggplot2::aes(y = intensity, color = var)
      )
      
      temp <- temp[temp$rt >= ft_min & temp$rt <= ft_max, ]
      
      plot <- plot + ggplot2::geom_ribbon(
        data = temp,
        ggplot2::aes(
          ymin = rep(min(intensity), length(intensity)),
          ymax = intensity,
          fill = var
        )
      )
    }
    
    plot <- plot + ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_fill_manual(values = cl50, guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    title <- list(text = title, font = list(size = 12, color = "black"))
    xaxis <- list(linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"))
    yaxis <- list(linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"))
    
    show_legend <- rep(TRUE, length(cl))
    names(show_legend) <- names(cl)
    
    plot <- plot_ly()
    
    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      
      ft_var <- pk$var
      
      hT <- .make_features_hover_string(pk)
      
      temp <- dplyr::filter(
        eic,
        analysis %in% pk$analysis &
        replicate %in% pk$replicate &
        feature %in% pk$feature &
        rt >= pk$rtmin &
        rt <= pk$rtmax
      )
      
      plot <- plot %>% add_trace(
        data = temp,
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        mode = "markers",
        marker = list(color = cl[ft_var], size = 5),
        text = hT,
        hoverinfo = "text",
        name = ft_var,
        legendgroup = ft_var,
        showlegend = FALSE
      )
      
      plot <- plot %>% plotly::add_ribbons(
        data = temp,
        x = ~rt,
        ymin = ~min(intensity),
        ymax = ~intensity,
        line = list(color = cl[ft_var], width = 1.5),
        fillcolor = cl50[ft_var],
        text = hT,
        hoverinfo = "text",
        name = ft_var,
        legendgroup = ft_var,
        showlegend = show_legend[ft_var]
      )
      
      show_legend[ft_var] <- FALSE
    }
    
    for (i in seq_len(nrow(fts))) {
      pk <- fts[i, ]
      ft_var <- pk$var
      
      plot <- plot %>% add_trace(
        data = dplyr::filter(
          eic,
          analysis %in% pk$analysis &
          replicate %in% pk$replicate &
          feature %in% pk$feature
        ),
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        mode = "lines",
        line = list(color = cl[ft_var], width = 0.5),
        name = ft_var,
        legendgroup = ft_var,
        showlegend = FALSE
      )
    }
    
    plot <- plot %>% plotly::layout(
      xaxis = xaxis,
      yaxis = yaxis,
      title = title
    )
    
    if (renderEngine %in% "webgl") {
      # Fix for warnings with hoveron when webgl is used
      plot$x$attrs <- lapply(plot$x$attrs, function(x) {
        if (!is.null(x[["hoveron"]])) {
          x[["hoveron"]] <- NULL
        }
        x
      })
      
      plot <- plot %>% plotly::toWebGL()
    }
    
    plot
  }
}

# MARK: get_features_ms1
## get_features_ms1 -----
#' @export
#' @noRd
S7::method(get_features_ms1, NTS) <- function(x,
                                              analyses = NULL,
                                              features = NULL,
                                              mass = NULL,
                                              mz = NULL,
                                              rt = NULL,
                                              mobility = NULL,
                                              ppm = 20,
                                              sec = 60,
                                              millisec = 5,
                                              rtWindow = c(-2, 2),
                                              mzWindow = c(-5, 100),
                                              mzClust = 0.003,
                                              presence = 0.8,
                                              minIntensity = 1000,
                                              filtered = FALSE,
                                              useLoadedData = TRUE) {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  if (useLoadedData) {
    if (x$has_features_ms1) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$analyses_info[x$analyses_info$analysis %in% names(fts_list), ]
    
    fts <- rcpp_ms_load_features_ms1(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$spectra_headers[names(fts_list)],
      features = fts_list,
      filtered = filtered,
      rtWindow = rtWindow,
      mzWindow = mzWindow,
      minTracesIntensity = minIntensity,
      mzClust = mzClust,
      presence = presence
    )
    
    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  }
  
  ms1_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
    temp <- fts[x, ]
    temp_ms <- temp[["ms1"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    if (!"mz" %in% colnames(temp_ms)) {
      return(data.table::data.table())
    }
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)
  
  ms1 <- data.table::rbindlist(ms1_list, fill = TRUE)
  ms1$replicate <- x$replicates[ms1$analysis]
  data.table::setcolorder(ms1, c("analysis", "replicate", "feature"))
  
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms1_id <- paste0(ms1$analysis, "-", ms1$feature)
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms1$group <- fgs[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "group"))
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms1$name <- tar_ids[unique_ms1_id]
    data.table::setcolorder(ms1, c("analysis", "replicate", "name"))
  }
  
  ms1
}

# MARK: plot_features_ms1
## plot_features_ms1 -----
#' @export
#' @noRd
S7::method(plot_features_ms1, NTS) <- function(x,
                                               analyses = NULL,
                                               features = NULL,
                                               mass = NULL,
                                               mz = NULL,
                                               rt = NULL,
                                               mobility = NULL,
                                               ppm = 20,
                                               sec = 60,
                                               millisec = 5,
                                               rtWindow = c(-2, 2),
                                               mzWindow = c(-5, 100),
                                               mzClust = 0.003,
                                               presence = 0.8,
                                               minIntensity = 1000,
                                               filtered = FALSE,
                                               useLoadedData = TRUE,
                                               legendNames = NULL,
                                               xLab = NULL,
                                               yLab = NULL,
                                               title = NULL,
                                               colorBy = "targets",
                                               showText = FALSE,
                                               interactive = TRUE) {
  ms1 <- get_features_ms1(
    x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
    rtWindow, mzWindow, mzClust, presence, minIntensity, filtered, useLoadedData
  )
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)
  
  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)
  
  cl <- .get_colors(unique(ms1$var))
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    plot <- ggplot2::ggplot(ms1, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var), linewidth = 1)
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = round(mz, 4)),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms1$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms1$intensity) * 1.5)
    )
    
    loop <- NULL
    
    plot <- ms1 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = 0.01)),
        text = ~paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}

# MARK: get_features_ms2
## get_features_ms2 -----
#' @export
#' @noRd
S7::method(get_features_ms2, NTS) <- function(x,
                                              analyses = NULL,
                                              features = NULL,
                                              mass = NULL,
                                              mz = NULL,
                                              rt = NULL,
                                              mobility = NULL,
                                              ppm = 20,
                                              sec = 60,
                                              millisec = 5,
                                              isolationWindow = 1.3,
                                              mzClust = 0.003,
                                              presence = 0.8,
                                              minIntensity = 0,
                                              filtered = FALSE,
                                              useLoadedData = TRUE) {
  fts <- get_features(x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  if (useLoadedData) {
    if (x$has_features_ms1) {
      useLoadedData <- TRUE
    } else {
      useLoadedData <- FALSE
    }
  }
  
  if (!useLoadedData) {
    fts_ana_split_vector <- fts$analysis
    fts$analysis <- NULL
    fts_list <- split(fts, fts_ana_split_vector)
    ana_info <- x$analyses_info[x$analyses_info$analysis %in% names(fts_list), ]
    
    fts <- rcpp_ms_load_features_ms2(
      analyses_names = ana_info$analysis,
      analyses_files = ana_info$file,
      headers = x$spectra_headers[names(fts_list)],
      features = fts_list,
      filtered = filtered,
      minTracesIntensity = minIntensity,
      isolationWindow = isolationWindow,
      mzClust = mzClust,
      presence = presence
    )
    
    fts <- data.table::rbindlist(fts, idcol = "analysis", fill = TRUE)
  }
  
  ms2_list <- lapply(seq_len(nrow(fts)), function(x, fts) {
    temp <- fts[x, ]
    temp_ms <- temp[["ms2"]][[1]]
    if (is.null(temp_ms)) {
      return(data.table::data.table())
    }
    if (!"mz" %in% colnames(temp_ms)) {
      return(data.table::data.table())
    }
    temp_ms$analysis <- temp$analysis
    temp_ms$feature <- temp$feature
    temp_ms
  }, fts = fts)
  
  ms2 <- data.table::rbindlist(ms2_list, fill = TRUE)
  ms2$replicate <- x$replicates[ms2$analysis]
  data.table::setcolorder(ms2, c("analysis", "replicate", "feature"))
  
  unique_fts_id <- paste0(fts$analysis, "-", fts$feature)
  unique_ms2_id <- paste0(ms2$analysis, "-", ms2$feature)
  
  if ("group" %in% colnames(fts)) {
    fgs <- fts$group
    names(fgs) <- unique_fts_id
    ms2$group <- fgs[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "group"))
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- unique_fts_id
    ms2$name <- tar_ids[unique_ms2_id]
    data.table::setcolorder(ms2, c("analysis", "replicate", "name"))
  }
  
  ms2
}

# MARK: plot_features_ms2
## plot_features_ms2 -----
#' @export
#' @noRd
S7::method(plot_features_ms2, NTS) <- function(x,
                                               analyses = NULL,
                                               features = NULL,
                                               mass = NULL,
                                               mz = NULL,
                                               rt = NULL,
                                               mobility = NULL,
                                               ppm = 20,
                                               sec = 60,
                                               millisec = 5,
                                               isolationWindow = 1.3,
                                               mzClust = 0.005,
                                               presence = 0.8,
                                               minIntensity = 0,
                                               filtered = FALSE,
                                               useLoadedData = TRUE,
                                               legendNames = NULL,
                                               xLab = NULL,
                                               yLab = NULL,
                                               title = NULL,
                                               colorBy = "targets",
                                               showText = TRUE,
                                               interactive = TRUE) {
  ms2 <- get_features_ms2(
    x, analyses, features, mass, mz, rt, mobility, ppm, sec, millisec,
    isolationWindow, mzClust, presence, minIntensity, filtered, useLoadedData
  )
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  
  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)
  
  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)
  
  cl <- .get_colors(unique(ms2$var))
  
  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2
    
    plot <- ggplot2::ggplot(ms2, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var, linewidth = linesize))
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = text_string),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms2$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2
    
    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )
    
    loop <- NULL
    
    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}

# MARK: get_groups
## get_groups -----
#' @export
#' @noRd
S7::method(get_groups, NTS) <- function(x,
                                        groups = NULL,
                                        mass = NULL,
                                        mz = NULL,
                                        rt = NULL,
                                        mobility = NULL,
                                        ppm = 20,
                                        sec = 60,
                                        millisec = 5,
                                        filtered = FALSE,
                                        intensities = TRUE,
                                        average = FALSE,
                                        sdValues = FALSE,
                                        metadata = FALSE,
                                        correctSuppression = FALSE) {
  
  if (!x$has_groups) {
    return(data.table::data.table())
  }
  
  fts <- get_features(
    x, analyses = NULL, features = groups,
    mass, mz, rt, mobility, ppm, sec, millisec,
    filtered = filtered
  )
  
  if (correctSuppression) {
    if ("suppression_factor" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$suppression_factor
    }
  }
  
  if (nrow(fts) > 0) {
    g_ids <- unique(fts$group)
    fgroups <- data.table::data.table("group" = g_ids)
    
    if (intensities) {
      if (average) {
        intensity <- NULL
        rpls <- x$replicates
        fts_temp <- data.table::copy(fts)
        fts_temp$analysis <- rpls[fts_temp$analysis]
        fts_av <- fts_temp[, .(
          intensity = mean(intensity),
          sd = sd(intensity),
          n = length(intensity)
        ), by = c("group", "analysis")]
        fts_av$sd[is.na(fts_av$sd)] <- 0
        fts_av$sd <- round(fts_av$sd / fts_av$intensity * 100, digits = 0)
        fts_sd <- data.table::copy(fts_av)
        fts_n <- data.table::copy(fts_av)
        fts_sd$intensity <- NULL
        fts_sd$n <- NULL
        fts_sd$analysis <- paste(fts_sd$analysis, "_sd", sep = "")
        fts_sd <- data.table::dcast(
          fts_sd[, c("group", "analysis", "sd"), with = TRUE], 
          group ~ analysis, 
          value.var = "sd"
        )
        fts_sd[is.na(fts_sd)] <- 0
        tbl_rpls <- table(rpls)
        fts_n$n <- tbl_rpls[fts_n$analysis]
        fts_n$intensity <- NULL
        fts_n$sd <- NULL
        fts_n$analysis <- paste(fts_n$analysis, "_n", sep = "")
        fts_n <- data.table::dcast(
          fts_n[, c("group", "analysis", "n"), with = TRUE],
          group ~ analysis,
          value.var = "n"
        )
        fts_n[is.na(fts_n)] <- 0
        fts_av$sd <- NULL
        fts_av$n <- NULL
        fts_av <- data.table::dcast(fts_av, group ~ analysis, value.var = "intensity")
        fts_av[is.na(fts_av)] <- 0
      } else {
        fts_av <- fts[, .(intensity = max(intensity)), by = c("group", "analysis")]
        fts_av <- data.table::dcast(fts_av, group ~ analysis, value.var = "intensity")
        fts_av[is.na(fts_av)] <- 0
      }
    }
    
    if ("name" %in% colnames(fts)) {
      g_names <- fts$name
      names(g_names) <- fts$group
      g_names <- g_names[!duplicated(names(g_names))]
      fgroups$name <- g_names[fgroups$group]
    }
    
    if (metadata) {
      rtmin <- NULL
      rtmax <- NULL
      mzmin <- NULL
      mzmax <- NULL
      feature <- NULL
      quality <- NULL
      annotation <- NULL
      istd <- NULL
      
      fts_meta <- fts[, .(
        rt = round(mean(rt), digits = 0),
        mass = round(mean(mass), digits = 4),
        rtdev = round(max(rtmax - rtmin), digits = 0),
        massdev = round(max(mzmax - mzmin), digits = 4),
        presence = round(length(feature) / length(x) * 100, digits = 1),
        maxint = round(max(intensity), digits = 0),
        sn = round(
          max(
            vapply(quality, function(z) {
              if (length(z) > 0) z$sn else 0
            }, 0),
            na.rm = TRUE
          ),
          digits = 1
        ),
        iso = min(
          vapply(annotation, function(z) if (length(z) > 0) z$iso_step else 0, 0),
          na.rm = TRUE
        ),
        istd = paste0(unique(
          vapply(istd, function(z) {
            if (length(z) > 0) z$name else NA_character_
          }, NA_character_)
        ), collapse = "; "),
        filtered = all(filtered)
      ), by = "group"]
      
      fgroups <- fgroups[fts_meta, on = "group"]
    }
    
    if (intensities) fgroups <- fgroups[fts_av, on = "group"]
    
    if (average && sdValues) {
      fgroups <- fgroups[fts_sd, on = "group"]
      fgroups <- fgroups[fts_n, on = "group"]
    }
    
    fgroups
  } else {
    data.table::data.table()
  }
}

# MARK: plot_groups
## plot_groups -----
#' @export
#' @noRd
S7::method(plot_groups, NTS) <- function(x,
                                         groups = NULL,
                                         mass = NULL,
                                         mz = NULL,
                                         rt = NULL,
                                         mobility = NULL,
                                         ppm = 20,
                                         sec = 60,
                                         millisec = 5,
                                         rtExpand = 15,
                                         mzExpand = 0.001,
                                         filtered = FALSE,
                                         legendNames = NULL,
                                         xLab = NULL,
                                         yLab = NULL,
                                         title = NULL,
                                         colorBy = "targets",
                                         interactive = TRUE,
                                         renderEngine = "webgl") {
  fts <- get_features(
    x, analyses = NULL, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered
  )
  
  if (grepl("targets", colorBy) & !isTRUE(legendNames)) {
    fts$name <- fts$group
    if (is.null(legendNames)) legendNames <- TRUE
  }
  
  plot_features(
    x,
    features = fts,
    rtExpand = rtExpand,
    mzExpand = mzExpand,
    filtered = filtered,
    legendNames = legendNames,
    xLab = xLab,
    yLab = yLab,
    title = title,
    colorBy = colorBy,
    interactive = interactive,
    renderEngine = renderEngine
  )
}

# MARK: plot_groups_overview
## plot_groups_overview -----
#' @export
#' @noRd
S7::method(plot_groups_overview, NTS) <- function(x,
                                                  analyses = NULL,
                                                  groups = NULL,
                                                  mass = NULL,
                                                  mz = NULL,
                                                  rt = NULL,
                                                  mobility = NULL,
                                                  ppm = 20,
                                                  sec = 60,
                                                  millisec = 5,
                                                  rtExpand = 120,
                                                  mzExpand = 0.005,
                                                  useLoadedData = TRUE,
                                                  correctSuppression = TRUE,
                                                  filtered = FALSE,
                                                  legendNames = NULL,
                                                  title = NULL,
                                                  heights = c(0.35, 0.5, 0.15),
                                                  renderEngine = "webgl") {
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  eic <- get_features_eic(
    x, analyses = unique(fts$analysis), features = fts,
    rtExpand = rtExpand, mzExpand = mzExpand,
    filtered = filtered, useLoadedData = useLoadedData
  )
  
  intensity <- NULL
  cols_by <- c("analysis", "polarity", "feature", "rt")
  eic <- eic[, `:=`(intensity = max(intensity)), by = cols_by]
  eic <- unique(eic)
  
  if (nrow(eic) == 0) {
    message("\U2717 Traces and/or features not found for targets!")
    return(NULL)
  }
  
  if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
    leg <- legendNames
    names(leg) <- unique(fts$group)
    leg <- leg[fts$group]
  } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
    leg <- fts$name
  } else {
    leg <- fts$group
  }
  
  names(leg) <- paste0(fts$feature, "_", fts$analysis)
  eic$uid <- paste0(eic$feature, "_", eic$analysis)
  fts$uid <- paste0(fts$feature, "_", fts$analysis)
  eic$var <- leg[eic$uid]
  fts$var <- leg
  
  analyses <- .check_analyses_argument(x, analyses)
  
  # .plot_groups_overview_aux(fts, eic, heights, analyses, correctSuppression)
  
  leg <- unique(eic$var)
  colors <- .get_colors(leg)
  showleg <- rep(TRUE, length(leg))
  names(showleg) <- names(leg)
  plot <- plot_ly()
  
  for (g in leg) {
    uid <- unique(eic$uid[eic$var == g])
    
    for (u in uid) {
      ft <- fts[fts$uid == u, ]
      if (nrow(ft) == 0) next
      df <- eic[eic$uid == u, ]
      
      plot <- plot %>% add_trace(
        df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines",
        line = list(width = 0.5, color = colors[g]),
        connectgaps = TRUE,
        name = g,
        legendgroup = g,
        showlegend = FALSE
      )
      
      df <- df[df$rt >= ft$rtmin & df$rt <= ft$rtmax, ]
      df$mz <- as.numeric(df$mz)
      
      plot <- plot %>% add_trace(
        df,
        x = df$rt,
        y = df$intensity,
        type = "scatter", mode = "lines+markers",
        fill = "tozeroy", connectgaps = TRUE,
        fillcolor = paste(color = colors[g], 50, sep = ""),
        line = list(width = 0.1, color = colors[g]),
        marker = list(size = 3, color = colors[g]),
        name = g,
        legendgroup = g,
        showlegend = showleg[which(leg %in% g)],
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = paste(
          "</br> name: ", g,
          "</br> group: ", ft$group,
          "</br> feature: ", ft$feature,
          "</br> analysis: ", ft$analysis,
          "</br> replicate: ", ft$replicate,
          "</br> <i>m/z</i>: ", round(ft$mz, digits = 4),
          "</br> rt: ", round(df$rt, digits = 0),
          "</br> intensity: ", round(df$intensity, digits = 0)
        )
      )
      showleg[which(leg %in% g)] <- FALSE
    }
  }
  
  plot2 <- plot_ly()
  
  for (g in leg) {
    ft2 <- fts[fts$var == g, ]
    if (!"filled" %in% colnames(ft2)) ft2$filled <- FALSE
    
    ft_nf <- ft2[!ft2$filled, ]
    
    if (nrow(ft_nf) > 0) {
      hT <- .make_features_hover_string(ft_nf)
      
      plot2 <- plot2 %>% add_trace(
        x = ft_nf$rt,
        y = ft_nf$analysis,
        type = "scatter",
        mode = "markers",
        marker = list(
          line = list(color = colors[g], width = 3),
          color = "#000000", size = 10
        ),
        error_x = list(
          type = "data",
          symmetric = FALSE,
          arrayminus = ft_nf$rt - ft_nf$rtmin,
          array = ft_nf$rtmax - ft_nf$rt,
          color = colors[g],
          width = 5
        ),
        name = g,
        legendgroup = g,
        showlegend = FALSE,
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = hT
      )
    }
    
    ft_f <- ft2[ft2$filled, ]
    
    if (nrow(ft_f) > 0) {
      hT <- .make_features_hover_string(ft_f)
      
      plot2 <- plot2 %>% add_trace(
        x = ft_f$rt,
        y = ft_f$analysis,
        type = "scatter",
        mode = "markers",
        marker = list(
          line = list(color = colors[g], width = 3),
          color = "#f8f8f8",
          size = 10
        ),
        error_x = list(
          type = "data",
          symmetric = FALSE,
          arrayminus = ft_f$rt - ft_f$rtmin,
          array = ft_f$rtmax - ft_f$rt,
          color = colors[g],
          width = 5
        ),
        name = g,
        legendgroup = g,
        showlegend = FALSE,
        hoverinfo = "text",
        hoverlabel = list(bgcolor = colors[g]),
        text = hT
      )
    }
  }
  plot2 <- hide_colorbar(plot2)
  
  plot3 <- plot_ly(fts, x = sort(unique(fts$analysis)))
  
  for (g in leg) {
    df_3 <- fts[fts$var == g, ]
    
    if (correctSuppression) {
      if ("suppression_factor" %in% colnames(df_3)) {
        df_3$intensity <- df_3$intensity * df_3$suppression_factor
      }
    }
    
    if (!all(analyses %in% df_3$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df_3$analysis],
        "var" = g,
        "intensity" = 0
      )
      df_3 <- rbind(df_3[, c("analysis", "var", "intensity")], extra)
    }
    
    df_3 <- df_3[order(df_3$analysis), ]
    
    plot3 <- plot3 %>% add_trace(
      df_3,
      x = df_3$analysis,
      y = df_3$intensity / max(df_3$intensity),
      type = "scatter", mode = "lines",
      line = list(width = 1, color = colors[g]),
      connectgaps = FALSE,
      name = g,
      legendgroup = g,
      showlegend = FALSE
    )
  }
  
  xaxis <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Retention time / seconds",
    titlefont = list(size = 12, color = "black"),
    range = c(min(eic$rt), max(eic$rt)),
    autotick = TRUE, ticks = "outside"
  )
  
  yaxis1 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Intensity / counts",
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis2 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0, dtick = 1
  )
  
  xaxis3 <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)
  
  yaxis3 <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = "Normalized intensity",
    titlefont = list(size = 12, color = "black"),
    tick0 = 0, dtick = 0.25, range = c(0, 1.5)
  )
  
  plotList <- list()
  
  plot <- plot %>% plotly::layout(xaxis = xaxis3, yaxis = yaxis1)
  plotList[["plot"]] <- plot
  
  plot2 <- plot2 %>% plotly::layout(xaxis = xaxis, yaxis = yaxis2)
  plotList[["plot2"]] <- plot2
  
  plot3 <- plot3 %>% plotly::layout(xaxis = xaxis3, yaxis = yaxis3)
  
  plotf <- subplot(
    plotList,
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = heights[1:2],
    margin = 0.01,
    shareX = TRUE,
    which_layout = "merge"
  )
  
  plotf_2 <- subplot(
    list(plotf, plot3),
    nrows = 2,
    titleY = TRUE, titleX = TRUE,
    heights = c(sum(heights[1:2]), heights[3]),
    margin = 0.01,
    shareX = FALSE,
    which_layout = "merge"
  )
  
  if (renderEngine == "webgl") {
    plotf_2 <- plotly::config(plotf_2, displayModeBar = TRUE)
  }
  
  if (renderEngine %in% "webgl") {
    # Fix for warnings with hoveron when webgl is used
    plotf_2$x$attrs <- lapply(plotf_2$x$attrs, function(x) {
      if (!is.null(x[["hoveron"]])) {
        x[["hoveron"]] <- NULL
      }
      x
    })
    
    plotf_2 <- plotf_2 %>% plotly::toWebGL()
  }
  
  plotf_2
}

# MARK: plot_groups_profile
## plot_groups_profile -----
#' @export
#' @noRd
S7::method(plot_groups_profile, NTS) <- function(x,
                                                 analyses = NULL,
                                                 groups = NULL,
                                                 mass = NULL,
                                                 mz = NULL,
                                                 rt = NULL,
                                                 mobility = NULL,
                                                 ppm = 20,
                                                 sec = 60,
                                                 millisec = 5,
                                                 filtered = FALSE,
                                                 correctSuppression = TRUE,
                                                 averaged = FALSE,
                                                 normalized = TRUE,
                                                 legendNames = NULL,
                                                 yLab = NULL,
                                                 title = NULL,
                                                 renderEngine = "webgl") {
  fts <- get_features(x, analyses, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered)
  
  analyses <- .check_analyses_argument(x, analyses)
  
  if (nrow(fts) == 0) {
    message("\U2717 Features not found for the targets!")
    return(NULL)
  }
  
  polarities <- x$spectra_polarity
  
  if (!"polarity" %in% colnames(fts)) fts$polarity <- polarities[fts$analysis]
  
  if (correctSuppression) {
    if ("suppression_factor" %in% colnames(fts)) {
      fts$intensity <- fts$intensity * fts$suppression_factor
    }
  }
  
  if (normalized && "intensity_rel" %in% colnames(fts)) {
    fts$intensity <- as.numeric(fts$intensity_rel)
  }
  
  if (averaged && x$has_groups) {
    group_cols <- c("replicate", "group", "polarity")
    if ("name" %in% colnames(fts)) group_cols <- c(group_cols, "name")
    intensity <- NULL
    fts <- fts[, .(intensity = mean(intensity)), by = group_cols]
    names(polarities) <- x$replicates[names(polarities)]
    polarities <- polarities[!duplicated(names(polarities))]
    data.table::setnames(fts, "replicate", "analysis")
    analyses <- unique(x$replicates[analyses])
  }
  
  if (is.character(legendNames) & length(legendNames) == length(unique(fts$group))) {
    leg <- legendNames
    names(leg) <- unique(fts$group)
    leg <- leg[fts$group]
    fts$var <- leg[fts$group]
  } else if (isTRUE(legendNames) & "name" %in% colnames(fts)) {
    leg <- fts$name
    fts$var <- fts$name
  } else {
    leg <- fts$group
    fts$var <- fts$group
  }
  
  u_leg <- unique(leg)
  
  colors <- .get_colors(u_leg)
  
  showLeg <- rep(TRUE, length(u_leg))
  names(showLeg) <- u_leg
  
  rpls <- x$replicates
  
  plot <- plot_ly(fts, x = sort(unique(fts$analysis)))
  
  for (g in u_leg) {
    df <- fts[fts$var == g, ]
    
    if (!all(analyses %in% df$analysis)) {
      extra <- data.frame(
        "analysis" = analyses[!analyses %in% df$analysis],
        "polarity" = polarities[
          !names(polarities) %in% df$analysis & names(polarities) %in% analyses
        ],
        "var" = g,
        "intensity" = 0
      )
      df <- rbind(df[, c("analysis", "var", "intensity", "polarity")], extra)
    }
    
    df <- df[order(df$analysis), ]
    
    if (normalized) {
      if (length(unique(df$polarity)) > 1) {
        for (p in unique(df$polarity)) {
          max_int <- max(df$intensity[df$polarity == p])
          if (max_int > 0) {
            df$intensity[df$polarity == p] <- df$intensity[df$polarity == p] / max_int
          }
        }
      } else {
        max_int <- max(df$intensity)
        if (max_int > 0) df$intensity <- df$intensity / max_int
      }
    }
    
    plot <- plot %>% add_trace(
      df,
      x = df$analysis,
      y = df$intensity,
      type = "scatter", mode = "lines",
      line = list(width = 0.5, color = colors[g], dash = "dash"),
      connectgaps = FALSE,
      name = g,
      legendgroup = g,
      showlegend = FALSE
    )
    
    df$replicate <- rpls[df$analysis]
    
    for (r in unique(df$replicate)) {
      df_r <- df[df$replicate %in% r, ]
      
      plot <- plot %>% add_trace(
        df,
        x = df_r$analysis,
        y = df_r$intensity,
        type = "scatter", mode = "lines+markers",
        line = list(width = 1.5, color = colors[g]),
        marker = list(size = 5, color = colors[g]),
        connectgaps = FALSE,
        name = g,
        legendgroup = g,
        showlegend = showLeg[g]
      )
      
      showLeg[g] <- FALSE
    }
  }
  
  xaxis <- list(linecolor = toRGB("black"), linewidth = 2, title = NULL)
  
  if (is.null(yLab)) {
    if (normalized) {
      yLab <- "Normalized intensity"
      range_yaxis <- c(0, max(1.1))
    } else {
      yLab <- "Intensity / counts"
      range_yaxis <- c(min(fts$intensity), max(fts$intensity))
    }
  }
  
  yaxis <- list(
    linecolor = toRGB("black"), linewidth = 2,
    title = yLab,
    titlefont = list(size = 12, color = "black"),
    range = range_yaxis
  )
  
  plot <- plot %>% plotly::layout(xaxis = xaxis, yaxis = yaxis)
  
  if (renderEngine %in% "webgl") {
    plot <- plot %>% plotly::toWebGL()
  }
  
  plot
}

# MARK: get_groups_ms1
## get_groups_ms1 -----
#' @export
#' @noRd
S7::method(get_groups_ms1, NTS) <- function(x,
                                            groups = NULL,
                                            mass = NULL,
                                            mz = NULL,
                                            rt = NULL,
                                            mobility = NULL,
                                            ppm = 20,
                                            sec = 60,
                                            millisec = 5,
                                            rtWindow = c(-2, 2),
                                            mzWindow = c(-5, 90),
                                            mzClustFeatures = 0.003,
                                            presenceFeatures = 0.8,
                                            minIntensityFeatures = 1000,
                                            useLoadedData = TRUE,
                                            mzClust = 0.003,
                                            presence = 0.8,
                                            minIntensity = 1000,
                                            groupBy = "groups",
                                            filtered = FALSE) {
  fgs <- get_groups(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
    intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
  )
  
  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }
  
  fts <- get_features(x, features = fgs$group)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  ms1 <- get_features_ms1(
    x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    rtWindow = rtWindow,
    mzWindow = mzWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  ms1 <- ms1[ms1$intensity > minIntensity, ]
  
  if (nrow(ms1) == 0) {
    return(data.table::data.table())
  }
  
  polarities <- unique(fts$polarity)
  polarities[polarities == 0] <- "unkown"
  polarities[polarities == 1] <- "positive"
  polarities[polarities == -1] <- "negative"
  
  multiple_polarities <- FALSE
  
  # TODO check for polarity switching with comma
  
  if (length(polarities) > 1) multiple_polarities <- TRUE
  
  if ("groups" %in% groupBy) {
    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$group, "_", ms1$polarity)
      ms1$analysis <- NA_character_
    } else {
      ms1$unique_id <- ms1$group
      ms1$analysis <- NA_character_
    }
  } else {
    rpls <- x$replicates
    ms1$analysis <- rpls[ms1$analysis]
    
    if (multiple_polarities) {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group, "", ms1$polarity)
    } else {
      ms1$unique_id <- paste0(ms1$analysis, "_", ms1$group)
    }
  }
  
  ms1$id <- ms1$group
  
  ms1_list <- rcpp_ms_cluster_spectra(ms1, mzClust, presence, verbose = FALSE)
  
  ms1_df <- data.table::rbindlist(ms1_list, fill = TRUE)
  
  ms1_df$group <- ms1_df$id
  
  ms1_df[["id"]] <- NULL
  
  ms1_df <- ms1_df[order(ms1_df$mz), ]
  
  ms1_df <- ms1_df[order(ms1_df$group), ]
  
  if ("groups" %in% groupBy) {
    ms1_df[["analysis"]] <- NULL
  } else {
    ms1_df <- ms1_df[order(ms1_df$analysis), ]
    data.table::setnames(ms1_df, "analysis", "replicate")
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- fts$group
    ms1_df$name <- tar_ids[ms1_df$group]
  }
  
  data.table::copy(ms1_df)
}

# MARK: get_groups_ms2
## get_groups_ms2 -----
#' @export
#' @noRd
S7::method(get_groups_ms2, NTS) <- function(x,
                                            groups = NULL,
                                            mass = NULL,
                                            mz = NULL,
                                            rt = NULL,
                                            mobility = NULL,
                                            ppm = 20,
                                            sec = 60,
                                            millisec = 5,
                                            isolationWindow = 1.3,
                                            mzClustFeatures = 0.003,
                                            presenceFeatures = 0.8,
                                            minIntensityFeatures = 100,
                                            useLoadedData = TRUE,
                                            mzClust = 0.003,
                                            presence = 0.8,
                                            minIntensity = 100,
                                            groupBy = "groups",
                                            filtered = FALSE) {
  fgs <- get_groups(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec, filtered,
    intensities = FALSE, average = FALSE, sdValues = FALSE, metadata = FALSE
  )
  
  if (nrow(fgs) == 0) {
    return(data.table::data.table())
  }
  
  fts <- get_features(x, features = fgs$group)
  
  if (nrow(fts) == 0) {
    return(data.table::data.table())
  }
  
  ms2 <- get_features_ms2(
    x,
    analyses = unique(fts$analysis),
    features = fts$feature,
    isolationWindow = isolationWindow,
    mzClust = mzClustFeatures,
    presence = presenceFeatures,
    minIntensity = minIntensityFeatures,
    filtered = filtered,
    useLoadedData = useLoadedData
  )
  
  ms2 <- ms2[ms2$intensity > minIntensity, ]
  
  if (nrow(ms2) == 0) {
    return(data.table::data.table())
  }
  
  polarities <- unique(fts$polarity)
  polarities[polarities == 0] <- "unkown"
  polarities[polarities == 1] <- "positive"
  polarities[polarities == -1] <- "negative"
  
  multiple_polarities <- FALSE
  
  # TODO check for polarity switching with comma
  
  if (length(polarities) > 1) multiple_polarities <- TRUE
  
  if ("groups" %in% groupBy) {
    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$group, "_", ms2$polarity)
      ms2$analysis <- NA_character_
    } else {
      ms2$unique_id <- ms2$group
      ms2$analysis <- NA_character_
    }
  } else {
    rpls <- x$replicates
    ms2$analysis <- rpls[ms2$analysis]
    
    if (multiple_polarities) {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group, "", ms2$polarity)
    } else {
      ms2$unique_id <- paste0(ms2$analysis, "_", ms2$group)
    }
  }
  
  ms2$id <- ms2$group
  
  ms2_list <- rcpp_ms_cluster_spectra(ms2, mzClust, presence, verbose = FALSE)
  
  ms2_df <- data.table::rbindlist(ms2_list, fill = TRUE)
  
  ms2_df$group <- ms2_df$id
  
  ms2_df[["id"]] <- NULL
  
  ms2_df <- ms2_df[order(ms2_df$mz), ]
  
  ms2_df <- ms2_df[order(ms2_df$group), ]
  
  if ("groups" %in% groupBy) {
    ms2_df[["analysis"]] <- NULL
  } else {
    ms2_df <- ms2_df[order(ms2_df$analysis), ]
    data.table::setnames(ms2_df, "analysis", "replicate")
  }
  
  if ("name" %in% colnames(fts)) {
    tar_ids <- fts$name
    names(tar_ids) <- fts$group
    ms2_df$name <- tar_ids[ms2_df$group]
  }
  
  data.table::copy(ms2_df)
}

# MARK: plot_groups_ms1
## plot_groups_ms1 -----
#' @export
#' @noRd
S7::method(plot_groups_ms1, NTS) <- function(x,
                                             groups = NULL,
                                             mass = NULL,
                                             mz = NULL,
                                             rt = NULL,
                                             mobility = NULL,
                                             ppm = 20,
                                             sec = 60,
                                             millisec = 5,
                                             rtWindow = c(-2, 2),
                                             mzWindow = c(-5, 90),
                                             mzClustFeatures = 0.005,
                                             presenceFeatures = 0.8,
                                             minIntensityFeatures = 1000,
                                             useLoadedData = TRUE,
                                             mzClust = 0.005,
                                             presence = 0.8,
                                             minIntensity = 1000,
                                             groupBy = "groups",
                                             filtered = FALSE,
                                             legendNames = NULL,
                                             xLab = NULL,
                                             yLab = NULL,
                                             title = NULL,
                                             colorBy = "targets",
                                             showText = FALSE,
                                             interactive = TRUE) {
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }
  
  ms1 <- get_groups_ms1(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec,
    rtWindow, mzWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures,
    useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
  )
  
  if (nrow(ms1) == 0) {
    message("\U2717 MS1 traces not found for the targets!")
    return(NULL)
  }
  
  if ("analyses" %in% colorBy) colorBy <- "replicates"
  if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"
  
  ms1 <- .make_colorBy_varkey(ms1, colorBy, legendNames)
  
  ms1$loop <- paste0(ms1$analysis, ms1$replicate, ms1$id, ms1$var)
  
  cl <- .get_colors(unique(ms1$var))
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    plot <- ggplot2::ggplot(ms1, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var), linewidth = 1)
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = round(mz, 4)),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms1$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ticksMin <- plyr::round_any(min(ms1$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms1$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms1$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms1$intensity) * 1.5)
    )
    
    loop <- NULL
    
    plot <- ms1 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = 0.01)),
        text = ~paste0(round(mz, digits = 4), "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}

# MARK: plot_groups_ms2
## plot_groups_ms2 -----
#' @export
#' @noRd
S7::method(plot_groups_ms2, NTS) <- function(x,
                                             groups = NULL,
                                             mass = NULL,
                                             mz = NULL,
                                             rt = NULL,
                                             mobility = NULL,
                                             ppm = 20,
                                             sec = 60,
                                             millisec = 5,
                                             isolationWindow = 1.3,
                                             mzClustFeatures = 0.003,
                                             presenceFeatures = 0.8,
                                             minIntensityFeatures = 100,
                                             useLoadedData = TRUE,
                                             mzClust = 0.003,
                                             presence = TRUE,
                                             minIntensity = 100,
                                             groupBy = "groups",
                                             filtered = FALSE,
                                             legendNames = NULL,
                                             xLab = NULL,
                                             yLab = NULL,
                                             title = NULL,
                                             colorBy = "targets",
                                             showText = TRUE,
                                             interactive = TRUE) {
  if (grepl("groups", colorBy) || grepl("targets", colorBy)) {
    groupBy <- "groups"
  } else {
    groupBy <- "replicates"
  }
  
  ms2 <- get_groups_ms2(
    x, groups, mass, mz, rt, mobility, ppm, sec, millisec,
    isolationWindow, mzClustFeatures, presenceFeatures, minIntensityFeatures,
    useLoadedData, mzClust, presence, minIntensity, groupBy, filtered
  )
  
  if (nrow(ms2) == 0) {
    message("\U2717 MS2 traces not found for the targets!")
    return(NULL)
  }
  
  if ("analyses" %in% colorBy) colorBy <- "replicates"
  if (grepl("analyses", colorBy) && grepl("targets", colorBy)) colorBy <- "replicates+targets"
  
  ms2 <- .make_colorBy_varkey(ms2, colorBy, legendNames)
  
  ms2$loop <- paste0(ms2$analysis, ms2$replicate, ms2$id, ms2$var)
  
  cl <- .get_colors(unique(ms2$var))
  
  if (showText) {
    ms2$text_string <- paste0(round(ms2$mz, 4))
    ms2$text_string[ms2$is_pre] <- paste0("Pre ", ms2$text_string[ms2$is_pre])
  } else {
    ms2$text_string <- ""
  }
  
  if (!interactive) {
    if (is.null(xLab)) xLab <- expression(italic("m/z ") / " Da")
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 1
    ms2$linesize[ms2$is_pre] <- 2
    
    plot <- ggplot2::ggplot(ms2, ggplot2::aes(x = mz, y = intensity, group = loop)) +
      ggplot2::geom_segment(ggplot2::aes(xend = mz, yend = 0, color = var, linewidth = linesize))
    
    if (showText) {
      plot <- plot + ggplot2::geom_text(
        ggplot2::aes(label = text_string),
        vjust = 0.2, hjust = -0.2, angle = 90, size = 2, show.legend = FALSE
      )
    }
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(ms2$intensity) * 1.5)) +
      ggplot2::labs(title = title, x = xLab, y = yLab) +
      ggplot2::scale_color_manual(values = cl) +
      ggplot2::scale_linewidth_continuous(range = c(1, 2), guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xLab, y = yLab, title = title) + 
      ggplot2::labs(color = colorBy)
    
    plot
    
  } else {
    if (is.null(xLab)) xLab <- "<i>m/z</i> / Da"
    if (is.null(yLab)) yLab <- "Intensity / counts"
    
    ms2$linesize <- 0.01
    ms2$linesize[ms2$is_pre] <- 2
    
    ticksMin <- plyr::round_any(min(ms2$mz, na.rm = TRUE) * 0.9, 10)
    ticksMax <- plyr::round_any(max(ms2$mz, na.rm = TRUE) * 1.1, 10)
    
    title <- list(text = title, font = list(size = 12, color = "black"))
    
    xaxis <- list(
      linecolor = "black", title = xLab, titlefont = list(size = 12, color = "black"),
      range = c(ticksMin, ticksMax),
      dtick = round((max(ms2$mz) / 10), -1),
      ticks = "outside"
    )
    
    yaxis <- list(
      linecolor = "black", title = yLab, titlefont = list(size = 12, color = "black"),
      range = c(0, max(ms2$intensity) * 1.5)
    )
    
    loop <- NULL
    
    plot <- ms2 %>%
      dplyr::group_by(loop) %>%
      plot_ly(
        x = ~mz,
        y = ~intensity,
        type = "bar",
        color = ~var,
        colors = cl,
        marker = list(line = list(width = ~linesize)),
        text = ~paste0(text_string, "  "),
        textposition = "outside",
        textangle = 90,
        textfont = list(size = 9)
      ) %>% plotly::layout(
        bargap = 1,
        title = title,
        xaxis = xaxis,
        yaxis = yaxis,
        barmode = "overlay",
        uniformtext = list(minsize = 6, mode = "show")
      )
    
    plot
  }
}
















# MARK: patRoon Objects
# patRoon Objects -----

# MARK: get_patRoon_features
## get_patRoon_features -----
#' @export
#' @noRd
S7::method(get_patRoon_features, NTS) <- function(x, filtered = FALSE, featureGroups = TRUE) {
  if (!x$has_features) {
    warning("No features found to get!")
    return(NULL)
  }
  
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  
  feature_list <- x$feature_list
  
  feature_list <- lapply(feature_list, function(z) {
    if (!filtered) z <- z[!z$filtered, ]
    data.table::setnames(z, "feature", "ID", skip_absent = TRUE)
    data.table::setnames(z, "rt", "ret", skip_absent = TRUE)
    data.table::setnames(z, "rtmin", "retmin", skip_absent = TRUE)
    data.table::setnames(z, "rtmax", "retmax", skip_absent = TRUE)
    z
  })
  
  feature_list <- feature_list[vapply(feature_list, nrow, 0) > 0]
  
  ana_info <- x$analyses_info
  ana_info <- ana_info[ana_info$analysis %in% names(feature_list), ]
  pols <- x$analyses_info$polarity
  ana_info$path <- dirname(ana_info$file)
  data.table::setnames(ana_info, "replicate", "group", skip_absent = TRUE)
  data.table::setcolorder(ana_info, c("path", "analysis", "group", "blank", "polarity"))
  
  make_set <- FALSE
  
  if (length(unique(pols)) > 1) {
    make_set <- TRUE
  }
  
  if (x$has_groups && featureGroups) {
    
    feature_list <- lapply(feature_list, function(z) {
      z <- z[!is.na(z$group), ]
      z$index <- seq_len(nrow(z))
      z
    })
    
    pat <- new("featuresOpenMS", features = feature_list, analysisInfo = ana_info)
    
    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) z$name <- NULL
      if ("index" %in% colnames(z)) z$index <- NULL
      z
    })
    
    if (make_set) {
      pat_set <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )
    }
    
    fts <- data.table::rbindlist(feature_list, idcol = "analysis")
    
    intensity <- NULL
    groups <- fts[, .(intensity = intensity), by = c("group", "analysis")]
    groups <- data.table::dcast(groups, analysis ~ group, value.var = "intensity", fill = 0)
    if (make_set) {
      groups <- groups[match(pat_set@analysisInfo$analysis, groups$analysis), ]
    } else {
      groups <- groups[match(pat@analysisInfo$analysis, groups$analysis), ]
    }
    groups$analysis <- NULL
    
    index <- NULL
    ftindex <- fts[, .(index = index), by = c("group", "analysis")]
    ftindex <- data.table::dcast(ftindex, analysis ~ group, value.var = "index", fill = 0)
    if (make_set) {
      ftindex <- ftindex[match(pat_set@analysisInfo$analysis, ftindex$analysis), ]
    } else {
      ftindex <- ftindex[match(pat@analysisInfo$analysis, ftindex$analysis), ]
    }
    ftindex$analysis <- NULL
    
    mass <- NULL
    ret <- NULL
    groups_info <- fts[ ,
      .(mass = round(mean(mass), digits = 4), ret = round(mean(ret), digits = 0)),
      by = c("group")
    ]
    groups_info_rows <- groups_info$group
    groups_info[["group"]] <- NULL
    groups_info <- as.data.frame(groups_info)
    rownames(groups_info) <- groups_info_rows
    colnames(groups_info) <- c("mzs", "rts") # Note that here the mzs is still neutral mass
    
    data.table::setcolorder(groups, groups_info_rows)
    data.table::setcolorder(ftindex, groups_info_rows)
    
    if (make_set) {
      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat_set@analysisInfo,
        groupInfo = groups_info,
        features = pat_set,
        ftindex = ftindex
      )
      
      fg_set <- patRoon::featureGroupsSet(
        groupAlgo = "openms",
        groupArgs = list(),
        groupVerbose = FALSE,
        groups = patRoon::groupTable(fg),
        groupInfo = patRoon::groupInfo(fg),
        analysisInfo = patRoon::analysisInfo(fg),
        features = patRoon::getFeatures(fg),
        ftindex = patRoon::groupFeatIndex(fg),
        algorithm = "openms-set"
      )
      
      fg_set@annotations <- patRoon:::getAnnotationsFromSetFeatures(fg_set)

      return(fg_set)
      
    } else {
      if (unique(pols) %in% "positive") {
        groups_info$mzs <- groups_info$mzs + 1.007276
      } else if (unique(pols) %in% "negative") {
        groups_info$mzs <- groups_info$mzs - 1.007276
      } else {
        stop("Polarity should be defined as positive or negative!")
      }
      
      fg <- new(
        "featureGroupsOpenMS",
        groups = groups,
        analysisInfo = pat@analysisInfo,
        groupInfo = groups_info,
        features = pat,
        ftindex = ftindex,
        groupAlgo = "openms"
      )
      
      return(fg)
    }
  } else {
    pat <- new("featuresOpenMS", features = feature_list, analysisInfo = ana_info)
    
    if (make_set) {
      pat <- patRoon::makeSet(
        pat[pols %in% "negative"],
        pat[pols %in% "positive"],
        adducts = list("[M-H]-", "[M+H]+")
      )
      
      pat@analysisInfo <- pat@analysisInfo[order(pat@analysisInfo$analysis), ]
      
      pat@features <- pat@features[pat@analysisInfo$analysis]
    }
    
    pat@features <- lapply(pat@features, function(z) {
      if ("name" %in% colnames(z)) z$name <- NULL
      if ("index" %in% colnames(z)) z$index <- NULL
      z
    })
    
    return(pat)
  }
}

# MARK: get_patRoon_MSPeakLists
## get_patRoon_MSPeakLists -----
#' @export
#' @noRd
S7::method(get_patRoon_MSPeakLists, NTS) <- function(x,
                                                     clusterMzWindow = 0.005,
                                                     topMost = 100,
                                                     minIntensityPre = 50,
                                                     minIntensityPost = 50,
                                                     avgFun = "mean",
                                                     method = "distance") {
  if (!x$has_features) {
    warning("No features found to get!")
    return(NULL)
  }
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found! Install it for finding features.")
    return(FALSE)
  }
  parameters <- list(
    clusterMzWindow = clusterMzWindow,
    topMost = topMost,
    minIntensityPre = minIntensityPre,
    minIntensityPost = minIntensityPost,
    avgFun = avgFun,
    method = method
  )
  parameters$avgFun <- get(parameters$avgFun)
  mspl <- .convert_ms1_ms2_columns_to_MSPeakLists(x, parameters)
  mspl
}

# MARK: report
## report -----
#' @export
#' @noRd
S7::method(report, NTS) <- function(x,
                                    path = paste0(getwd(), "/report"),
                                    filtered = FALSE,
                                    settingsFile = system.file(
                                      "report",
                                      "settings.yml",
                                      package = "patRoon"
                                    ),
                                    eicRtWindow = 30,
                                    eicTopMost = 1,
                                    eicTopMostByRGroup = TRUE,
                                    eicOnlyPresent = TRUE,
                                    eicMzExpWindow = 0.001,
                                    adductPos = "[M+H]+",
                                    adductNeg = "[M-H]-",
                                    specSimMethod = "cosine",
                                    specSimRemovePrecursor = FALSE,
                                    specSimMzWeight = 0,
                                    specSimIntWeight = 1,
                                    specSimAbsMzDev = 0.005,
                                    specSimRelMinIntensity = 0.05,
                                    specSimMinPeaks = 1,
                                    specSimShift = "none",
                                    specSimCombineMethod = "mean",
                                    clearPath = FALSE,
                                    openReport = TRUE,
                                    parallel = TRUE) {
  if (!requireNamespace("patRoon", quietly = TRUE)) {
    warning("patRoon package not found!")
    return(NULL)
  }

  if (!x$has_groups) {
    warning("No feature groups found to report!")
    return(NULL)
  }

  pat <- x$get_patRoon_features(filtered = filtered, featureGroups = TRUE)

  patRoon::report(
    x$features[[1]],
    MSPeakLists = NULL,
    formulas = NULL,
    compounds = NULL,
    compsCluster = NULL,
    components = NULL,
    TPs = NULL,
    settingsFile = settingsFile,
    path = path,
    EICParams = list(
      rtWindow = eicRtWindow,
      topMost = eicTopMost,
      topMostByRGroup = eicTopMostByRGroup,
      onlyPresent = eicOnlyPresent,
      mzExpWindow = eicMzExpWindow,
      setsAdductPos = adductPos,
      setsAdductNeg = adductNeg
    ),
    specSimParams = list(
      method = specSimMethod,
      removePrecursor = specSimRemovePrecursor,
      mzWeight = specSimMzWeight,
      intWeight = specSimIntWeight,
      absMzDev = specSimAbsMzDev,
      relMinIntensity = specSimRelMinIntensity,
      minPeaks = specSimMinPeaks,
      shift = specSimShift,
      setCombineMethod = specSimCombineMethod
    ),
    clearPath = clearPath,
    openReport = openReport,
    parallel = parallel,
    overrideSettings = list()
  )

  message("\U2713 Report generated!")
}

# MARK: Utility functions
# Utility functions -----

# MARK: .add_features_column
## .add_features_column -----
#' @noRd
.add_features_column <- function(NTS = NULL, name = NULL, data = NULL) {
  if (!is(NTS, "StreamFind::NTS")) {
    warning("NTS object is not of class NTS! Not done.")
    return(NTS)
  }
  if (NTS@has_features) {
    feature_list <- NTS@feature_list
    feature_list <- Map(function(x, y) {
      if (nrow(x) == length(y)) x[[name]] <- y
      x
    }, feature_list, data)
    NTS$feature_list <- feature_list
  } else {
    warning("No features found! Not done.")
  }
  NTS
}

# MARk: .make_features_hover_string
## .make_features_hover_string -----
#' @noRd
.make_features_hover_string <- function(pk = data.table::data.table()) {
  if (nrow(pk) == 0) return("")
  
  has_quality <- any(vapply(pk$quality, function(z) nrow(z) > 0, logical(1)))
  has_annotation <- any(vapply(pk$annotation, function(z) nrow(z) > 0, logical(1)))
  hT <- paste(
    "</br> feature: ", pk$feature,
    ifelse("group" %in% colnames(pk), paste("</br> group: ", pk$group), ""),
    "</br> analysis: ", pk$analysis,
    "</br> replicate: ", pk$replicate,
    "</br> mass: ", round(pk$mass, digits = 4),
    "</br> <i>m/z</i>: ", round(pk$mz, digits = 4),
    "</br> dppm: ", round(((pk$mzmax - pk$mzmin) / pk$mz) * 1E6, digits = 0),
    "</br> rt: ", round(pk$rt, digits = 0),
    "</br> drt: ", round(pk$rtmax - pk$rtmin, digits = 0),
    "</br> intensity: ", round(pk$intensity, digits = 0),
    "</br> filtered: ", pk$filtered,
    "</br> filled: ", pk$filled,
    if (has_quality) {
      paste(
        "</br> noise: ",  vapply(pk$quality, function(z) {
          if (nrow(z) > 0) round(z$noise, digits = 0) else NA_real_
        }, NA_real_),
        "</br> sn: ", vapply(pk$quality, function(z) {
          if (nrow(z) > 0) round(z$sn, digits = 1) else NA_real_
        }, NA_real_),
        "</br> gaufit: ", vapply(pk$quality, function(z) {
          if (nrow(z) > 0) round(z$gauss_f, digits = 4) else NA_real_
        }, NA_real_),
        "</br> A: ", vapply(pk$quality, function(z) {
          if (nrow(z) > 0) round(z$gauss_a, digits = 2) else NA_real_
        }, NA_real_),
        "</br> mu: ", vapply(pk$quality, function(z) {
          if (nrow(z) > 0)  round(z$gauss_u, digits = 2) else NA_real_
        }, NA_real_),
        "</br> sigma: ", vapply(pk$quality, function(z) {
          if (nrow(z) > 0) round(z$gauss_s, digits = 2) else NA_real_
        }, NA_real_)
      )
    } else {
      ""
    },
    if (has_annotation) {
      paste(
        "</br> component: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) z$component_feature else NA_character_
        }, NA_character_),
        "</br> isotope: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) z$iso_cat else NA_character_
        }, NA_character_
        ),
        "</br> iso_elements: ", vapply(pk$annotation, function(z) {
          z$iso_isotope
        }, NA_character_),
        "</br> iso_number_carbons: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) round(z$iso_number_carbons, digits = 0) else NA_real_
        }, NA_real_),
        "</br> iso_mass_error: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) round(z$iso_mass_error, digits = 5) else NA_real_
        }, NA_real_),
        "</br> iso_time_error: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) round(z$iso_time_error, digits = 1) else NA_real_
        }, NA_real_),
        "</br> adduct: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) z$adduct_cat else NA_character_
        }, NA_character_),
        "</br> adduct_mass_error: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0)  round(z$adduct_mass_error, digits = 5) else NA_real_
        }, NA_real_),
        "</br> adduct_time_error: ", vapply(pk$annotation, function(z) {
          if (nrow(z) > 0) round(z$adduct_time_error, digits = 1) else NA_real_
        }, NA_real_)
      )
    } else {
      ""
    }
    # if ("dqsPeak" %in% colnames(pk)) {
    #   paste("</br> DQS: ", pk$dqsPeak)
    # } else {
    #   ""
    # },
  )
  
  hT
}
