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

# MARK: plot_features_eic
## plot_features_eic -----
#' @export
#' @noRd
S7::method(plot_features_eic, NTS) <- function(x,
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
                                               showLegend = TRUE,
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
  eic <- eic[, `:=`(intensity = max(intensity)), by = c("analysis", "replicate", "polarity", "feature", "rt")][]
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
      ft_analysis <- fts[["analysis"]][i]
      ft_replicate <- fts[["replicate"]][i]
      ft_id <- fts[["feature"]][i]
      ft_var <- fts[["var"]][i]
      ft_min <- fts[["rtmin"]][i]
      ft_max <- fts[["rtmax"]][i]
      
      # ft_sn <- fts[["sn"]][i]
      
      temp <- dplyr::filter(
        eic,
        analysis %in% ft_analysis &
        replicate %in% ft_replicate &
        feature %in% ft_id &
        rt >= ft_min &
        rt <= ft_max
      )
      
      plot <- plot %>% add_trace(
        data = temp,
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        mode = "markers",
        marker = list(color = cl[ft_var], size = 5),
        text = ~paste(
          "<br>analysis: ", ft_analysis,
          "<br>replicate: ", ft_replicate,
          "<br>feature: ", ft_id,
          "<br>rt: ", round(rt, 0),
          "<br>intensity: ", round(intensity, 0)
        ),
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
        text = ~paste(
          "<br>analysis: ", ft_analysis,
          "<br>replicate: ", ft_replicate,
          "<br>feature: ", ft_id,
          "<br>rt: ", round(rt, 0),
          "<br>intensity: ", round(intensity, 0)
        ),
        hoverinfo = "text",
        name = ft_var,
        legendgroup = ft_var,
        showlegend = show_legend[ft_var]
      )
      
      show_legend[ft_var] <- FALSE
    }
    
    for (i in seq_len(nrow(fts))) {
      ft_analysis <- fts[["analysis"]][i]
      ft_replicate <- fts[["replicate"]][i]
      ft_var <- fts[["var"]][i]
      
      plot <- plot %>% add_trace(
        data = dplyr::filter(
          eic,
          analysis %in% ft_analysis &
          replicate %in% ft_replicate &
          feature %in% ft_id
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
