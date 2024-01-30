
#' .merge_replicate_files
#'
#' @param self A Raman object.
#' @param preCut The number of pre Raman scans to exclude when merging.
#'
#' @return Logical of length one.
#'
#' @noRd
#'
.merge_replicate_files <- function(raman, preCut = 2) {
  
  rpls <- raman$get_replicate_names()
  
  urpls <- unique(rpls)
  
  unified <- lapply(urpls, function(x) {
    
    anas <- names(rpls)[rpls %in% x]
    
    anasl <- raman$get_analyses(anas)
    
    cached_merged_analysis <- FALSE
    
    merged_analysis <- NULL
    
    if (.caches_data()) {
      hash <- patRoon::makeHash(x, anas, anasl)
      
      merged_analysis <- patRoon::loadCacheData("merged_raman_analysis", hash)
      
      if (!is.null(merged_analysis)) {
        message("\U2139 Merged Raman analysis loaded from cache!")
        cached_merged_analysis <- TRUE
      }
      
    } else {
      hash <- NULL
      merged_analysis <- NULL
    }
    
    if (is.null(merged_analysis) & !cached_merged_analysis) {
      rtvec <- vapply(anasl, function(z) {
          as.numeric(z$metadata$`Accumulate Cycle Time (secs)`)
        }, NA_real_
      )
      
      rtvec <- cumsum(unname(rtvec))
      
      spectral <- lapply(anasl, function(z) z$spectra)
      
      spectral <- spectral[-(1:preCut)]
      
      names(spectral) <- as.character(rtvec[-(1:preCut)])
      
      spectra <- rbindlist(spectral, idcol = "rt")
      
      spectra$rt <- as.numeric(spectra$rt)
      
      setcolorder(spectra, c("rt"))
      
      message("\U2699 Writting unified analysis file..." , appendLF = FALSE)
      
      ana_name <- x
      
      ana_dir <- dirname(anasl[[1]]$file)
      
      ana_ext <- file_ext(anasl[[1]]$file)
      
      new_file <- paste0(ana_dir, "/", ana_name, ".", ana_ext)
      
      ana_metadata <- anasl[[1]]$metadata
      
      if (file.exists(new_file)) file.remove(new_file)
      
      rcpp_write_asc_file(file = new_file, ana_metadata, as.matrix(spectra))
      
      merged_analysis <- list(
        "name" = ana_name,
        "replicate" = ana_name,
        "blank" = NA_character_,
        "file" = new_file,
        "metadata" = ana_metadata,
        "spectra" = spectra
      )
      
      message(" Done!")
      
      if (!is.null(hash)) {
        patRoon::saveCacheData("merged_raman_analysis", merged_analysis, hash)
        message("\U1f5ab Merged Raman analysis cached!")
      }
      
    } else {
      if (!file.exists(merged_analysis$file)) {
        message("\U2699 Writting unified analysis file..." , appendLF = FALSE)
        
        rcpp_write_asc_file(
          file = merged_analysis$file,
          merged_analysis$metadata,
          as.matrix(merged_analysis$spectra)
        )
        
        message(" Done!")
      }
    }

    message("\U2699 Removing ", length(anas),  " analyses..." , appendLF = FALSE)
    
    suppressMessages(raman$remove_analyses(anas))
    
    message(" Done!")
    
    raman$add_analyses(merged_analysis)
    
    TRUE
  })
  
  all(unlist(unified))
}

#' .plot_raman_spectra_static
#' 
#' @noRd
#'
.plot_raman_spectra_static <- function(spectra, xLab, yLab, title, cex, showLegend) {
  
  
  cl <- .get_colors(unique(spectra$var))
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  xr <- c(min(spectra$shift), max(spectra$shift))
  if (showLegend) {
    xr[2] <- xr[2] * 1.01
  }
  
  intr <- c(0, max(spectra$intensity))
  
  if (is.null(cex) || !is.numeric(cex)) cex <- 1
  
  plot(spectra$shift,
       type = "n",
       xlab = xLab,
       ylab = yLab,
       xlim = xr,
       ylim = intr,
       main = title
  )
  
  for (t in loop_key) {
    
    select_vector <- spectra$loop %in% t
    
    lt <- unique(spectra$var[select_vector])
    
    lines(
      x = spectra$shift[select_vector],
      y = spectra$intensity[select_vector],
      type = "l",
      pch = 19,
      cex = 0.5,
      col = cl[lt]
    )
  }
  
  if (showLegend) {
    legend(
      x = "topright",
      legend = names(cl),
      col = cl,
      lwd = 2,
      lty = 1,
      cex = cex,
      bty = "n"
    )
  }
}

#' .plot_raman_spectra_interactive
#' 
#' @noRd
#'
.plot_raman_spectra_interactive <- function(spectra, xLab, yLab, title, colorBy) {
  
  leg <- unique(spectra$var)
  
  cl <- .get_colors(leg)
  
  spectra$loop <- paste0(spectra$analysis, spectra$id, spectra$var)
  
  loop_key <- unique(spectra$loop)
  
  title <- list(
    text = title, x = 0.13, y = 0.98,
    font = list(size = 12, color = "black")
  )
  
  xaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = xLab,
    titlefont = list(size = 12, color = "black")
  )
  
  yaxis <- list(
    linecolor = toRGB("black"),
    linewidth = 2, title = yLab,
    titlefont = list(size = 12, color = "black")
  )
  
  plot <- plot_ly()
  
  showL <- rep(TRUE, length(leg))
  
  names(showL) <- leg
  
  for (t in loop_key) {
    select_vector <- spectra$loop %in% t
    lt <- unique(spectra$var[select_vector])
    x <- spectra$rt[select_vector]
    y <- spectra$intensity[select_vector]
    
    plot <- plot %>% add_trace(
      x = x,
      y = y,
      type = "scatter", mode = "lines+markers",
      line = list(width = 0.5, color = unname(cl[lt])),
      marker = list(size = 2, color = unname(cl[lt])),
      name = lt,
      legendgroup = lt,
      showlegend = showL[lt],
      hovertemplate = paste("<br>x: %{x}<br>", "y: %{y}")
    )
    
    if (length(y) >= 1) showL[lt] <- FALSE
  }
  
  plot <- plot %>% plotly::layout(
    legend = list(title = list(text = paste("<b>", colorBy, "</b>"))),
    xaxis = xaxis,
    yaxis = yaxis,
    title = title
  )
  
  plot
}

#' .parse_asc_file
#'
#' @param file Character with the file full path.
#'
#' @return A list.
#'
#' @noRd
#'
.parse_asc_file <- function(file) {
  text_data <- readLines(file)
  
  metadata_list <- list()
  
  data_values <- data.frame()
  
  for (line in text_data) {
    # Extract metadata
    if (grepl(":", line)) {
      metadata <- strsplit(line, ":\\s+")[[1]]
      metadata_list[[metadata[1]]] <- metadata[2]
      
    } else if (grepl("^-?\\d+\\.\\d+;", line)) {
      values <- strsplit(line, ";")[[1]]
      data_values <- rbind(data_values, as.numeric(values))
    }
  }
  
  colnames(data_values) <- c("shift", "intensity")
  
  f_name <- basename(file)
  f_ext <- file_ext(f_name)
  f_name <- sub(paste0(".", f_ext), "", f_name)
  
  list(
    "name" = f_name,
    "replicate" = f_name,
    "blank" = NA_character_,
    "file" = file,
    "metadata" = metadata_list,
    "spectrum" = data_values
  )
}
