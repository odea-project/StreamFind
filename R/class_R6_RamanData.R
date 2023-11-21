#' **RamanData** R6 class and methods
#'
#' @description
#' The RamanData R6 class is a framework with methods for parsing, processing,
#' inspecting and storing RAMAN data.
#'
#' @template arg-headers
#' @template arg-ms-analyses
#' @template arg-raman-shift
#' @template arg-ms-title
#'
#' @export
#'
RamanData <- R6::R6Class("RamanData",

  # private fields -----
  private = list(

    ## .headers -----
    .headers = NULL,

    ## .settings -----
    .settings = NULL,

    ## .analyses -----
    .analyses = NULL,

    ## .averaged -----
    .averaged = NULL,

    ## ___ .utils -----

    # Checks the analyses argument as a character/integer vector to match
    # analyses names or indices from the `RamanData` object. Returns a valid
    # character vector with analysis names or `NULL` for non-matching.
    #
    .check_analyses_argument = function(analyses = NULL) {
      if (is.null(analyses)) {
        self$get_analysis_names()
      } else {
        analyses <- self$get_analysis_names(analyses)
        if (!all(analyses %in% self$get_analysis_names())) {
          warning("Defined analyses not found!")
          NULL
        } else {
          analyses
        }
      }
    },

    # Gets an entry from the analyses private field.
    #
    .get_analyses_entry = function(analyses = NULL, value = NA_character_) {
      analyses <- private$.check_analyses_argument(analyses)

      if (is.null(analyses)) return(NULL)
      output <- lapply(private$.analyses, function(x, value) {

        temp <- x[[value]]
        names(temp) <- rep(x$name, length(temp))
        temp

      }, value = value)

      output <- unname(output)

      output <- unlist(output, recursive = FALSE, use.names = TRUE)

      output[names(output) %in% analyses]
    }
  ),

  # public fields/methods -----
  public = list(

    ## system -----
    #' @description
    #' Creates an R6 RamanData class object. When `headers` are not given
    #' (i.e., `NULL`), a default Headers S3 class object is generated with name
    #' as `NA_character`, path as `get_wd()` and date as `Sys.time()`.
    #' See `?Headers` for more information.
    #'
    #' @param files Full file paths of Raman analyses.
    #'
    initialize = function(files = NULL, headers = NULL) {

      if (!is.null(headers)) suppressMessages(self$add_headers(headers))

      if (is.null(private$.headers)) {
        private$.headers <- ProjectHeaders(
          name = NA_character_,
          path = getwd(),
          date = Sys.time()
        )
      }

      if (!is.null(files)) {

        analyses <- lapply(files, function(x) {
          
          text_data <- readLines(x)
          
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

          f_name <- basename(x)
          f_ext <- file_ext(f_name)
          f_name <- sub(paste0(".", f_ext), "", f_name)

          list(
            "name" = f_name,
            "replicate" = f_name,
            "blank" = NA_character_,
            "file" = x,
            "metadata" = metadata_list,
            "spectrum" = data_values
          )
        })

        names(analyses) <- vapply(analyses, function(x) x$name, "")

        private$.analyses <- analyses
      }

      message("\U2713 RamanData class object created!")
    },

    #' @description
    #' Prints a summary of the `RamanData` object in the console.
    #'
    #' @return Console text.
    #'
    print = function() {
      cat(
        paste(is(self), collapse = "; "), "\n",
        "name          ", private$.headers$name, "\n",
        "author        ", private$.headers$author, "\n",
        "path          ", private$.headers$path, "\n",
        "date          ", as.character(private$.headers$date), "\n",
        sep = ""
      )

      cat("\n")

      if (length(private$.analyses) > 0) {
        overview <- self$get_overview()
        overview$file <- NULL
        cat("Analyses: \n")
        row.names(overview) <- paste0(" ", seq_len(nrow(overview)), ":")
        print(overview)

      } else {
        cat("Analyses: ", 0, "\n", sep = "")
      }
      cat("\n")
    },

    ## get -----
    #' @description
    #' Gets the headers.
    #'
    #' @param value A character vector with the name/s of the header elements.
    #' When `NULL`, the entire headers list is returned.
    #'
    #' @return The headers list or the header elements as defined by `value`.
    #'
    get_headers = function(value = NULL) {
      if (is.null(value)) {
        private$.headers
      } else {
        private$.headers[value]
      }
    },

    #' @description
    #' Gets analyses.
    #'
    #' @return The list of analyses or the analyses as defined by `analyses`
    #' argument.
    #'
    get_analyses = function() {
      private$.analyses
    },

    #' @description
    #' Gets the number of analyses present.
    #'
    #' @return An integer value.
    #'
    get_number_analyses = function() {
      length(private$.analyses)
    },

    #' @description
    #' Gets the analysis names.
    #'
    #' @param analyses X.
    #'
    #' @return A character vector.
    #'
    get_analysis_names = function(analyses = NULL) {
      if (length(private$.analyses) > 0) {
        ana <- vapply(private$.analyses, function(x) x$name, "")
        names(ana) <- vapply(private$.analyses, function(x) x$name, "")
        if (!is.null(analyses)) {
          ana[analyses]
        } else {
          ana
        }
      } else {
        NULL
      }
    },
    
    #' @description
    #' Gets the analysis replicate names.
    #'
    #' @return A character vector.
    #'
    get_replicate_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "replicate")
    },
    
    #' @description
    #' Gets the analysis blank replicate names.
    #'
    #' @return A character vector.
    #'
    get_blank_names = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "blank")
    },

    #' @description
    #' Gets the full file paths of the analyses.
    #'
    #' @param analyses X.
    #'
    #' @return A character vector.
    #'
    get_files = function(analyses = NULL) {
      private$.get_analyses_entry(analyses, "file")
    },

    #' @description
    #' Gets spectra from analyses.
    #'
    #' @return A data.frame.
    #'
    get_spectra = function(analyses = NULL, shift = NULL) {

      analyses <- private$.check_analyses_argument(analyses)
      if (is.null(analyses)) return(data.frame())

      spec <- lapply(private$.analyses[analyses], function(x) x$spectrum)
      spec <- rbindlist(spec, idcol = "analysis", fill = TRUE)

      if (!is.null(shift) && length(shift) == 2) {
        shift_range <- sort(shift)
        spec <- spec[shift >= shift_range[1] & shift <= shift_range[2], ]
      }

      spec
    },

    #' @description
    #' Gets the overview data.frame with all the analysis types,
    #' names, replicates, associated blank replicates and full
    #' file paths.
    #'
    #' @return A data.frame with columns type, analysis, replicate, blank and
    #' file.
    #'
    get_overview = function() {

      if (length(private$.analyses) > 0) {
        df <- data.frame(
          "analysis" = vapply(private$.analyses, function(x) x$name, ""),
          "replicate" = vapply(private$.analyses, function(x) x$replicate, ""),
          "blank" = vapply(private$.analyses, function(x) x$blank, ""),
          "size" = vapply(private$.analyses, function(x) nrow(x$spectrum), 0),
          "file" = vapply(private$.analyses, function(x) x$file, "")
        )
        row.names(df) <- seq_len(nrow(df))
        df
      } else {
        data.frame()
      }
    },

    ## add -----
    #' @description
    #' Adds headers. If an argument or element "name" is given, it must
    #' be type character. If an argument or element path is given, it must be
    #' type character and exist. If an argument or element date is given, it
    #' must be class POSIXct or POSIXt. If given date is character, conversion
    #' to class POSIXct or POSIXt is attempted. See `?Headers` for more
    #' information.
    #'
    #' @template arg-headers-ellipsis
    #'
    #' @return Invisible.
    #'
    add_headers = function(...) {

      headers <- ProjectHeaders(...)

      if (is(headers, "ProjectHeaders")) {
        old_headers <- private$.headers
        if (is.null(old_headers)) old_headers <- list()

        if (length(old_headers) > 0) {
          new_headers <- old_headers[!names(old_headers) %in% names(headers)]
          new_headers[names(headers)] <- headers
        } else {
          new_headers <- headers
        }

        new_headers <- as.ProjectHeaders(new_headers)

        if (!identical(new_headers, old_headers) &
              is(new_headers, "ProjectHeaders")) {
          private$.headers <- new_headers
          message("\U2713 Added headers!")
        }

      } else {
        warning("Invalid headers content or structure! Not added.")
      }
      invisible(self)
    },
    
    #' @description
    #' Adds or redefines the analysis replicate names.
    #'
    #' @param value A character vector with the analysis replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_replicate_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        private$.analyses <- Map(
          function(x, y) {
            x$replicate <- y
            x
          },
          private$.analyses, value
        )
        
        message("\U2713 Replicate names added!")
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    #' @description
    #' Adds or redefines the analysis blank replicate names.
    #'
    #' @param value A character vector with the analysis blank replicate names.
    #' Must be of the same length as the number of analyses.
    #'
    #' @return Invisible.
    #'
    add_blank_names = function(value = NULL) {
      if (is.character(value) & length(value) == self$get_number_analyses()) {
        
        if (all(value %in% self$get_replicate_names())) {
          private$.analyses <- Map(
            function(x, y) {
              x$blank <- y
              x
            },
            private$.analyses, value
          )
          
          message("\U2713 Blank names added!")
          
        } else {
          warning("Not done, blank names not among replicate names!")
        }
        
      } else {
        warning("Not done, check the value!")
      }
      invisible(self)
    },
    
    ## plot -----
    
    #' @description
    #' Plots spectra for given RAMAN analyses.
    #'
    #' @param colorBy A string of length 1. One of `analyses` (the default) or 
    #' `replicates`.
    #'
    #' @return A 3D interactive plot.
    #' 
    plot_spectra = function(analyses = NULL,
                            shift = NULL,
                            colorBy = "analyses",
                            title = NULL) {
      
      spectra <- self$get_spectra(analyses, shift)
      
      spectra <- .make_colorBy_varkey(spectra, colorBy, legendNames = NULL)
      
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
        linewidth = 2, title = "Raman shift / cm<sup>-1</sup>",
        titlefont = list(size = 12, color = "black")
      )
      
      yaxis <- list(
        linecolor = toRGB("black"),
        linewidth = 2, title = "Intensity / a.u.",
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
          hovertemplate = paste("<br>shift: %{x}<br>", "intensity: %{y}")
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
  )
)
