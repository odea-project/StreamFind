#' @title .s3_ms_suspect_screening.Settings_suspect_screening_StreamFind
#'
#' @description Makes suspect screening on features.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_StreamFind <- function(settings, self) {

  if (!validate(settings)) return(FALSE)

  suspect_features <- self$get_suspects(
    database = settings$parameters$database,
    ppm = settings$parameters$ppm,
    sec = settings$parameters$sec
  )

  suspect_groups <- self$get_groups(groups = unique(suspect_features$group))

  if (nrow(suspect_features) > 0) {
    suspects_data <- list(
      "suspect_features" = suspect_features,
      "suspect_groups" = suspect_groups,
      "database" = settings$parameters$database
    )

    output <- list(suspects_data)
    names(output) <- settings$call

    self$add_modules_data(output)

    TRUE
  } else {
    FALSE
  }
}

#' @title .s3_ms_suspect_screening.Settings_suspect_screening_forident
#'
#' @description Makes suspect screening on features.
#'
#' @noRd
#'
.s3_ms_suspect_screening.Settings_suspect_screening_forident <- function(settings, self) {

  if (!validate(settings)) return(FALSE)

  if (self$has_groups()) {

    polarities <- unique(self$get_polarities())

    if (length(polarities) > 1 & settings$parameters$addMS2) {
      warning("Using ms2 data of feature groups from multiple polarities is not possible! Using features of each analysis instead.")

      out_list <- self$get_features()
      out_list$rt <- out_list$rt / 60
      out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
    }

    out_list <- self$get_groups()
    out_list$rt <- out_list$rt / 60
    out_list$label <- out_list$group

  } else {
    out_list <- self$get_features()
    out_list$rt <- out_list$rt / 60
    out_list$label <- paste0(out_list$analysis, "_" , out_list$feature)
  }

  if (settings$parameters$useNeutralMass & "mass" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mass

  } else if ("mz" %in% colnames(out_list)) {
    out_list$Mass <- out_list$mz

  } else {
    out_list$Mass <- out_list$mass
  }

  if (!settings$parameters$addMS2) {

    sink(paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))
    cat("\n")
    cat("\n")

    for (i in seq_len(nrow(out_list))) {
      cat("NAME: ")
      cat(out_list$label[i])
      cat("\n")
      cat("RETENTIONTIME: ")
      cat(round(out_list$rt[i], digits = 3))
      cat("\n")
      cat("Mass: ")
      cat(round(out_list$Mass[i], digits = 4))
      cat("\n")
      cat("Formula: ")
      cat("\n")
      cat("//")
      cat("\n")
      cat("\n")
    }
    sink()

    message("\U2713 List saved as ", paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))

  } else {

    if ("ms2" %in% colnames(out_list)) {

      if (!"mz" %in% colnames(out_list)) {
        warning("m/z values not found in the feature groups data.table but it is required for inclusion of fragments! .txt files not exported.")
        return(FALSE)
      }

      out_list$Mass <- out_list$mz

      sink(paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))
      cat("\n")
      cat("\n")

      for (i in seq_len(nrow(out_list))) {
        cat("NAME: ")
        cat(out_list$label[i])
        cat("\n")
        cat("RETENTIONTIME: ")
        cat(round(out_list$rt[i], digits = 3))
        cat("\n")
        cat("PRECURSORMZ: ")
        cat(round(out_list$Mass[i], digits = 4))
        cat("\n")
        cat("Formula: ")
        cat("\n")
        if (is.data.frame(out_list$ms2[[i]])) {
          temp_ms2 <- out_list$ms2[[i]]
          for (j in seq_len(nrow(temp_ms2))) {
            cat(
              paste(
                round(temp_ms2$mz[j], digits = 4),
                round(temp_ms2$intensity[j], digits = 0),
                sep = " "
              )
            )
            cat(" ")
          }
          rm(j, temp_ms2)
        } else {
          cat("N/A")
        }
        cat("\n")
        cat("//")
        cat("\n")
        cat("\n")
      }
      sink()

      message("\U2713 List saved as ", paste0(settings$parameters$path,"/", settings$parameters$name, ".txt"))

    } else {
      warning("MS2 data not found! Load features and/or feature groups MS2 data.")
      return(FALSE)
    }
  }

  TRUE
}
