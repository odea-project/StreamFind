#' @title RamanMethod_SubtractBlankSpectra_StreamFind Class
#'
#' @description Subtracts the blank spectra to each analysis according to the blank assignment.
#'
#' @param negativeToZero Logical (length 1) indicating if negative values should be set to zero.
#'
#' @return A RamanMethod_SubtractBlankSpectra_StreamFind object.
#'
#' @export
#'
RamanMethod_SubtractBlankSpectra_StreamFind <- function(
  negativeToZero = FALSE
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "SubtractBlankSpectra",
    algorithm = "StreamFind",
    parameters = list(negativeToZero = negativeToZero),
    number_permitted = 1,
    version = as.character(packageVersion("StreamFind")),
    software = "StreamFind",
    developer = "Ricardo Cunha",
    contact = "cunha@iuta.de",
    link = "https://odea-project.github.io/StreamFind",
    doi = NA_character_
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid RamanMethod_SubtractBlankSpectra_StreamFind object!")
  }
}

#' @describeIn RamanMethod_SubtractBlankSpectra_StreamFind Validate the RamanMethod_SubtractBlankSpectra_StreamFind object, returning NULL if valid.
#' @param x A RamanMethod_SubtractBlankSpectra_StreamFind object.
#' @export
#'
validate_object.RamanMethod_SubtractBlankSpectra_StreamFind <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "SubtractBlankSpectra")
  checkmate::assert_choice(x$algorithm, "StreamFind")
  checkmate::assert_logical(x$parameters$negativeToZero, max.len = 1)
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_SubtractBlankSpectra_StreamFind <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$Analyses$has_spectra) {
    warning("No spectra results object available! Not done.")
    return(FALSE)
  }

  spec_list <- engine$Spectra$spectra

  spec_list <- Map(
    function(i, j) {
      i$analysis <- j
      i
    },
    spec_list,
    names(spec_list)
  )

  ntozero <- x$parameters$negativeToZero

  blks <- get_blank_names(engine$Analyses)

  names(blks) <- get_replicate_names(engine$Analyses)

  blk_anas <- get_replicate_names(engine$Analyses)
  blk_anas <- blk_anas[blk_anas %in% blks]

  spec_blk <- spec_list[names(spec_list) %in% c(blks, names(blk_anas))]

  if (length(spec_blk) == length(unique(blks))) {
    names(spec_blk) <- unique(blks)
  } else if (length(spec_blk) == length(blk_anas)) {
    names(spec_blk) <- blk_anas
  } else {
    warning("Blank spectra not found! Not done.")
    return(FALSE)
  }

  spec_sub <- lapply(spec_list, function(z) {
    if (nrow(z) == 0) {
      return(z)
    }

    if (!engine$Spectra$is_averaged) {
      rp <- get_replicate_names(engine$Analyses)[z$analysis[1]]
    } else {
      rp <- z$analysis[1]
    }

    if (rp %in% blks) {
      return(data.table::data.table())
    }

    blk <- spec_blk[names(spec_blk) %in% blks[rp]]

    if (length(blk) > 1) {
      intensity <- NULL
      blk <- rbindlist(blk)
      blk[["analysis"]] <- NULL
      blk[["replicate"]] <- NULL
      blk[["polarity"]] <- NULL
      blk[["level"]] <- NULL
      blk[["pre_mz"]] <- NULL
      blk[["pre_ce"]] <- NULL

      merge_vals <- character()
      if ("shift" %in% colnames(blk)) {
        merge_vals <- c(merge_vals, "shift")
      }
      if ("rt" %in% colnames(blk)) {
        merge_vals <- c(merge_vals, "rt")
      }
      if ("mz" %in% colnames(blk)) {
        merge_vals <- c(merge_vals, "mz")
      }

      blk <- blk[, intensity := mean(intensity), by = merge_vals]

      blk <- unique(blk)

      blk <- blk$intensity
    } else {
      blk <- blk[[1]]$intensity
    }

    if (length(blk) != nrow(z)) {
      warning("Spectra do not have the same dimention! Not done.")
      return(z)
    }

    z$blank <- blk

    z$intensity <- z$intensity - blk

    if (ntozero) {
      z$intensity[z$intensity < 0] <- 0
    }

    z$analysis <- NULL
    z <- unique(z)

    z
  })

  engine$Spectra$spectra <- spec_sub
  message(paste0("\U2713 ", "Blank spectra subtracted in spectra!"))
  invisible(TRUE)
}
