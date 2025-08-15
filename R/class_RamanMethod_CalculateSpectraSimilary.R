#' @title RamanMethod_CalculateSpectraSimilary_hqi Class
#'
#' @description Calculate the similarity between Raman spectra based on predefined reference
#' spectra. The similarity is assessed by estimating the high quality index (HQI) and P-value
#' between a given analysis and the associated reference.
#'
#' @param reference data.frame with the reference spectra to be used for similarity. The data.frame
#' should have two columns with shift and intensity values. Note that the shift values should match
#' the shift values of the analyses.
#' @param method character(1) indicating the method to be used for the similarity calculation. The
#' available methods are "pearson", "kendall", and "spearman".
#'
#' @return A RamanMethod_CalculateSpectraSimilary_hqi object.
#'
#' @export
#'
RamanMethod_CalculateSpectraSimilary_hqi <- function(
  reference = data.frame(),
  method = "pearson"
) {
  x <- ProcessingStep(
    type = "Raman",
    method = "CalculateSpectraSimilary",
    required = NA_character_,
    algorithm = "hqi",
    parameters = list(
      reference = reference,
      method = method
    ),
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
    stop("Invalid RamanMethod_CalculateSpectraSimilary_hqi object!")
  }
}

#' @describeIn RamanMethod_CalculateSpectraSimilary_hqi Validate the RamanMethod_CalculateSpectraSimilary_hqi object, returning NULL if valid.
#' @param x A RamanMethod_CalculateSpectraSimilary_hqi object.
#' @export
#'
validate_object.RamanMethod_CalculateSpectraSimilary_hqi <- function(x) {
  checkmate::assert_choice(x$type, "Raman")
  checkmate::assert_choice(x$method, "CalculateSpectraSimilary")
  checkmate::assert_choice(x$algorithm, "hqi")
  checkmate::assert_data_frame(x$parameters$reference)
  checkmate::assert_choice(
    x$parameters$method,
    c("pearson", "kendall", "spearman")
  )
  if (nrow(x$parameters$reference) > 0) {
    checkmate::assert_true(
      c("shift", "intenisty") %in% colnames(x$parameters$reference)
    )
  }
  NextMethod()
  NULL
}

#' @export
#' @noRd
run.RamanMethod_CalculateSpectraSimilary_hqi <- function(x, engine = NULL) {
  if (!is(engine, "RamanEngine")) {
    warning("Engine is not a RamanEngine object!")
    return(FALSE)
  }
  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }
  if (is.null(engine$Results[["RamanResults_Spectra"]])) {
    engine$Results <- RamanResults_Spectra(
      lapply(engine$Analyses$analyses, function(a) a$spectra)
    )
  }
  spec_obj <- engine$Results[["RamanResults_Spectra"]]
  spectra_list <- spec_obj$spectra
  reference_spectra_added <- x$parameters$reference
  similarity_method <- x$parameters$method
  if (nrow(reference_spectra_added) == 0) {
    references <- vapply(engine$Analyses$analyses, function(a) a$reference, NA_character_)
    if (any(is.na(references))) {
      warning("At least one reference spectra is not indicated! Not done.")
      return(FALSE)
    }
    if (spec_obj$is_averaged) {
      reference_spectra_unique <- spectra_list[unique(references)]
      rpls <- get_replicate_names(engine$Analyses)
      names(references) <- rpls
      references <- references[!duplicated(names(references))]
      reference_spectra <- lapply(names(spectra_list), function(r) {
        rpl <- references[[r]]
        reference_spectra_unique[[rpl]]
      })
    } else {
      intensity <- NULL
      rpls <- get_replicate_names(engine$Analyses)
      rpls <- rpls[rpls %in% references]
      reference_spectra_av <- spectra_list[names(rpls)]
      reference_spectra_av <- data.table::rbindlist(
        reference_spectra_av,
        idcol = "analysis"
      )
      reference_spectra_av$replicate <- rpls[reference_spectra_av$analysis]
      reference_spectra_av <- reference_spectra_av[,
        .(intensity = mean(intensity)),
        by = c("replicate", "shift")
      ]
      reference_spectra_av <- split(reference_spectra_av, by = "replicate")
      reference_spectra <- lapply(names(spectra_list), function(r) {
        rpl <- references[[r]]
        reference_spectra_av[[rpl]]
      })
    }
  } else {
    ref_shifts <- reference_spectra_added$shift
    if (!all(ref_shifts %in% spectra_list[[1]]$shift)) {
      warning(
        "Reference spectra shift values do not match the analyses! Not done."
      )
      return(FALSE)
    }
    reference_spectra <- lapply(spectra_list, function(z) {
      reference_spectra_added
    })
  }
  names(reference_spectra) <- names(spectra_list)
  similarity <- Map(
    function(a, r) {
      intensity <- NULL
      if (nrow(a) == 0) {
        return(data.table::data.table())
      }
      if (nrow(r) == 0) {
        return(data.table::data.table())
      }
      a_spec <- a[, .(intensity = mean(intensity)), by = "shift"]
      r_spec <- r[, .(intensity = mean(intensity)), by = "shift"]
      HQI <- stats::cor(
        a_spec$intensity,
        r_spec$intensity,
        method = similarity_method
      )
      HQI <- HQI^2
      ttest <- stats::cor.test(
        a_spec$intensity,
        r_spec$intensity,
        method = similarity_method,
        conf.level = 0.95
      )
      pvalue <- ttest$p.value
      data.table::data.table(
        "HQI" = HQI,
        "pValue" = pvalue,
        "tTest" = list(ttest),
        "data" = list(
          data.table::data.table(
            "analysis" = a,
            "reference" = r
          )
        )
      )
    },
    spectra_list,
    reference_spectra
  )
  engine$Results <- RamanResults_SpectraSimilarity(similarity)
  message(paste0("\U2713 ", "Spectra similarity added!"))
  invisible(TRUE)
}
