generate_peak_formulas <- function(peaks_dt, mzabs, ppm, elements, validation, charge) {
  results <- list()
  for (i in seq_len(nrow(peaks_dt))) {
    peak_mz <- peaks_dt$mz[i]
    allowed_error <- pmax(mzabs, (peak_mz * ppm / 1e6))
    mfSet <- rcdk::generate.formula(
      peak_mz - 1.007276, # convert to neutral mass assuming initial [M+H]+ ionization
      window = allowed_error,
      elements = elements,
      validation = validation,
      charge = charge
    )
    if (length(mfSet) > 0) {
      mf_df <- data.table(
        formula = sapply(mfSet, function(x) x@string),
        formula_mass = sapply(mfSet, function(x) x@mass),
        formula_charge = sapply(mfSet, function(x) x@charge),
        formula_isotopes = lapply(mfSet, function(x) as.data.table(x@isotopes))
      )

      mf_df$formula_C_H_ratio <- sapply(mf_df$formula_isotopes, function(x) {
        C_count <- as.numeric(x$number[x$isoto %in% "C"])
        H_count <- as.numeric(x$number[x$isoto %in% "H"])
        if (H_count == 0) {
          NA
        } else {
          round(C_count / H_count, 2)
        }
      })

      mf_df <- cbind(peaks_dt[rep(i, nrow(mf_df)), ], mf_df)

      for (k in seq_len(nrow(mf_df))) {
        mf_df$formula_error[k] <- abs(mf_df$formula_mass[k] + 1.007276 - peak_mz) / peak_mz * 1e6
      }
      results[[length(results) + 1]] <- mf_df
    }
  }
  if (length(results) > 0) {
    do.call(rbind, results)
  } else {
    data.frame()
  }
}
