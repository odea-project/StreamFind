library(data.table)
library(pbapply)

clean_spectra <- function(
    spl,
    noiseThreshold = 0,
    noiseQuantile = 0.98,
    minSNR = 5,
    mzrThreshold = 0.005) {
    spl_clean <- pbapply::pblapply(spl, function(spec, noiseThreshold, noiseQuantile, minSNR, mzrThreshold) {
        spec <- as.data.table(spec)
        spec <- spec[intensity > 0, .(rt, mz, intensity)]
        if (nrow(spec) == 0) return(NULL)

        n_bins <- 6
        bin <- cut(seq_len(nrow(spec)), breaks = n_bins, labels = FALSE)
        local_quantile_vals <- spec[, .(q = quantile(intensity, probs = noiseQuantile)), by = bin]
        spec[, noise := pmax(local_quantile_vals$q[bin] * minSNR, noiseThreshold)]
        spec <- spec[intensity > noise]
        if (nrow(spec) == 0) return(NULL)

        setorder(spec, mz)
        mzs_diff <- c(TRUE, diff(spec$mz) > mzrThreshold)
        spec[, cluster := cumsum(mzs_diff)]
        spec <- spec[, .(mz = mz[which.max(intensity)], intensity = max(intensity), noise = max(noise)), by = .(rt, cluster)]
        spec[, cluster := NULL]
        spec
    },
    noiseThreshold = noiseThreshold,
    noiseQuantile = noiseQuantile,
    minSNR = minSNR,
    mzrThreshold = mzrThreshold)

    spl_clean <- spl_clean[!sapply(spl_clean, is.null)]
    spl_clean
}

