
trim_ranges <- function(vals, ranges) {
  return(rowSums(mapply(function(a, b) data.table::between(vals, a, b),
                        ranges[1], ranges[2])) > 0)
}

dt_bluid_eics <- function(spec, targets, trim_ranges) {

  eics_list <- lapply(seq_len(nrow(targets)), function(x, spec, trim_ranges) {

    spec <- spec[trim_ranges(spec$rt, c(targets$rtmin[x], targets$rtmax[x])), ]
    spec <- spec[trim_ranges(spec$mz, c(targets$mzmin[x], targets$mzmax[x])), ]
    spec$id <- targets$id[x]

    setcolorder(spec, c("id", "rt", "mz", "intensity"))

    return(spec)

  }, spec = spec, trim_ranges = trim_ranges)

  eics <- rbindlist(eics_list)

  return(eics)

}
