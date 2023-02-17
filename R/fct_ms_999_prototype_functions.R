
# object <- qc_set
# db <- qc_db
# ppm <- 10
# sec <- 10
prototype_suspectScreening <- function(object, db, ppm = 10 , sec = 10) {

  # prototype for suspect screening of an msData class object

  # make assert for cols in db
  # obligatory is name (must be unique for now), formula and mz
  # optional rt
  #x <- db$name[1]

  if (length(unique(polarities(object))) > 1) {

    sus <- lapply(db$name, function(x, object, db, ppm, sec) {

      temp <- features(object, mass = db[name %in% x, .(mass, rt)],
                       ppm = ppm, sec = sec, complete = TRUE)

      if (nrow(temp) > 0) {

        temp$name <- x
        temp$formula <- db$formula[db$name %in% x]
        temp$sus_ppm <- round(
          ((temp$mass - db$mass[db$name %in% x]) / temp$mass) * 1E6,
          digits = 1
        )
        temp$sus_sec <- round(temp$rt - db$rt[db$name %in% x], digits = 1)

        return(temp)

      } else {

        cat(paste0("Features not found for ", x, "\n"))
        return(NULL)

      }
    },
    object = object,
    db = db,
    ppm = ppm,
    sec = sec)

  } else {

    sus <- lapply(db$name, function(x, object, db, ppm, sec) {

      polarity <- unique(polarities(object))

      if (polarity %in% "positive") db$mz <- db$mass + 1.0073

      if (polarity %in% "negative") db$mz <- db$mass - 1.0073

      temp <- features(object, mz = db[db$name %in% x, .(mz, rt)],
                       ppm = ppm, sec = sec, complete = TRUE)
      if (nrow(temp) > 0) {
        temp$name <- x
        temp$formula <- db$formula[db$name %in% x]
        temp$sus_ppm <- round(
          ((temp$mz - db$mz[db$name %in% x]) / temp$mz) * 1E6,
          digits = 1
        )
        temp$sus_sec <- round(temp$rt - db$rt[db$name %in% x], digits = 1)
        return(temp)
      } else {
        return(NULL)
      }
    },
    object = object,
    db = db,
    ppm = ppm,
    sec = sec)
  }

  sus <- data.table::rbindlist(sus)
  sus <- dplyr::select(sus, name, formula, id, sus_ppm, sus_sec, dplyr::everything())

  return(sus)
}
