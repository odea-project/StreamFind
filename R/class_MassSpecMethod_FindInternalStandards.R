#' **MassSpecMethod_FindInternalStandards_StreamFind**
#'
#' @description Settings for finding internal standards using a data.frame.
#'
#' @param database A data.table with at least the columns name, mass, and rt indicating the name,
#' neutral monoisotopic
#' mass and retention time of the internal standards, respectively.
#' @template arg-ms-ppm
#' @template arg-ms-sec
#'
#' @return A `MassSpecMethod_FindInternalStandards_StreamFind` object.
#'
#' @export
#'
MassSpecMethod_FindInternalStandards_StreamFind <- S7::new_class(
  name = "MassSpecMethod_FindInternalStandards_StreamFind",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(database = data.table::data.table(
                           name = character(),
                           formula = character(),
                           mass = numeric(),
                           rt = numeric()
                         ),
                         ppm = 5,
                         sec = 10) {
    S7::new_object(
      ProcessingStep(
        engine = "MassSpec",
        method = "FindInternalStandards",
        required = "FindFeatures",
        algorithm = "StreamFind",
        parameters = list(
          database = data.table::as.data.table(database),
          ppm = as.numeric(ppm),
          sec = as.numeric(sec)
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "StreamFind",
        developer = "Ricardo Cunha",
        contact = "cunha@iuta.de",
        link = "https://odea-project.github.io/StreamFind",
        doi = NA_character_
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "FindInternalStandards")
    checkmate::assert_choice(self@algorithm, "StreamFind")
    checkmate::assert_number(self@parameters$ppm)
    checkmate::assert_number(self@parameters$sec)
    checkmate::assert_data_table(self@parameters$database)
    checkmate::assert_true(
      all(c("name", "neutralMass", "rt") %in% colnames(self@parameters$database)) ||
        all(c("name", "mass", "rt") %in% colnames(self@parameters$database)) ||
        all(c("name", "mz", "rt") %in% colnames(self@parameters$database))
    )
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_FindInternalStandards_StreamFind) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_results_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  nts <- engine$NTS

  if (!nts$has_features) {
    warning("There are no features! Run find_features first!")
    return(FALSE)
  }

  database <- x$parameters$database

  database <- data.table::as.data.table(database)

  if (nrow(database) == 0) {
    warning("Database is empty!")
    return(FALSE)
  }

  internal_standards <- get_suspects(
    nts,
    database = database,
    ppm = x$parameters$ppm,
    sec = x$parameters$sec,
    filtered = TRUE
  )

  if (nrow(internal_standards) == 0) {
    warning("Internal standards were not found!")
    return(FALSE)
  }

  if ("intensity" %in% colnames(database)) {
    intensity <- database$intensity
    names(intensity) <- database$name

    internal_standards$rec <- round(
      (internal_standards$intensity / intensity[internal_standards$istd_name]) * 100,
      digits = 1
    )
  } else if ("area" %in% colnames(database)) {
    area <- database$area
    names(area) <- database$name

    internal_standards$rec <- round(
      (internal_standards$area / area[internal_standards$istd_name]) * 100,
      digits = 1
    )
  } else {
    blks <- engine$Analyses$blanks

    if (any(!is.na(blks)) & nts$has_groups) {
      rpls <- engine$Analyses$replicates

      internal_standards$replicate <- rpls[internal_standards$analysis]

      internal_standards$rec <- vapply(seq_len(nrow(internal_standards)),
        function(x, internal_standards, blks) {
          feat <- internal_standards[x, ]

          if (feat$replicate %in% blks) {
            return(1)
          }

          feat_area <- feat$area

          blk <- blks[feat$analysis]

          blk_feats <- internal_standards[
            internal_standards$replicate %in% blk &
              internal_standards$group %in% feat$group,
          ]

          if (nrow(blk_feats) > 0) {
            blk_area <- mean(blk_feats$area, na.rm = TRUE)
            return(round(feat_area / blk_area, digits = 2))
          } else {
            NA_real_
          }
        },
        internal_standards = internal_standards,
        blks = blks,
        NA_real_
      )
    } else {
      internal_standards$rec <- NA_real_
    }
  }
  
  if (nrow(internal_standards) > 0) {
    istd_analyses <- unique(internal_standards$analysis)
    for (a in istd_analyses) {
      temp <- internal_standards[internal_standards$analysis == a, ]
      if (TRUE %in% duplicated(temp$name)) {
        message("\U26a0 Duplicated internal standards found in analysis ", a, "!")
        duplicated_isdt <- unique(temp$name[duplicated(temp$name)])
        for (d in duplicated_isdt) {
          temp2 <- temp[temp$name %in% d, ]
          
          if (any(is.na(temp2$group))) {
            temp2 <- temp2[!is.na(temp2$group), ]
          }
          
          if (nrow(temp2) > 1) {
            temp2 <- temp2[which(abs(temp2$error_rt) == min(abs(temp2$error_rt))), ]
          }
          
          if (nrow(temp2) > 1) {
            temp2 <- temp2[which(abs(temp2$error_mass) == min(abs(temp2$error_mass))), ]
          }
          
          fts_rem <- temp[temp$name %in% d & !temp$feature %in% temp2$feature, ]
          
          if (nrow(fts_rem) > 0) {
            internal_standards <- internal_standards[
              !(internal_standards$feature %in% fts_rem$feature & internal_standards$analysis %in% a),
            ]
          }
        }
      }
    }
    
    internal_standards_l <- split(internal_standards, internal_standards$analysis)
    
    features <- nts$feature_list

    istd_col <- lapply(names(features), function(z, features, internal_standards_l) {
      istd <- internal_standards_l[[z]]
      fts <- features[[z]]

      if (!is.null(istd)) {
        istd_l <- lapply(fts$feature, function(j, istd) {
          istd_idx <- which(istd$feature %in% j)

          if (length(istd_idx) > 0) {
            istd_temp <- istd[istd_idx, ]
            istd_temp <- istd_temp[, c("name", "error_mass", "error_rt", "rec"), with = FALSE]

            if (nrow(istd_temp) > 0) {
              istd_temp
            } else {
              data.table::data.table()
            }
          } else {
            data.table::data.table()
          }
        }, istd = istd)

        istd_l
      } else {
        lapply(fts$feature, function(j) data.table::data.table())
      }
    }, features = features, internal_standards_l = internal_standards_l)

    names(istd_col) <- names(features)

    features <- Map(function(fts, i) {
      fts$istd <- i
      fts
    }, features, istd_col)

    nts$feature_list <- features
    
    engine$NTS <- nts

    message(
      "\U2713 ",
      length(unique(internal_standards$name)),
      " internal standards found and tagged!"
    )
    TRUE
  } else {
    FALSE
  }
}
