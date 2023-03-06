
### //// validity -------------------------------------------------------------

msData_validity <- function(object) {

  valid <- TRUE

  if (!all(sapply(object@analyses, is) %in% "msAnalysis")) {
    warning("Analysis objects must be as msAnalysis S4 class objects!")
    valid <- FALSE
  }

  blks <- blankReplicateNames(object)
  blks[blks %in% ""] <- NA_character_
  blks <- na.omit(blks)
  if (length(blks) > 0) {
    if (FALSE %in% (blks %in% replicateNames(object))) {
      warning("Blank replicates not present in analyses set!")
      valid <- FALSE
    }
  }

  # TODO validation for features classes,
  # including the analysis/replicates and blanks

  return(valid)
}



### //// msData ---------------------------------------------------------------

#' @title msData
#'
#' @description An S4 class object to store and manage processing of mass
#' spcetrometric analyses. The \code{msData} object inherits the
#' \linkS4class{streamSet} structure plus the features slot for
#' processing MS data.
#'
#' @slot title A character string defining the title.
#' @slot date A \code{POSIXt} class object.
#' @slot analyses A list with \linkS4class{msAnalysis} objects.
#' @slot features  An \linkS4class{msFeatures} class object.
#'
#' @export
#'
#' @md
setClass("msData",
  slots = c(
    features = "msFeatures"
  ),
  contains = "streamSet",
  prototype = list(
    features = new("msFeatures")
  ),
  validity = msData_validity
)



### //// Auxiliary functions --------------------------------------------------

checkAnalysesArgument <- function(object, analyses = NULL) {

  if (!"msData" %in% class(object)) return(NULL)

  if (is.null(analyses)) return(analysisNames(object))

  if (is.character(analyses)) {

    if (FALSE %in% (unname(analyses) %in% analysisNames(object))) {
      message("Given analysis names not found in the msData object!")
      return(NULL)
    }

    return(analysisNames(object)[analysisNames(object) %in% analyses])

  } else if (is.numeric(analyses)) {

    if (max(analyses) > length(analysisNames(object))) {
      message("Analyses index not matching the number of analyses in the object!")
      return(NULL)
    }

    return(analysisNames(object)[analyses])
  }

  return(NULL)
}



# buildEICs <- function(spec, targets, run_parallel) {
#
#   analyses <- names(spec)
#
#   chunk_list <- lapply(analyses, function(x, targets, spec) {
#     return(list(spec = as.data.frame(spec[[x]]), targets = as.data.frame(targets)))
#   }, spec = spec, targets = targets)
#
#   if (run_parallel) {
#
#     ex_packages = "Rcpp"
#
#     cpp_file <- system.file("scripts/cpp_build_eics_ext.cpp",
#                             package = "streamFind")
#
#     ex_globals = "cpp_file"
#
#     workers <- detectCores() - 1
#
#     if (length(chunk_list) < workers) workers <- length(chunk_list)
#
#     par_type <- "PSOCK"
#
#     if (supportsMulticore()) par_type <- "FORK"
#
#     cl <- makeCluster(workers, type = par_type)
#
#     clusterExport(cl, ex_globals, envir = environment())
#
#     registerDoParallel(cl)
#
#     eics_list <- foreach(chunk = chunk_list, .packages = ex_packages) %dopar% {
#       sourceCpp(cpp_file)
#       return(cpp_build_eics_ext(chunk[[1]], chunk[[2]]))
#     }
#
#     stopCluster(cl)
#
#   } else {
#
#     eics_list <- lapply(chunk_list, function(chunk) {
#       return(rcpp_ms_make_eics_for_msAnalysis(chunk[[1]], chunk[[2]]))
#     })
#
#   }
#
#   names(eics_list) <- names(chunk_list)
#
#   eics <- rbindlist(eics_list, idcol = "analysis", fill = TRUE)
#
#   return(eics)
# }
#
#
#
# buildEICs2 <- function(spec, targets, run_parallel) {
#
#   analyses <- names(spec)
#
#   chunk_list <- lapply(analyses, function(x, targets, spec) {
#     return(list(spec = spec[[x]][, .(rt, mz, intensity)], targets = targets))
#   }, spec = spec, targets = targets)
#
#   if (run_parallel) {
#
#     ex_packages = "data.table"
#
#     source(system.file("scripts/dt_build_eics_ext.R",
#                             package = "streamFind"))
#
#     ex_globals = c("dt_bluid_eics", "trim_ranges")
#
#     workers <- detectCores() - 1
#
#     if (length(chunk_list) < workers) workers <- length(chunk_list)
#
#     par_type <- "PSOCK"
#
#     if (supportsMulticore()) par_type <- "FORK"
#
#     cl <- makeCluster(workers, type = par_type)
#
#     clusterExport(cl, ex_globals, envir = environment())
#
#     registerDoParallel(cl)
#
#     eics_list <- foreach(chunk = chunk_list, .packages = ex_packages) %dopar% {
#       return(dt_bluid_eics(chunk[[1]], chunk[[2]], trim_ranges))
#     }
#
#     stopCluster(cl)
#
#   } else {
#
#     eics_list <- lapply(chunk_list, function(chunk) {
#       return(rcpp_ms_make_eics_for_msAnalysis(chunk[[1]], chunk[[2]]))
#     })
#
#   }
#
#   names(eics_list) <- names(chunk_list)
#
#   eics <- rbindlist(eics_list, idcol = "analysis", fill = TRUE)
#
#   return(eics)
# }



### //// S4 methods -----------------------------------------------------------

#### _initialize_ -------------------------------------------------------------

#' @describeIn msData creates an \linkS4class{msData} object.
#'
#' @param run_parallel Logical, set to \code{TRUE} for parallel processing.
#'
#' @export
#'
setMethod("initialize", "msData", function(.Object,
                                           files = NA_character_,
                                           title = NA_character_,
                                           date = Sys.time(),
                                           replicates = NA_character_,
                                           blanks = NA_character_,
                                           run_parallel = TRUE, ...) {

  .Object <- callNextMethod(.Object, ...)

  ms_data <- .Object

  analysisTable <- checkFilesInput(files, replicates, blanks)

  if (!is.null(analysisTable)) {

    ms_data@title <- title

    ms_data@date <- date

    files <- analysisTable$file

    source(system.file("scripts/initialize_msAnalysis_ext.R",
                       package = "streamFind"), local = TRUE)

    if (run_parallel) {

      ex_packages = "mzR"

      #ex_globals = "initialize_msAnalysis_ext"

      workers <- detectCores() - 1

      if (length(files) < workers) workers <- length(files)

      par_type <- "PSOCK"

      if (supportsMulticore()) par_type <- "FORK"

      cl <- makeCluster(workers, type = par_type)

      #clusterExport(cl, ex_globals, envir = environment())

      registerDoParallel(cl)

      analyses <- foreach(file = files, .packages = ex_packages, .verbose = T) %dopar% {

        return(initialize_msAnalysis_ext(file))

      }

      stopCluster(cl)


    } else {

      analyses <- lapply(files, function(file) {

        return(initialize_msAnalysis_ext(file))

      })

    }


    analyses <- lapply(analyses, function(x) as.msAnalysis(x))

    analyses_class <- sapply(analyses, function(x) is(x))


    if (all(analyses_class %in% "msAnalysis")) {

      analyses_names <- sapply(analyses, function(x) analysisName(x))
      names(analyses) <- analyses_names

      ms_data@analyses <- analyses

      analysisTable$class <- sapply(analyses, function(x) is(x))

      analysisTable$polarity <- polarities(ms_data)

      rownames(analysisTable) <- seq_len(nrow(analysisTable))

      ms_data@features <- new("msFeatures", analyses = analysisTable)

    } else {

      warning("Not all files deliver an msAnalysis class!
              Not possible to contruct an msData class.")

      return(NULL)
    }
  }

  return(ms_data)
})



#### _show_ -------------------------------------------------------------------

#' @describeIn msData prints a summary of the \linkS4class{msData}.
#'
#' @param object An \linkS4class{msData} object.
#'
#' @export
#'
setMethod("show", "msData", function(object) {

  cat(
    "  Class         ", paste(is(object), collapse = "; "), "\n",
    "  Title         ", object@title, "\n",
    "  Date          ", as.character(object@date), "\n",
    sep = ""
  )
  if (length(object@analyses) > 0) {
    tb <- data.table(
      analysis = analysisNames(object),
      replicate = replicateNames(object),
      blank = blankReplicateNames(object),
      class = sapply(object@analyses, function(x) is(x)),
      polarity = polarities(object)
    )

    tb$traces <- sapply(object@analyses, function(x) nrow(x@spectra))
    tb$peaks <- sapply(object@analyses, function(x) nrow(x@peaks))

    if (nrow(object@features@metadata) > 0) {
      tb$features <- apply(
        object@features@intensity[, analysisNames(object), with = FALSE], 2,
        function(x) length(x[x > 0])
      )
    } else {
      tb$features <- 0
    }

    print(tb)

  } else {
    cat("     n.a.", "\n", sep = "")
  }

})



#### names and paths __________ -----------------------------------------------

##### analysisInfo ------------------------------------------------------------

#' @describeIn msData getter for analysis info as \link{data.frame} with
#' four columns: path, analysis, group and blank. The \link{data.frame}
#' can be used as analysisInfo in \pkg{patRoon}.
#'
#' @param obj An \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases analysisInfo,msData,msData-method
#'
setMethod("analysisInfo", "msData", function(obj) {

  temp <- data.frame(
    "path" = sapply(obj@analyses, function(x) dirname(x@file)),
    "analysis" = sapply(obj@analyses, function(x) x@name),
    "group" = replicateNames(obj),
    "blank" = blankReplicateNames(obj)
  )

  if (length(unique(polarities(obj))) > 1) {
    temp$set = polarities(obj)
  }

  temp$class = sapply(obj@analyses, function(x) is(x))
  temp$file = filePaths(obj)

  rownames(temp) <- seq_len(nrow(temp))

  return(temp)
})



##### analysisTable -----------------------------------------------------------

#' @describeIn msData getter for analysis table as \link{data.table} with
#' four columns: file, analysis, replicate, blank, class and polarity.
#'
#' @export
#'
#' @aliases analysisTable,msData,msData-method
#'
setMethod("analysisTable", "msData", function(object) {

  analyses_table <- data.table(
    "file" = filePaths(object),
    "analysis" = analysisNames(object),
    "replicate" = replicateNames(object),
    "blank" = blankReplicateNames(object),
    "class" = sapply(object@analyses, function(x) is(x)),
    "polarity" = polarities(object)
  )

  rownames(analyses_table) <- seq_len(nrow(analyses_table))

  return(analyses_table)
})



##### filePaths ---------------------------------------------------------------

#' @describeIn msData getter for analysis file full paths.
#'
#' @export
#'
#' @aliases filePaths,msData,msData-method
#'
setMethod("filePaths", "msData", function(object) {
  return(sapply(object@analyses, function(x) x@file))
})



##### analysisNames -----------------------------------------------------------

#' @describeIn msData getter for analysis names.
#'
#' @export
#'
#' @aliases analysisNames,msData,msData-method
#'
setMethod("analysisNames", "msData", function(object) {
  return(sapply(object@analyses, function(x) x@name))
})



##### replicateNames ----------------------------------------------------------

#' @describeIn msData getter for replicate names.
#'
#' @export
#'
#' @aliases replicateNames,msData,msData-method
#'
setMethod("replicateNames", "msData", function(object) {
  replicates <- object@features@analyses$replicate
  names(replicates) <- object@features@analyses$analysis
  return(replicates)
})



##### replicateNames<- --------------------------------------------------------

#' @describeIn msData setter for analysis replicate names.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing analysis replicate name for each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases replicateNames<-,msData,msData-method
#'
setMethod("replicateNames<-", signature("msData", "ANY"), function(object, value) {

  ana <- analysisNames(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  object@features@analyses$replicate <- value

  return(object)
})



##### blankReplicateNames -----------------------------------------------------

#' @describeIn msData getter for blank replicate names.
#'
#' @export
#'
#' @aliases blankReplicateNames,msData,msData-method
#'
setMethod("blankReplicateNames", "msData", function(object) {
  blanks <- object@features@analyses$blank
  names(blanks) <- object@features@analyses$analysis
  return(blanks)
})



##### blankReplicateNames<- ---------------------------------------------------

#' @describeIn msData setter for associated blank replicates for each analysis.
#' The \code{value} is a character vector with the same length as
#' the number of analyses in the \code{object},
#' containing the associated blank replicate name of each analysis.
#'
#' @param value A character vector applicable to the respective method.
#'
#' @export
#'
#' @aliases blankReplicateNames<-,msData,msData-method
#'
setMethod("blankReplicateNames<-", signature("msData", "ANY"), function(object, value) {

  ana <- analysisNames(object)
  if (length(value) != length(ana)) {
    warning("Length of value does not match the number of analyses.")
    return(object)
  }

  object@features@analyses$blank <- value

  return(object)
})







##### polarities --------------------------------------------------------------

#' @describeIn msData getter for the polarity of the analyses.
#'
#' @export
#'
#' @aliases polarities,msData,msData-method
#'
setMethod("polarities", "msData", function(object) {
  mt <- unlist(lapply(object@analyses, function(x) polarity(x)))
  names(mt) <- analysisNames(object)
  return(mt)
})



#### msAnalysis _______________ -----------------------------------------------

##### addAnalyses -------------------------------------------------------------

#' @describeIn msData adds \linkS4class{msAnalysis} objects to the existing
#' \linkS4class{msData} object.
#'
#' @param analysisList A list of \linkS4class{msAnalysis} objects.
#'
#' @export
#'
#' @aliases addAnalyses,msData,msData-method
#'
setMethod("addAnalyses", "msData", function(object,
                                            analysisList = NULL,
                                            replicates = NA_character_,
                                            blanks = NA_character_) {
  # placer for var
  analysis <- NULL

  if (!is.null(analysisList)) {

    cls <- NA_character_

    if (length(analysisList) == 1 & !(class(analysisList) %in% "list")) {
      cls <- class(analysisList)
    } else {
      cls <- sapply(analysisList, function(x) class(x))
    }

    if (all(cls %in% "msAnalysis")) {

      if (!is.list(analysisList)) {
        analysisList <- list(analysisList)
        names(analysisList) <- analysisNames(analysisList)
      } else {
        names(analysisList) <- sapply(analysisList, function(x) analysisNames(x))
      }

      #check name
      if (TRUE %in% (names(analysisList) %in% analysisNames(object))) {
        warning("Analysis name/s to add is already in msData!")
        return(object)
      }

      analysisTable <- checkFilesInput(
        files = sapply(analysisList, function(x) filePath(x)),
        replicates = replicates,
        blanks = blanks
      )

      analysisTable$analysis <- sapply(analysisList, function(x) analysisNames(x))

      analysisTable$class <- cls

      analysisTable$polarity <- sapply(analysisList, function(x) polarities(x))

      old_analysisTable <- analysisTable(object)

      setcolorder(analysisTable, colnames(old_analysisTable))

      new_analysisTable <- rbind(old_analysisTable, analysisTable)

      setorder(new_analysisTable, analysis)

      object@analyses <- c(object@analyses, analysisList)

      object@analyses <-  object@analyses[order(names(object@analyses))]

      object@features <- new("msFeatures")

      object@features@analyses <- copy(new_analysisTable)

      object@analyses <- lapply(object@analyses, function(x) {
        if (nrow(x@peaks) > 0) {
          x@peaks$feature <- NA_character_
        }
        return(x)
      })

      return(object)
    }

    # TODO add possibility to add file paths also in addAnalyses
  }

  warning("No msAnalysis objects given!")
  return(object)
})



##### getAnalyses -------------------------------------------------------------

#' @describeIn msData getter for \linkS4class{msAnalysis} objects within the
#' \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases getAnalyses,msData,msData-method
#'
setMethod("getAnalyses", "msData", function(object, analyses = NULL) {

  if (!is.null(analyses)) {
    if (is.character(analyses)) {
      analyses <- which(analyses == analysisNames(object))
    }
    if (length(analyses) > 0 && is.numeric(analyses)) {
      if (length(analyses) == 1) return(object@analyses[[analyses]])
      return(object@analyses[analyses])
    }
  }
  warning("Analysis not specified or not found in object!")
  return(list())
})



#### raw data __________________ ----------------------------------------------

##### getRawData --------------------------------------------------------------

#' @describeIn msData gets (when available) raw spectra and chromatograms
#' from each analysis in the \linkS4class{msData} object.
#'
#' @param spectra Logical, set to \code{TRUE} for parsing spectra.
#' @param TIC Logical, set to \code{TRUE} for parsing TIC from xml headings.
#' @param BPC Logical, set to \code{TRUE} for parsing BPC from xml headings.
#' @param chroms Logical, set to \code{TRUE} for parsing chromatograms.
#' @param levels A numeric vector with the MS levels for parsing spectra.
#' @param rtr A numeric vector of length two with the time range (in seconds)
#' for parsing spectra, TIC and/or BPC.
#' @param preMZrange A numeric vector of length two with the \code{m/z} range
#' of precursor ions for collecting spectra with level higher than 1.
#' @param minIntensityMS1 Numeric value on length one with the
#' minimum intensity of MS1 level traces.
#' @param minIntensityMS2 Numeric value on length one with the
#' minimum intensity of MS2 level traces.
#'
#' @export
#'
#' @aliases getRawData,msData,msData-method
#'
setMethod("getRawData", "msData", function(object,
                                           analyses = NULL,
                                           spectra = TRUE,
                                           TIC = TRUE,
                                           BPC = TRUE,
                                           chroms = TRUE,
                                           levels = NULL,
                                           rtr = NULL,
                                           preMZrange = NULL,
                                           minIntensityMS1 = 0,
                                           minIntensityMS2 = 0,
                                           run_parallel = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)

  analysesList <- object@analyses[analyses]

  list_out <- runParallelLapply(
    analysesList,
    run_parallel,
    NULL,
    function(x, spectra, TIC, BPC, chroms, levels,
                   rtr, preMZrange, minIntensityMS1, minIntensityMS2) {

      temp_list <- getRawData(object = x,
        spectra = spectra,
        TIC = TIC,
        BPC = BPC,
        chroms = chroms,
        levels = levels,
        rtr = rtr,
        preMZrange = preMZrange,
        minIntensityMS1 = minIntensityMS1,
        minIntensityMS2 = minIntensityMS1
      )

      p()

      return(temp_list)

    }, spectra = spectra, TIC = TIC, BPC = BPC,
    chroms = chroms, levels = levels, rtr = rtr,
    preMZrange = preMZrange, minIntensityMS1 = minIntensityMS1,
    minIntensityMS2 = minIntensityMS1
  )

  return(list_out)
})



##### loadRawData -------------------------------------------------------------

#' @describeIn msData adds raw data to all or defined analyses in the
#' \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases loadRawData,msData,msData-method
#'
setMethod("loadRawData", "msData", function(object, run_parallel = FALSE) {

  analyses <- analysisNames(object)

  analysesList <- object@analyses[analyses]

  analysisList <- runParallelLapply(
    analysesList,
    run_parallel,
    NULL,
    function(x) {
      temp <- streamFind::loadRawData(object = x)
      p()
      return(temp)
    }
  )

  object@analyses[analyses] <- analysisList

  return(object)
})



##### loadSpectraInfo ---------------------------------------------------------

#' @describeIn msData adds raw spectra information (i.e., scan number,
#'  ms level and retention time of each spectrum) to the slot \code{spectra}
#'  of each \linkS4class{msAnalysis} in the \linkS4class{msData}.
#'  If the levels are higher than one, as the case of MS/MS data,
#'  the collision energy and precursor scan and \emph{m/z} are also returned.
#'
#' @export
#'
#' @aliases loadSpectraInfo,msData,msData-method
#'
setMethod("loadSpectraInfo", "msData", function(object, run_parallel = FALSE) {

  analyses <- analysisNames(object)

  analysesList <- object@analyses[analyses]

  analysisList <- runParallelLapply(
    analysesList,
    run_parallel,
    NULL,
    function(x) {
      temp <- streamFind::loadSpectraInfo(object = x)
      p()
      return(temp)
    }
  )

  object@analyses[analyses] <- analysisList

  return(object)
})



#### spectra __________________ -----------------------------------------------

##### getSpectra --------------------------------------------------------------

#' @describeIn msData adds (when available) raw spectra to each
#' \linkS4class{msAnalysis} of the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases getSpectra,msData,msData-method
#'
setMethod("getSpectra", "msData", function(object,
                                           analyses = NULL,
                                           levels = NULL,
                                           rtr = NULL,
                                           preMZrange = NULL,
                                           minIntensityMS1 = 0,
                                           minIntensityMS2 = 0,
                                           run_parallel = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  files <- filePaths(object)[analyses]

  source(
    system.file("scripts/get_ms_spectra_from_file_ext.R",
                package = "streamFind"), local = TRUE
  )

  if (run_parallel) {

    ex_packages = c("mzR", "data.table")

    # ex_globals = c("levels", "rtr", "preMZrange",
    #                "minIntensityMS1", "minIntensityMS2",
    #                "get_ms_spectra_from_file_ext")

    workers <- detectCores() - 1

    if (length(files) < workers) workers <- length(files)

    par_type <- "PSOCK"

    if (supportsMulticore()) par_type <- "FORK"

    cl <- makeCluster(workers, type = par_type)

    #clusterExport(cl, ex_globals, envir = environment())

    registerDoParallel(cl)

    spec_list <- foreach(i = files, .packages = ex_packages) %dopar% {

      return(get_ms_spectra_from_file_ext(
        file = i , levels, rtr, preMZrange, minIntensityMS1, minIntensityMS2))

    }

    stopCluster(cl)

  } else {

    spec_list <- lapply(files, function(file, levels, rtr, preMZrange,
                                        minIntensityMS1, minIntensityMS2) {

      return(get_ms_spectra_from_file_ext(
        file, levels, rtr, preMZrange, minIntensityMS1, minIntensityMS2))

    }, levels = levels, rtr = rtr, preMZrange = preMZrange,
    minIntensityMS1 = minIntensityMS1, minIntensityMS2 = minIntensityMS2)

  }

  names(spec_list) <- analyses

  return(spec_list)
})



##### loadSpectra -------------------------------------------------------------

#' @describeIn msData adds (when available) raw spectra to each
#' \linkS4class{msAnalysis} of the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases loadSpectra,msData,msData-method
#'
setMethod("loadSpectra", "msData", function(object, run_parallel = FALSE) {

  spec_list <- getSpectra(object)

  object@analyses <- lapply(object@analyses, function(x, spec_list) {
    x@spectra <- spec_list[[analysisName(x)]]
    return(x)
  }, spec_list = spec_list)

  return(object)
})



##### hasLoadedSpectra --------------------------------------------------------

#' @describeIn msData checks if the analyses in the \linkS4class{msData} have
#' loaded raw spectra.
#'
#' @export
#'
#' @aliases hasLoadedSpectra,msData,msData-method
#'
setMethod("hasLoadedSpectra", "msData", function(object) {
  return(sapply(object@analyses, function(x) hasLoadedSpectra(x)))
})



##### spectra -----------------------------------------------------------------

#' @describeIn msData getter for slot spectra from \linkS4class{msAnalysis}
#' objects of the in the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases spectra,msData,msData-method
#'
setMethod("spectra", "msData", function(object, analyses = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  spec <- lapply(object@analyses[analyses], function(x) {
    return(x@spectra)
  })

  spec <- rbindlist(spec, idcol = "analysis")

  return(spec)
})



##### plotSpectra -------------------------------------------------------------

#' @describeIn msData plots spectra from \linkS4class{msAnalysis} objects
#' in the \linkS4class{msData} object in 3D. The arguments \code{mz},
#' \code{ppm}, \code{rt} and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information. The argument \code{colorBy}
#' can be set to \emph{analyses} or \emph{levels} to color by
#' \linkS4class{msAnalysis} or MS levels.
#'
#' @export
#'
#' @aliases plotSpectra,msData,msData-method
#'
setMethod("plotSpectra", "msData", function(object,
                                            analyses = NULL,
                                            levels = c(1, 2),
                                            mz = NULL, rt = NULL,
                                            ppm = 20, sec = 60,
                                            colorBy = "analyses") {

  spec <- spectra(object, analyses)

  if (nrow(spec) == 0) {
    message("Spectra not found, load spectra
            first with loadSpectra() method.")
    return(NULL)
  }

  targets <- makeTargets(mz, rt, ppm, sec)

  if (TRUE %in% c((targets$mzmax > 0), (targets$rtmax > 0))) {

    if (0 %in% targets$mzmax) targets$mzmax <- max(spec$mz)
    if (0 %in% targets$rtmax) targets$rtmax <- max(spec$rt)

    spec <- spec[mz >= min(targets$mzmin) & mz <= max(targets$mzmax) &
                 rt >= min(targets$rtmin) & rt <= max(targets$rtmax), ]
  }

  spec <- spec[spec$level %in% levels, ]

  if (nrow(spec) == 0) {
    message("Requested MS traces not found.")
    return(NULL)
  }

  spec$level <- factor(spec$level)
  spec$analysis <- factor(spec$analysis)

  spec_2 <- copy(spec)
  spec_2 <- split(spec_2, spec_2$analysis)

  spec_2 <- lapply(spec_2, function(x) {
    x$rtmz <- paste(x$mz, x$rt, x$analysis, sep = "_")
    x_temp <- copy(x)
    x_temp$intensity <- 0
    x <- rbind(x, x_temp)
    return(x)
  })

  spec_2 <- rbindlist(spec_2)

  spec_2$level <- paste("MS", spec_2$level, sep = "")

  if (colorBy == "levels") {
    spec_2$var <- spec_2$level
  } else {
    spec_2$var <- spec_2$analysis
  }

  colors_var <- getColors(unique(spec_2$var))

  fig <- plot_ly(spec_2, x = ~rt, y = ~mz, z = ~intensity) %>%
    group_by(spec_2$rtmz) %>%
    add_lines(color = ~var,  colors = colors_var)

  fig <- fig %>% layout(scene = list(
    xaxis = list(title = "Retention time / seconds"),
    yaxis = list(title = "<i>m/z</i>"),
    zaxis = list(title = "Intensity / counts")))

  return(fig)
})



#### chromatograms ___________ ------------------------------------------------

##### getChromatograms --------------------------------------------------------

#' @describeIn msData adds (when available) raw spectra to each
#' \linkS4class{msAnalysis} of the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases getChromatograms,msData,msData-method
#'
setMethod("getChromatograms", "msData", function(object,
                                                 analyses = NULL,
                                                 minIntensity = 0,
                                                 run_parallel = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  files <- filePaths(object)[analyses]

  source(
    system.file("scripts/get_ms_chromatograms_from_file_ext.R",
                package = "streamFind")
  )

  if (run_parallel) {

    ex_packages = c("mzR", "data.table")

    ex_globals = c("minIntensity", "get_ms_chromatograms_from_file_ext")

    workers <- detectCores() - 1

    if (length(files) < workers) workers <- length(files)

    par_type <- "PSOCK"

    if (supportsMulticore()) par_type <- "FORK"

    cl <- makeCluster(workers, type = par_type)

    clusterExport(cl, ex_globals, envir = environment())

    registerDoParallel(cl)

    chrom_list <- foreach(file = files, .packages = ex_packages) %dopar% {

      setDTthreads(1)

      return(get_ms_chromatograms_from_file_ext(file, minIntensity))

    }

    stopCluster(cl)

    setDTthreads(0)

  } else {

    chrom_list <- lapply(files, function(file, minIntensity) {

      return(get_ms_spectra_from_file_ext(file, minIntensity))

    }, minIntensity)

  }

  names(chrom_list) <- analyses

  return(chrom_list)
})



##### loadChromatograms -------------------------------------------------------

#' @describeIn msData adds (when available) raw spectra to each
#' \linkS4class{msAnalysis} of the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases loadChromatograms,msData,msData-method
#'
setMethod("loadChromatograms", "msData", function(object, run_parallel = FALSE) {

  chrom_list <- getChromatograms(object)

  object@analyses <- lapply(object@analyses, function(x, chrom_list) {
    x@chromatograms <- chrom_list[[analysisName(x)]]
    return(x)
  }, chrom_list = chrom_list)

  return(object)
})



##### hasLoadedChromatograms --------------------------------------------------

#' @describeIn msData checks if the analyses in the \linkS4class{msData} have
#' loaded raw chromatograms.
#'
#' @export
#'
#' @aliases hasLoadedChromatograms,msData,msData-method
#'
setMethod("hasLoadedChromatograms", "msData", function(object) {

  return(sapply(object@analyses, function(x) hasLoadedChromatograms(x)))
})



##### chromatograms -----------------------------------------------------------

#' @describeIn msData getter for slot chromatograms in the
#' \linkS4class{msAnalysis} objects of the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases chromatograms,msData,msData-method
#'
setMethod("chromatograms", "msData", function(object, analyses = NULL) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  chroms <- lapply(object@analyses[analyses], function(x) {
    return(x@chromatograms)
  })

  chroms <- rbindlist(chroms, idcol = "analysis")

  return(chroms)
})



##### plotChromatograms -------------------------------------------------------

#' @describeIn msData plots chromatograms from \linkS4class{msAnalysis} in the
#' \linkS4class{msData} object.
#'
#' @export
#'
#' @param index A numeric vector with the index(s).
#'
#' @aliases plotChromatograms,msData,msData-method
#'
setMethod("plotChromatograms", "msData", function(object,
                                                  analyses = NULL,
                                                  index = NULL,
                                                  id = NULL,
                                                  colorBy = "analyses",
                                                  interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  chroms <- chromatograms(object[analyses])

  if (nrow(chroms ) == 0) {
    message("Chromatograms not found, load chromatograms
            first with loadChromatograms() method.")
    return(NULL)
  }

  chroms$replicate <- replicateNames(object)[chroms$analysis]

  if (!is.null(index)) {
    idx <- index
    chroms <- chroms[chroms$index %in% idx, ]
  }

  if (!is.null(id)) {
    id_name <- id
    chroms <- chroms[chroms$id %in% id_name, ]
  }

  cols_order <- c("analysis", "replicate", "id", "rt", "intensity")

  chroms <- chroms[, cols_order, with = FALSE]

  plotEICs(chroms, colorBy = colorBy, interactive = interactive)

})



#### parsing data ______________ ----------------------------------------------

##### EICs --------------------------------------------------------------------

#' @describeIn msData get extracted ion chromatograms (EICs)
#' for specified \emph{m/z} (Da) and retention time (seconds) targets
#' in given analyses. The arguments \code{mz}, \code{ppm}, \code{rt}
#' and \code{sec} are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @template args-makeTargets
#'
#' @export
#'
#' @aliases EICs,msData,msData-method
#'
setMethod("EICs", "msData", function(object,
                                     analyses = NULL,
                                     mz = NULL, rt = NULL,
                                     ppm = 20, sec = 60, id = NULL,
                                     run_parallel = TRUE) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  targets <- makeTargets(mz, rt, ppm, sec, id)

  if (is.null(analyses)) return(data.table())

  rtr <- targets[, c("rtmin", "rtmax")]
  colnames(rtr) <- c("min", "max")
  if ((nrow(rtr) == 1) & (TRUE %in% (rtr$max == 0))) rtr <- NULL

  specLoaded <- hasLoadedSpectra(object)[analyses]

  if (all(specLoaded)) {

    spec <- spectra(object, analyses)

  } else {

    # spec <- getSpectra(object, analyses, rtr, levels = 1,
    #                    run_parallel = run_parallel)

    files <- filePaths(object)[analyses]

    source(
      system.file("scripts/get_ms_spectra_from_file_ext.R",
                  package = "streamFind"), local = TRUE
    )

    ex_packages = c("mzR", "data.table")

    ex_globals = c("rtr", "get_ms_spectra_from_file_ext")

    workers <- detectCores() - 1

    if (length(files) < workers) workers <- length(files)

    par_type <- "PSOCK"

    if (supportsMulticore()) par_type <- "FORK"

    cat(Sys.time())

    cl <- makeCluster(workers, type = par_type)

    cat(Sys.time())

    registerDoParallel(cl)

    cat(Sys.time())

    spec_list <- foreach(i = files, .packages = ex_packages, .verbose = T) %dopar% {

      return(get_ms_spectra_from_file_ext(
        file = i , levels = 1, rtr))

    }

    stopCluster(cl)

    names(spec_list) <- analyses

    spec <- spec_list
  }

  #eics <- buildEICs2(spec, targets, run_parallel)

  chunk_list <- lapply(analyses, function(x, targets, spec) {
    return(list(spec = spec[[x]][, .(rt, mz, intensity)], targets = targets))
  }, spec = spec, targets = targets)

  if (run_parallel) {

    ex_packages = "data.table"

    source(system.file("scripts/dt_build_eics_ext.R",
                       package = "streamFind"), local = TRUE)

    #ex_globals = c("dt_bluid_eics", "trim_ranges")

    workers <- detectCores() - 1

    if (length(chunk_list) < workers) workers <- length(chunk_list)

    par_type <- "PSOCK"

    if (supportsMulticore()) par_type <- "FORK"

    cl <- makeCluster(workers, type = par_type)

    #clusterExport(cl, ex_globals, envir = environment())

    registerDoParallel(cl)

    eics_list <- foreach(chunk = chunk_list, .packages = ex_packages) %dopar% {
      return(dt_bluid_eics(chunk[[1]], chunk[[2]], trim_ranges))
    }

    stopCluster(cl)

  } else {

    eics_list <- lapply(chunk_list, function(chunk) {
      return(rcpp_ms_make_eics_for_msAnalysis(chunk[[1]], chunk[[2]]))
    })

  }

  names(eics_list) <- names(chunk_list)

  eics <- rbindlist(eics_list, idcol = "analysis", fill = TRUE)




  eics$replicate <- replicateNames(object)[eics$analysis]

  setcolorder(eics, c("replicate", "analysis"))

  intensity <- NULL

  eics <- eics[, `:=`(intensity = sum(intensity)),
                            by = c("replicate", "analysis", "id", "rt")][]

  return(eics)
})



##### plotEICs ----------------------------------------------------------------

#' @describeIn msData plots extracted ion chromatograms (EICs)
#' of data in an \linkS4class{msData} object.
#' The arguments for data collection are the same as the \link{EICs} method.
#' A \linkS4class{data.table} can be used instead.
#' The \code{colorBy} argument can be \code{"analyses"}, \code{replicates} or
#' \code{targets} (the default), for coloring by analyses, replicates or targets
#' of the EICs, respectively.  The \code{legendNames} is a character vector
#' with the same length as targets for plotting and can be used to legend
#' the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets".
#'
#' @template args_plots_colorby_legendNames_title_interactive
#'
#' @export
#'
#' @aliases plotEICs,msData,msData-method
#'
setMethod("plotEICs", "msData", function(object,
                                         analyses = NULL,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 30, id = NULL,
                                         run_parallel = FALSE,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         title = NULL,
                                         interactive = FALSE) {

  eic <- EICs(object, analyses, mz, rt, ppm, sec, id, run_parallel)

  return(
    plotEICs(eic,
      analyses = NULL, colorBy,
      legendNames, title, interactive
    )
  )
})



##### TICs --------------------------------------------------------------------

#' @describeIn msData extracts total ion chromatograms (TICs)
#' for analyses in the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases TICs,msData,msData-method
#'
setMethod("TICs", "msData", function(object,
                                     analyses = NULL,
                                     run_parallel = FALSE) {

  analysis <- NULL
  replicate <- NULL

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  tics <- runParallelLapply(
    object@analyses[analyses],
    run_parallel,
    NULL,
    function(x) {
      tic <- streamFind::TIC(x)
      p()
      return(tic)
    }
  )

  tics <- rbindlist(tics, fill = TRUE, idcol = "analysis")

  if (nrow(tics) > 0) {

    tics$replicate <- replicateNames(object)[tics$analysis]

    tics <- select(tics, analysis, replicate, everything())

  }

  return(tics)
})



##### plotTICs ----------------------------------------------------------------

#' @describeIn msData plots the total ion chromatogram (TIC) for analyses
#' in the \linkS4class{msData} object. The \code{colorBy} argument can be
#' "analyses" or "replicates" to color the plot by analyses or
#' by analysis replicates.
#'
#' @export
#'
#' @aliases plotTICs,msData,msData-method
#'
setMethod("plotTICs", "msData", function(object,
                                         analyses = NULL,
                                         run_parallel = FALSE,
                                         colorBy = "analyses",
                                         title = NULL,
                                         interactive = FALSE) {

  tics <- TICs(object, analyses = analyses, run_parallel = run_parallel)

  return(
    plotTICs(tics,
      analyses = NULL, colorBy = colorBy,
      title = title, interactive = interactive
    )
  )
})



##### BPCs --------------------------------------------------------------------

#' @describeIn msData extracts base peak chromatograms (BPCs)
#' for analyses in the \linkS4class{msData} object.
#'
#' @export
#'
#' @aliases BPCs,msData,msData-method
#'
setMethod("BPCs", "msData", function(object,
                                     analyses = NULL,
                                     run_parallel = FALSE) {

  analysis <- NULL
  replicate <- NULL

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  bpcs <- runParallelLapply(
    object@analyses[analyses],
    run_parallel,
    NULL,
    function(x) {
      bpc <- streamFind::BPC(x)
      p()
      return(bpc)
    }
  )

  bpcs <- rbindlist(bpcs, fill = TRUE, idcol = "analysis")

  if (nrow(bpcs) > 0) {

    bpcs$replicate <- replicateNames(object)[bpcs$analysis]

    bpcs <- select(bpcs, analysis, replicate, everything())

  }

  return(bpcs)
})



##### plotBPCs ----------------------------------------------------------------

#' @describeIn msData plots the base peak chromatogram (BPC) of analyses
#' in the \linkS4class{msData} object. The \code{colorBy} argument can be
#' "analyses" or "replicates" to color the plot by analyses or
#' by analysis replicates.
#'
#' @export
#'
#' @aliases plotBPCs,msData,msData-method
#'
setMethod("plotBPCs", "msData", function(object,
                                         analyses = NULL,
                                         run_parallel = FALSE,
                                         colorBy = "analyses",
                                         title = NULL,
                                         interactive = FALSE) {

  bpcs <- BPCs(object, analyses = analyses, run_parallel = run_parallel)

  return(
    plotBPCs(bpcs,
             analyses = NULL, colorBy = colorBy,
             title = title, interactive = interactive
    )
  )
})



##### XICs --------------------------------------------------------------------

#' @describeIn msData get three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention
#' time pair targets in analyses of an \linkS4class{msData} object.
#' The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id}
#' are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#'
#' @export
#'
#' @aliases XICs,msData,msData-method
#'
setMethod("XICs", "msData", function(object,
                                     analyses = NULL,
                                     mz = NULL, rt = NULL,
                                     ppm = 20, sec = 60, id = NULL,
                                     run_parallel = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  targets <- makeTargets(mz, rt, ppm, sec, id)

  if (is.null(analyses)) return(data.table())

  rtr <- targets[, c("rtmin", "rtmax")]
  colnames(rtr) <- c("min", "max")
  if ((nrow(rtr) == 1) & (TRUE %in% (rtr$max == 0))) rtr <- NULL

  specLoaded <- hasLoadedSpectra(object)[analyses]

  if (all(specLoaded)) {

    spec <- spectra(object, analyses)

  } else {

    spec <- getSpectra(object, analyses, rtr, levels = 1,
                       run_parallel = run_parallel)

  }

  xics <- buildEICs(spec, targets, run_parallel)

  xics$replicate <- replicateNames(object)[xics$analysis]

  setcolorder(xics, c("analysis", "replicate", "id", "rt", "mz", "intensity"))

  return(xics)
})



##### plotXICs ----------------------------------------------------------------

#' @describeIn msData plots three dimensional (\emph{m/z}, time and intensity)
#' extracted ion chromatograms (XICs) for specified \emph{m/z} and retention
#' time pair targets in analyses of an \linkS4class{msData} object.
#' The arguments \code{mz}, \code{ppm}, \code{rt}, \code{sec} and \code{id}
#' are used to construct the targets.
#' See ?\link{makeTargets} for more information.
#' When \code{plotTargetMark} is \code{TRUE} a target is plotted representing
#' the deviations as defined by the arguments \code{ppmMark} and \code{secMark}
#' in ppm and seconds, respectively. When ranges were given to build the XIC,
#' exact \emph{m/z} and time targets can be specified with the argument
#' \code{targetsMark}. \code{targetsMark} should be a two column table
#' named mz and rt with exact \emph{m/z} and time targets. Note that the number
#' of rows should be the same as the number of target in the XIC.
#' The number of rows to plot multiple targets can be defined by the
#' \code{numberRows} argument.
#'
#' @template args_plots_xics
#'
#' @export
#'
#' @aliases plotXICs,msData,msData-method
#'
setMethod("plotXICs", "msData", function(object,
                                         analyses = NULL,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 60, id = NULL,
                                         run_parallel = FALSE,
                                         legendNames = NULL,
                                         plotTargetMark = TRUE,
                                         targetsMark = NULL,
                                         ppmMark = 5,
                                         secMark = 10,
                                         numberRows = 1) {

  xics <- XICs(object, analyses, mz, rt, ppm, sec, id, run_parallel)

  if (is.null(targetsMark)) {
    targetsMark <- makeTargets(mz, rt, ppm, sec, id)
    targetsMark <- targetsMark[id %in% xics$id, ]
  }

  plot <- plotXICs(xics,
    legendNames = legendNames,
    plotTargetMark = plotTargetMark,
    targetsMark = targetsMark[, c("id", "mz", "rt")],
    ppmMark = ppmMark,
    secMark = secMark,
    numberRows = numberRows
  )

  return(plot)
})

##### MS2s --------------------------------------------------------------------

#' @describeIn msData gets MS2 data for specified \emph{m/z} and retention
#' time (seconds) targets in analyses of the \linkS4class{msData} object.
#' The argument \code{isolationWindow} defines the range in Da that was applied
#' to isolate the MS1 precursor. The argument \code{mzClust} defines the
#' deviation in Da for merging MS2 spectrum of a given target.
#' The \code{mzClust} will be reduced when more than one trace is found for the
#' same retention time value.
#'
#' @param mzClust A numeric value defining the \emph{m/z} cutoff (in Da) to
#' cluster mass traces from different scans.
#' @param isolationWindow A numeric value defining the isolation window (in Da)
#' applied to isolate the MS1 precursor.
#'
#' @export
#'
#' @aliases MS2s,msData,msData-method
#'
setMethod("MS2s", "msData", function(object = NULL,
                                     analyses = NULL,
                                     isolationWindow = 1,
                                     mzClust = 0.01,
                                     mz = NULL, rt = NULL,
                                     ppm = 20, sec = 60, id = NULL,
                                     run_parallel = FALSE) {

  analysis <- NULL
  replicate <- NULL

  analyses <- checkAnalysesArgument(object, analyses)

  if (is.null(analyses)) return(data.table())

  targets <- makeTargets(mz, rt, ppm, sec, id)

  if (is.null(analyses)) return(data.table())

  ms2s <- runParallelLapply(
    object@analyses[analyses],
    run_parallel,
    NULL,
    function(x, targets, mzClust, isolationWindow) {
      ms2 <- streamFind::MS2s(x,
        mz = targets,
        mzClust = mzClust,
        isolationWindow = isolationWindow
      )
      p()
      return(ms2)
    }, targets = targets, mzClust = mzClust, isolationWindow = isolationWindow
  )

  ms2s <- rbindlist(ms2s, idcol = "analysis")

  ms2s$replicate <- replicateNames(object)[ms2s$analysis]

  ms2s <- select(ms2s, analysis, replicate, everything())

  return(ms2s)
})



##### plotMS2s ----------------------------------------------------------------

#' @describeIn msData plots MS2 data for specified \emph{m/z} and retention
#' time (seconds) targets in analyses of the \linkS4class{msData} object.
#' The argument \code{isolationWindow} defines the range in Da that was applied
#' to isolate the MS1 precursor. The argument \code{mzClust} defines the
#' deviation in Da for merging MS2 spectrum of a given target.
#' The \code{mzClust} will be reduced when more than one trace is found for the
#' same retention time value. The possible values for the \code{colorBy}
#' argument are "targets", "analyses" and "replicates" to color by
#' each target, analyses or replicate analysis groups, respectively.
#'
#' @export
#'
#' @aliases plotMS2s,msData,msData-method
#'
setMethod("plotMS2s", "msData", function(object = NULL,
                                         analyses = NULL,
                                         isolationWindow = 1,
                                         mzClust = 0.01,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 60, id = NULL,
                                         run_parallel = FALSE,
                                         legendNames = NULL,
                                         title = NULL,
                                         colorBy = "targets",
                                         interactive = FALSE) {

  ms2s <- MS2s(object, analyses, isolationWindow, mzClust,
               mz, rt, ppm, sec, id, run_parallel)

  if (nrow(ms2s) < 1) return(cat("Data was not found for any of the targets!"))

  return(
    plotMS2s(ms2s, legendNames = legendNames, title = title,
      colorBy = colorBy, interactive = interactive
    )
  )
})



#### settings __________________-----------------------------------------------

##### addSettings -------------------------------------------------------------

#' @describeIn msData adds processing settings to \linkS4class{msAnalysis} or
#' \linkS4class{msFeatures} objects in the \linkS4class{msData} as defined by
#' the argument \code{where}. So where can be either "analyses" or "features".
#'
#' @template args-single-settings
#' @param where A character vector defining where to add the \linkS4class{settings}.
#' @template args-single-analyses
#'
#' @export
#'
#' @aliases addSettings,msData,msData-method
#'
setMethod("addSettings", "msData", function(object,
                                            settings,
                                            where = "analyses",
                                            analyses = NULL) {

  valid <- FALSE

  valid <- testClass(settings, "settings")

  valid <- testChoice(where, c("analyses", "features"))

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  if (where %in% "analyses") {

    analyses <- checkAnalysesArgument(object, analyses)

    for (ana in analyses) {
      object@analyses[[ana]]@settings[[settings@call]] <- settings
    }
  }

  if (where %in% "features") {
    object@features@settings[[getCall(settings)]] <- settings
  }

  return(object)
})



##### getSettingsNames --------------------------------------------------------

#' @describeIn msData gets the call names of processing settings in the
#' \linkS4class{msData} object for both analyses and features.
#'
#' @export
#'
#' @aliases getSettingsNames,msData,msData-method
#'
setMethod("getSettingsNames", "msData", function(object) {

  stgs <- list(
    "features" = names(object@features@settings),
    "analysis" = sapply(object@analyses, function(x) names(x@settings))
  )

  return(stgs)
})



##### getSettings -------------------------------------------------------------

#' @describeIn msData gets processing settings from \linkS4class{msAnalysis} or
#' \linkS4class{msFeatures} objects in the \linkS4class{msData} as defined by
#' the argument \code{where}. So where can be either "analyses" or "features".
#' The argument \code{call} is used to specify which settings are returned
#' from the object defined in \code{where}. When, \code{call} is \code{NULL}
#' all the settings are returned.
#'
#' @param where A character vector defining where to get the \linkS4class{settings}.
#' @template args-single-analyses
#' @param call The call name of the settings to retrieve.
#'
#' @export
#'
#' @aliases addSettings,msData,msData-method
#'
setMethod("getSettings", "msData", function(object,
                                            where = "analyses",
                                            analyses = NULL,
                                            call = NULL) {

  valid <- testChoice(where, c("analyses", "features"))

  if (!valid) {
    warning("Arguments not correct, returning original object!")
    return(object)
  }

  if (where %in% "analyses") {

    analyses <- checkAnalysesArgument(object, analyses)

    param <- sapply(analyses, function(x, object, call) {

      if (is.null(call)) {
        object@analyses[[x]]@settings
      } else {
        object@analyses[[x]]@settings[[call]]
      }
    }, object = object, call)

  }

  if (where %in% "features") {
    if (is.null(call)) {
      param <- object@features@settings
    } else {
      param <- object@features@settings[[call]]
    }
  }


  return(param)
})



#### peak methods _____________------------------------------------------------

##### hasPeaks ----------------------------------------------------------------

#' @describeIn msData checks if the \linkS4class{msAnalysis} objects in the
#' \linkS4class{msData} object have peaks.
#'
#' @export
#'
#' @aliases hasPeaks,msData,msData-method
#'
setMethod("hasPeaks", "msData", function(object) {
  return(sapply(object@analyses, function(x) hasPeaks(x)))
})



##### get_peaks -------------------------------------------------------------------

#' @describeIn msData getter for chromatographic peaks from the
#' \linkS4class{msAnalysis} objects in the \linkS4class{msData} object.
#' The arguments \code{targetID} and \code{mass}, \code{mz} and \code{rt}
#' can be used to select specific peaks. The \emph{id} of peaks and/or
#' features can be given in the \code{targetsID} argument to select the
#' respective peaks. Also, analyses can be selected using the \code{analyses}
#' argument. When the \code{filtered} argument is set to \code{TRUE}, filtered
#' peaks are also returned.
#'
#' @param mass ...
#' @template args-single-targetsID
#' @template args-single-filtered
#'
#' @export
#'
#' @aliases get_peaks,msData,msData-method
#'
setMethod("get_peaks", "msData", function(object,
                                      analyses = NULL,
                                      targetsID = NULL,
                                      mass = NULL,
                                      mz = NULL, rt = NULL,
                                      ppm = 20, sec = 60,
                                      filtered = TRUE) {

  analyses <- checkAnalysesArgument(object, analyses)

  analyses <- object@analyses[analyses]

  pks <- lapply(analyses, function(x, targetsID, mass, mz, rt, ppm, sec,
                                   filtered) {

    pks_a <- peaks(x, targetsID, mass, mz, rt, ppm, sec, filtered)

    return(pks_a)
  },
  filtered = filtered,
  targetsID = targetsID,
  mass = mass,
  mz = mz,
  rt = rt,
  ppm = ppm,
  sec = sec
  )

  pks <- rbindlist(pks, idcol = "analysis")

  if (nrow(pks) > 0) pks$replicate <- replicateNames(object)[pks$analysis]

  return(pks)
})



##### peakEICs ----------------------------------------------------------------

#' @describeIn msData getter of peak EICs from \linkS4class{msAnalysis}
#' objects in the \linkS4class{msData} object.
#' The arguments \code{targetID} and \code{mass}, \code{mz} and \code{rt}
#' can be used to select specific peaks. The \emph{id} of peaks and/or
#' features can be given in the \code{targetsID} argument to select the
#' respective peaks. Also, analyses can be selected using the \code{analyses}
#' argument. When the \code{filtered} argument is set to \code{TRUE}, filtered
#' peaks are also returned.
#'
#' @export
#'
#' @aliases plotPeaks,msData,msData-method
#'
setMethod("peakEICs", "msData", function(object,
                                         analyses = NULL,
                                         targetsID = NULL,
                                         mass = NULL,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 30,
                                         rtExpand = 120,
                                         mzExpand = 0.005,
                                         filtered = TRUE,
                                         run_parallel = FALSE) {

  pks <- peaks(object, analyses, targetsID, mass, mz, rt, ppm, sec, filtered)

  cols_keep <- c("analysis", "id", "mz", "rt", "mzmin", "mzmax", "rtmin", "rtmax")

  pks_tars <- copy(pks[, cols_keep, with = FALSE])

  if (nrow(pks_tars) == 0) {
    warning("No peaks were found with the defined targets!")
    return(NULL)
  }

  analyses <- unique(pks_tars$analysis)

  pks_tars <- split(pks_tars, pks_tars$analysis)

  eics <- runParallelLapply(
    object@analyses[analyses],
    run_parallel,
    NULL,
    function(x, targets, rtExpand) {
      ana <- analysisName(x)
      eic <- peakEICs(x,
                      targetsID = targets[[ana]]$id,
                      rtExpand = rtExpand,
                      mzExpand = mzExpand)
      eic$analysis <- ana
      p()
      return(eic)
    }, targets = pks_tars, rtExpand = rtExpand
  )

  eics <- rbindlist(eics)

  if (nrow(eics) > 0) {
    eics$replicate <- replicateNames(object)[eics$analysis]
    setcolorder(eics, c("replicate", "analysis"))
  }

  return(eics)
})



##### plotPeaks ---------------------------------------------------------------

#' @describeIn msData plots chromatographic peaks from \linkS4class{msAnalysis}
#' objects in the \linkS4class{msData} object.
#' The arguments \code{targetID} and \code{mass}, \code{mz} and \code{rt}
#' can be used to select specific peaks. The \emph{id} of peaks and/or
#' features can be given in the \code{targetsID} argument to select the
#' respective peaks. Also, analyses can be selected using the \code{analyses}
#' argument. When the \code{filtered} argument is set to \code{TRUE}, filtered
#' peaks are also returned.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates}
#' or \code{targets} (the default), for coloring by analyses, replicates or
#' peak targets, respectively. The \code{legendNames} is a character vector
#' with the same length as targets for plotting and can be used to legend
#' the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets" automatically.
#'
#' @export
#'
#' @aliases plotPeaks,msData,msData-method
#'
setMethod("plotPeaks", "msData", function(object,
                                          analyses = NULL,
                                          targetsID = NULL,
                                          mass = NULL,
                                          mz = NULL, rt = NULL,
                                          ppm = 20, sec = 30,
                                          rtExpand = 60,
                                          mzExpand = 0.005,
                                          filtered = TRUE,
                                          run_parallel = FALSE,
                                          colorBy = "targets",
                                          legendNames = NULL,
                                          title = NULL,
                                          interactive = FALSE) {

  peaks <- peaks(object, analyses, targetsID, mass, mz, rt, ppm, sec, filtered)

  eics <- peakEICs(object,
                   analyses, targetsID,
                   mass, mz, rt, ppm, sec,
                   rtExpand, mzExpand,
                   filtered, run_parallel)

  return(
    plotPeaks(eics, peaks,
              analyses = NULL,
              colorBy = colorBy,
              legendNames = legendNames,
              title = title,
              interactive = interactive
    )
  )
})



##### mapPeaks ----------------------------------------------------------------

#' @describeIn msData mapps peaks mass and time space.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates}
#' or \code{targets} (the default), for coloring by analyses, replicates or
#' peak targets, respectively. The \code{legendNames} is a character vector
#' with the same length as targets for plotting and can be used to legend the
#' plot. Note that, by setting \code{legendNames} the \code{colorBy} is set to
#' "targets" automatically.
#'
#' @param xlim A length one or two numeric vector
#' for setting the \emph{x} limits of a plot.
#' @param ylim A length one or two numeric vector
#' for setting the \emph{y} limits of a plot.
#'
#' @export
#'
#' @aliases mapPeaks,msData,msData-method
#'
setMethod("mapPeaks", "msData", function(object,
                                         analyses = NULL,
                                         targetsID = NULL,
                                         mass = NULL,
                                         mz = NULL, rt = NULL,
                                         ppm = 20, sec = 30,
                                         filtered = TRUE,
                                         colorBy = "targets",
                                         legendNames = NULL,
                                         xlim = 30,
                                         ylim = 0.05,
                                         title = NULL,
                                         showLegend = TRUE,
                                         interactive = FALSE) {

  peaks <- peaks(
    object,
    analyses,
    targetsID,
    mass,
    mz, rt,
    ppm, sec,
    filtered
  )

  if (nrow(peaks) < 1) return(cat("Requested peaks were not found!"))

  if (colorBy == "analyses") {
    leg <- unique(peaks$analysis)
    varkey <- peaks$analysis
  } else if (colorBy == "replicates") {
    leg <- unique(peaks[, c("analysis", "replicate")])
    leg <- leg$replicate
    varkey <- peaks$replicate
  } else if ((!is.null(legendNames)) &
             (length(legendNames) == length(unique(peaks$id)))) {
    leg <- legendNames
    names(leg) <- unique(peaks$id)
    varkey <- sapply(peaks$id, function(x) leg[x])
  } else {
    leg <- peaks$id
    names(leg) <- peaks$id
    varkey <- sapply(peaks$id, function(x) leg[names(leg) == x])
  }

  peaks[, var := varkey][]

  if (!interactive) {

    return(
      mapPeaksStatic(peaks, xlim, ylim, title,
                     showLegend = showLegend)
    )

  } else {

    plot <- mapPeaksInteractive(peaks, xlim, ylim, title)

    return(plot)
  }
})



#### feature methods ___________-----------------------------------------------

##### features ----------------------------------------------------------------

#' @describeIn msData getter for features (i.e., grouped peaks). When complete
#' is set to \code{TRUE}, additional feature metadata is also returned.
#'
#' @param complete Logical, set to \code{TRUE} for a complete version of the
#' output.
#' @param average Logical, set to \code{TRUE} for returning the intensity of
#' features averaged for each analysis replicate group.
#'
#' @export
#'
#' @aliases features,msData,msData-method
#'
setMethod("features", "msData", function(object,
                                         targetsID = NULL,
                                         mass = NULL,
                                         mz = NULL, ppm = 20,
                                         rt = NULL, sec = 60,
                                         filtered = TRUE,
                                         complete = FALSE,
                                         average = TRUE) {

  if (!hasFeatures(object@features)) return(data.table())

  return(
    features(
      object = object@features,
      targetsID = targetsID,
      mass = mass,
      mz = mz, ppm = ppm,
      rt = rt, sec = sec,
      filtered = filtered,
      complete = complete,
      average = average
    )
  )
})



### plotFeatures --------------------------------------------------------------------------------------------

#' @describeIn msData A method for plotting peaks from given features
#' in an \linkS4class{msData} object.
#' The \code{colorBy} argument can be be \code{"analyses"}, \code{replicates} or \code{targets}
#' (the default), for coloring by analyses, replicates or peak targets, respectively.
#' The \code{legendNames} is a character vector with the same length as targets for plotting and
#' can be used to lengend the plot. Note that, by setting \code{legendNames} the \code{colorBy}
#' is set to "targets" automatically.
#'
#' @export
#'
#' @aliases plotFeatures,msData,msData-method
#'
setMethod("plotFeatures", "msData", function(object,
                                             analyses = NULL,
                                             targetsID = NULL,
                                             mass = NULL,
                                             mz = NULL, ppm = 20,
                                             rt = NULL, sec = 30,
                                             filtered = TRUE,
                                             colorBy = "targets",
                                             legendNames = NULL,
                                             title = NULL,
                                             interactive = FALSE) {

  analyses <- checkAnalysesArgument(object, analyses)
  obj <- object[which(analyses %in% analysisNames(object))]

  feats <- features(
    object = obj,
    targetsID = targetsID,
    mass = mass,
    mz = mz,
    ppm = ppm,
    rt = rt,
    sec = sec,
    filtered = filtered
  )

  peaks <- peaks(
    obj,
    analyses,
    targetsID = feats$id,
    filtered = filtered
  )

  if ((!is.null(legendNames)) &
      (length(legendNames) == length(unique(peaks$feature)))) {

    names(legendNames) <- unique(peaks$feature)
    peaks$feature <- sapply(peaks$feature, function(x) legendNames[x])
    names(legendNames) <- peaks$id

  } else if (colorBy %in% "targets") {

    legendNames <- peaks$feature
    names(legendNames) <- peaks$id

  }

  cols_keep <- c(
    "analysis", "replicate", "id", "mz", "rt",
    "mzmin", "mzmax", "rtmin", "rtmax"
  )

  pks_tars <- copy(peaks[, cols_keep, with = FALSE])
  pks_tars$rtmin <- min(pks_tars$rtmin) - 60
  pks_tars$rtmax <- max(pks_tars$rtmax) + 60

  eic <- lapply(object@analyses, function(x, pks_tars, object) {
    eic <- EICs(x, mz = pks_tars[pks_tars$analysis %in% analysisNames(x), ])
    eic$replicate <- replicateNames(object)[analysisNames(x)]
    return(eic)
  }, pks_tars = pks_tars, object = object)

  eic <- rbindlist(eic)

  adj_rt <- hasAdjustedRetentionTime(obj)
  if (TRUE %in% adj_rt) {
    for (i in names(adj_rt)[adj_rt]) {
      eic[eic$analysis == i, rt := unlist(sapply(rt, function(x, ana) {
        temp <- copy(ana@spectra[rt == x, ])
        return(temp$rtAdjusted)
      }, ana = getAnalyses(obj, analyses = i)))]

      # TODO Adds the retention adjustment to the peak average, not working
      # pks[analysis == i, rt := unlist(sapply(rt, function(x, ana) {
      #   temp <- copy(ana@spectra[rt == x, ])
      #   return(temp$rtAdjusted)
      # }, ana = getAnalyses(obj, analyses = i)))]
    }
  }

  return(
    plotPeaks(eic, peaks, analyses = NULL,
              colorBy = colorBy,
              legendNames = legendNames,
              title = title,
              interactive = interactive
    )
  )
})



### annotation -----------------------------------------------------------

#' @describeIn msData getter for annotation (i.e., isotopes and adducts clusters).
#' When giving the argument \code{all} as \code{TRUE},
#' all the features within the target feature annotation cluster are included in the output.
#'
#' @param all Logical, set to \code{TRUE} for displaying all features/peaks.
#'
#' @export
#'
#' @aliases annotation,msData,msData-method
#'
setMethod("annotation", "msData", function(object,
                                           targetsID = NULL,
                                           mass = NULL,
                                           mz = NULL, ppm = 20,
                                           rt = NULL, sec = 60, id = NULL,
                                           all = FALSE) {

  feats <- features(
    object = object@features,
    targetsID = targetsID,
    mass = mass,
    mz = mz, ppm = ppm,
    rt = rt, sec = sec,
    filtered = TRUE,
    complete = TRUE,
    average = TRUE
  )

  if (!"component" %in% colnames(feats)) {
    return(cat("Annotation seems to not be present in the given object!"))
  }

  all_feats <- features(object, complete = TRUE, average = TRUE)

  if (all) {
    outfeats <- all_feats[component %in% feats$component, ]
  } else {
    outfeats <- all_feats[(iso_id %in% feats$id | mass %in% feats$mass) & component %in% feats$component, ]
  }

  return(outfeats)
})


### plotAnnotation -------------------------------------------------------

#' @describeIn msData plots peaks from given feature annotation targets
#' in the \linkS4class{msData} object. The \code{colorBy} argument can be
#' set to \code{"isotopes"} for coloring by monoisotopic mass instead of
#' neutral mass of the cluster.
#'
#' @export
#'
#' @aliases plotAnnotation,msData,msData-method
#'
setMethod("plotAnnotation", "msData", function(object,
                                               targetsID = NULL,
                                               mass = NULL,
                                               mz = NULL, ppm = 20,
                                               rt = NULL, sec = 30, id = NULL,
                                               all = FALSE,
                                               colorBy = "mass") {

  comps <- annotation(
    object = object,
    targetsID = targetsID,
    mass = mass,
    mz = mz,
    ppm = ppm,
    rt = rt,
    sec = sec,
    all = all
  )

  return(
    plotAnnotationInteractive(
      object = object,
      comps = comps,
      colorBy = colorBy
    )
  )
})



#### sub-setting _______________-----------------------------------------------

##### on analyses --------------------------------------------------------------

#' @describeIn msData subset on analyses, using indexes or analysis names.
#'
#' @template args-single-i-subsetting
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "missing", "missing"), function(x, i, ...) {

  if (!missing(i)) {

    x <- callNextMethod()

    x@features <- x@features[i]

    #when has features, updates feature column in peaks slot of each analysis
    if (nrow(x@features@metadata) > 0) {
      f_id <- features(x)[["id"]]

      x@analyses <- lapply(x@analyses, function(z, f_id) {

        temp <- z@peaks

        temp[!temp$feature %in% f_id, `:=`(feature = NA_character_,
                                           filtered = TRUE,
                                           filter = "grouping")]
        z@peaks <- copy(temp)

        return(z)

      }, f_id = f_id)
    }
    return(x)
  }

  return(x)
})



##### on features -------------------------------------------------------------

#' @describeIn msData subset on analyses and features, using index or name.
#' Note that this method is irreversible.
#'
#' @param x An \linkS4class{msData} object.
#' @param i The indice/s or name/s of the analyses to keep in \code{x}.
#' @param j The indice/s or \emph{id}/s for of features to keep.
#' @param drop Not applicable to \linkS4class{msData}.
#' @param ... Other arguments.
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "ANY", "missing"), function(x, i, j, ...) {

  if (!missing(i)) x <- x[i]

  if (!missing(j)) {

    if (nrow(x@features@metadata) == 0) {
      warning("There are no features in the msData object!")
      return(x)
    }

    if (!is.character(j)) j <- features(x)$id[j]

    x@features@intensity <- x@features@intensity[id %in% j, ]
    x@features@metadata <- x@features@metadata[id %in% j, ]

    x@analyses <- lapply(x@analyses, function(z, j) {
      temp <- copy(z@peaks)
      temp[!(temp$feature %in% j), `:=`(feature = NA_character_, filtered = TRUE, filter = "grouping")]
      z@peaks <- temp
      return(z)
    }, j = j)
  }

  return(x)
})



##### on peaks ----------------------------------------------------------------

#' @describeIn msData subset on analyses, features and peaks, using index or name.
#' Note that this method is irreversible.
#'
#' @param p The indice/s or \emph{id}/s of peaks to keep.
#'
#' @export
#'
setMethod("[", c("msData", "ANY", "ANY", "ANY"), function(x, i, j, p) {

  if (!missing(i)) x <- x[i]

  if (!missing(j)) x <- x[, j]

  if (!missing(p)) {

    if (!is.character(p)) {
      p <- peaks(x)$id[p]
      x@analyses <- lapply(x@analyses, function(z, p) {
        temp <- copy(z@peaks)
        temp <- temp[id %in% p, ]
        z@peaks <- temp
        return(z)
      }, p = p)

    #sub-sets the index of each analysis
    } else {
      x@analyses <- lapply(x@analyses, function(z, p) {
        temp <- copy(z@peaks)
        temp <- temp[p, ]
        z@peaks <- temp
        return(z)
      }, p = p)
    }
  }

  return(x)
})



#### metadata _________________ -----------------------------------------------

##### getMetadataNames --------------------------------------------------------

#' @describeIn msData getter for the names of metadata in each analyses.
#' When simplify is set to \code{TRUE}, a character vector of unique names for
#' all analyses is returned instead of a list with metadata names for
#' each analysis.
#'
#' @param simplify Logical, set to \code{TRUE} for simplifying
#' the returned object.
#'
#' @export
#'
#' @aliases getMetadataNames,msData,msData-method
#'
setMethod("getMetadataNames", "msData", function(object, simplify = FALSE) {

  meta <- lapply(object@analyses, function(x) names(x@metadata))

  if (simplify) {
    meta <- unique(unname(unlist(meta)))
  }

  return(meta)
})



##### getMetadata -------------------------------------------------------------

#' @describeIn msData getter for analyses metadata.
#' Returns a \code{data.table} with each metadata entry as a column and each
#' analysis as a row. The \code{which} argument is used to filter the metadata
#' entries to be returned. When \code{which} is \code{NULL}, all entries are
#' returned. Metadata entries with multiple values are returned as list in the
#' \code{data.table} cell. Metadata not present in a given analysis is assigned
#' as NA in the returned \code{data.table}.
#'
#' @param analyses A numeric or character vector with the index or
#' analysis name/s. When \code{NULL}, all the analyses are used.
#' @param which A character vector with the entry name/s.
#'
#' @export
#'
#' @aliases getMetadata,msData,msData-method
#'
setMethod("getMetadata", "msData", function(object, analyses = NULL, which = NULL) {

  if (!is.null(analyses)) object <- object[analyses]

  meta_names <- getMetadataNames(object, simplify = TRUE)

  if (!is.null(which)) {

    meta_names <- meta_names[meta_names %in% which]

    if (length(meta_names) < 1) {
      warning("Metadata defined by which were not found")
      return(list())
    }
  }

  mtd <- lapply(object@analyses, function(x, meta_names) {
    temp <- getMetadata(x, which = meta_names)
    temp <- lapply(temp, function(z) {
      if (length(z) > 1) z <- list(z)
      return(z)
    })
    temp <- as.data.table(temp)
    return(temp)
  }, meta_names = meta_names)

  mtd <- rbindlist(mtd, idcol = "analysis", fill = TRUE)

  return(mtd)
})



##### addMetadata -------------------------------------------------------------

#' @describeIn msData setter for analyses metadata.
#'
#' @param metadata A list with a named vector of metadata for each analyses
#' in the \linkS4class{msData} object or a \code{data.frame} or
#' \code{data.table} with metadata added as columns and with the number of
#' rows equal to the number of analyses in the \linkS4class{msData} object.
#' @param overwrite Logical, set to \code{TRUE} to overwrite existing metadata
#' entries.
#'
#' @export
#'
#' @aliases addMetadata,msData,msData-method
#'
setMethod("addMetadata", "msData", function(object,
                                            metadata = NULL,
                                            overwrite = FALSE) {

  if (is.data.frame(metadata) | is.data.table(metadata)) {

    if (nrow(metadata) != length(object@analyses)) {
      warning("Number of rows must be the same
               as the number of analyses!")
      return(object)
    }

    if (!"analysis" %in% colnames(metadata)) {
      metdata$analysis <- analysisNames(object)
    }

    metadata <- split(metadata, metadata$analysis)

    metadata <- lapply(metadata, function(x) {
      x[["analysis"]] <- NULL
      return(unlist(x))
    })

  } else if (is.list(metadata)) {

    if (length(metadata) != length(object@analyses)) {
      warning("Metadata list must be the same
               length as the number of analyses!")
      return(object)
    }

    if (all(sapply(metadata, function(x) is.null(names(x))))) {
      warning("Metadata must be a named vector!")
      return(object)
    }

    if (TRUE %in% is.null(names(metadata))) {
      names(metadata) <- analysisNames(object)
    }

  }

  object@analyses <- lapply(object@analyses, function(x, metadata, overwrite) {
    x <- addMetadata(x,
                     metadata = metadata[[analysisName(x)]],
                     overwrite = overwrite)
    return(x)
  }, metadata = metadata, overwrite = overwrite)

  return(object)
})



#### others ___________________ -----------------------------------------------

#### _hasAdjustedRetentionTime_ -----------------------------------------------

#' @describeIn msData getter for presence of adjusted retention time
#' in the analyses.
#'
#' @export
#'
#' @aliases hasAdjustedRetentionTime,msData,msData-method
#'
setMethod("hasAdjustedRetentionTime", "msData", function(object) {

  return(sapply(object@analyses,
                function(x) "rtAdjusted" %in% colnames(x@spectra)))
})



#### _as.features_ ------------------------------------------------------------

#' @describeIn msData converts the \linkS4class{msData}
#' to a \linkS4class{features} object from the package \pkg{patRoon}.
#'
#' @export
#'
#' @aliases as.features,msData,msData-method
#'
setMethod("as.features", "msData", function(object) {

  requireNamespace("patRoon")

  anaInfo <- analysisInfo(object)

  feat <- lapply(object@analyses, function(x) {

    ft <- copy(x@peaks)

    if ("filtered" %in% colnames(ft)) ft <- ft[!ft$filtered, ]

    setnames(ft,
             c("id", "rt", "rtmin", "rtmax", "feature"),
             c("ID", "ret", "retmin", "retmax", "group"),
             skip_absent = TRUE
    )

    if (nrow(ft) == 0) return(ft)

    ft <- select(ft,
                 ID, ret, mz, area, intensity, retmin, retmax, mzmin, mzmax,
                 everything()
    )

    ft$index <- seq_len(nrow(ft))
    ft$ID <- gsub("_p.*", "", ft$ID)
    ft$ID <- paste0(ft$ID, "-p", ft$index)

    return(ft)
  })

  # change mz to mass for setWorkflows in patRoon
  if (length(unique(polarities(object))) > 1 | "both" %in% polarities(object)) {

    feat <- lapply(feat, function(x) {

      x$mzmin <- x$mass - (x$mz - x$mzmin)
      x$mzmax <- x$mass + (x$mzmax - x$mz)
      x$mz <- x$mass
      x$mass <- NULL

      return(x)
    })

    feat_obj <- new("featuresSet",
                    features = feat,
                    analysisInfo = anaInfo,
                    algorithm = "openms-set")

  } else {

    feat_obj <- new("featuresOpenMS", features = feat, analysisInfo = anaInfo)

  }

  return(feat_obj)
})



#### _as.featureGroups_ -------------------------------------------------------

#' @describeIn msData converts the \linkS4class{msData}
#' to a \linkS4class{featureGroups} object from the package \pkg{patRoon}.
#'
#' @export
#'
#' @aliases as.featureGroups,msData,msData-method
#'
setMethod("as.featureGroups", "msData", function(object) {

  anaInfo <- analysisInfo(object)

  feat <- as.features(object)

  if ("filtered" %in% colnames(object@features@metadata)) {
    feat_id_notFiltered <- object@features@metadata$id[!object@features@metadata$filtered]
  } else {
    feat_id_notFiltered <- object@features@metadata$id
  }

  groups_temp <- features(object, targetsID = feat_id_notFiltered, average = FALSE)
  groups <- copy(groups_temp)
  groups <- t(groups[, id := NULL])
  groups <- as.data.table(groups)
  #colnames(groups) <- groups_temp$id

  groupInfo_temp <- object@features@metadata[ id %in% feat_id_notFiltered, ]
  groupInfo <- copy(groupInfo_temp)
  groupInfo <- as.data.frame(groupInfo[, .(rt, mz, id)])
  colnames(groupInfo) <- c("rts", "mzs", "id")

  new_id <- groupInfo_temp$id

  # make group id as patRoon, so far works without it
  # new_id <- object@features@metadata[, .(index, mz, rt)]
  # new_id <- paste0("M", round(new_id$mz, digits = 0),
  #                 "_R", round(new_id$rt, digits = 0),
  #                 "_", new_id$index)

  colnames(groups) <- new_id
  rownames(groups) <- seq_len(nrow(groups))

  rownames(groupInfo) <- new_id
  groupInfo[["id"]] <- NULL


  tp_pks <- setNames(
    data.frame(
      matrix(ncol = length(analysisNames(object)),
             nrow = 1)), analysisNames(object)
  )

  tp_pks[1, ] <- 0

  ftindex <- lapply(groups_temp$id, function(x, pk, tp_pks) {
    idx <- pk[pk$group %in% x, .(analysis, index)]
    temp <- copy(tp_pks)
    temp[1, colnames(temp) %in% idx$analysis] <- idx$index
    return(temp)
  }, pk = patRoon::as.data.table(feat), tp_pks = tp_pks)

  names(ftindex) <- groups_temp$id

  ftindex <- rbindlist(ftindex)
  ftindex <- as.data.table(t(ftindex))
  colnames(ftindex) <- new_id
  rownames(ftindex) <- seq_len(nrow(ftindex))

  # TODO adapt for as.featuresSet when multiple polarities present

  pat_fg <- new("featureGroupsOpenMS",
                groups = groups,
                analysisInfo = anaInfo,
                groupInfo = groupInfo,
                features = feat,
                ftindex = ftindex)

  return(pat_fg)
})
