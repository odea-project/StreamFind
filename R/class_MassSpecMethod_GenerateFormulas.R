#' MassSpecMethod_GenerateFormulas_genform S7 class
#'
#' @description Settings for generating formulas using the algorithm \href{https://sourceforge.net/projects/genform/}{GenForm}.
#' The algorithm is used via the function \link[patRoon]{generateFormulas} from the package \pkg{patRoon}. Therefore,
#' it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
#' 
#' @param MSPeakListsClusterMzWindow m/z window (in Da) used for clustering m/z values when spectra are averaged.
#' For method="hclust" this corresponds to the cluster height, while for method="distance" this value is used to find
#' nearby masses (+/- window). Too small windows will prevent clustering m/z values (thus erroneously treating equal 
#' masses along spectra as different), whereas too big windows may cluster unrelated m/z values from different or even
#' the same spectrum together.
#' @param MSPeakListsTopMost Only retain this maximum number of MS peaks when generating averaged spectra. Lowering
#' this number may exclude more irrelevant (noisy) MS peaks and decrease processing time, whereas higher values may
#' avoid excluding lower intense MS peaks that may still be of interest.
#' @param MSPeakListsMinIntensityPre MS peaks with intensities below this value will be removed (applied prior to
#' selection by `topMost`) before averaging.
#' @param MSPeakListsMinIntensityPost MS peaks with intensities below this value will be removed after averaging.
#' @param MSPeakListsAvgFun Character with the function name that is used to calculate average m/z values.
#' @param MSPeakListsMethod Method used for producing averaged MS spectra. Valid values are "hclust", used for
#' hierarchical clustering (using the fastcluster package), and "distance", to use the between peak distance.
#' The latter method may reduces processing time and memory requirements, at the potential cost of reduced accuracy.
#' @param relMzDev Numeric (length 1) with the relative mass deviation, in ppm.
#' @param elements Character vector with the elements to use for formulae annotation. Always try to work with a minimal
#' set by excluding elements you don't expect.
#' @param hetero Logical (length 1) indicating if heteroatoms are allowed in the formulae.
#' @param oc Logical (length 1) indicating presence of at least one carbon in the formulae.
#' @param thrMS Numeric (length 1) Sets the thresholds for the GenForm MS score (isoScore). Sets the thms command line
#' options, respectively. Set to NULL for no threshold.
#' @param thrMSMS Numeric (length 1) Sets the thresholds for the GenForm MS/MS score (MSMSScore). Sets the thmsms
#' command line options, respectively. Set to NULL for no threshold.
#' @param thrComb Numeric (length 1) Sets the thresholds for the GenForm combined score (combMatch). Sets the thcomb
#' command line options, respectively. Set to NULL for no threshold.
#' @param maxCandidates Numeric (length 1) with the maximum number of candidates to be generated.
#' @param extraOpts Character (length 1) with extra CLI options to be passed to the GenForm algorithm.
#' @param calculateFeatures Logical (length 1) indicating if features should be calculated.
#' @param featThreshold Numeric (length 1). If `calculateFeatures` is TRUE the minimum presence (from 0 to 1) of features
#' with formula annotation to be considered for the respective feature group.
#' @param featThresholdAnn Numeric (length 1). As `featThreshold`, but only considers features with annotations.
#' @param absAlignMzDev Numeric (length 1). When the group formula annotation consensus is made from feature annotations,
#' the \emph{m/z} values of annotated MS/MS fragments may slightly deviate from those of the corresponding group MS/MS
#' peak list. The `absAlignMzDev` argument specifies the maximum \emph{m/z} window used to re-align the mass peaks.
#' @param MSMode Character (length 1) with the MS mode to be used. Possible values are "MS", "MSMS", or "both".
#' @param isolatePrec Settings used for isolation of precursor mass peaks and their isotopes. This isolation is highly
#' important for accurate isotope scoring of candidates, as non-relevant mass peaks will dramatically decrease the score.
#' The value of `isolatePrec` should either be a `list` with parameters (see the `filter` method for `MSPeakLists` for
#' more details), `TRUE` for default parameters or `FALSE` for no isolation (e.g. when you already performed isolation
#' with the filter method). The `z` parameter (charge) is automatically deduced from the adduct used for annotation
#' (unless `isolatePrec` is FALSE), hence any custom `z` setting is ignored.
#' @param timeout Numeric (length 1) with the maximum time in seconds to wait for the GenForm algorithm to finish.
#' @param topMost Numeric (length 1) with the maximum number of top candidates to be returned.
#' @param batchSize Maximum number of `GenForm` commands that should be run sequentially in each parallel process.
#'
#' @return A `MassSpecMethod_GenerateFormulas_genform` object.
#'
#' @references
#' \insertRef{patroon01}{StreamFind}
#'
#' \insertRef{patroon02}{StreamFind}
#'
#' \insertRef{genform}{StreamFind}
#'
#' @export
#'
MassSpecMethod_GenerateFormulas_genform <- S7::new_class(
  name = "MassSpecMethod_GenerateFormulas_genform",
  parent = ProcessingStep,
  package = "StreamFind",
  constructor = function(MSPeakListsClusterMzWindow = 0.005,
                         MSPeakListsTopMost = 100,
                         MSPeakListsMinIntensityPre = 50,
                         MSPeakListsMinIntensityPost = 50,
                         MSPeakListsAvgFun = "mean",
                         MSPeakListsMethod = "distance",
                         relMzDev = 5,
                         elements = "CHNOP",
                         hetero = TRUE,
                         oc = FALSE,
                         thrMS = numeric(),
                         thrMSMS = numeric(),
                         thrComb = numeric(),
                         maxCandidates = Inf,
                         extraOpts = character(),
                         calculateFeatures = TRUE,
                         featThreshold = 0,
                         featThresholdAnn = 0.75,
                         absAlignMzDev = 0.002,
                         MSMode = "both",
                         isolatePrec = TRUE,
                         timeout = 120,
                         topMost = 50,
                         batchSize = 8) {
    S7::new_object(
      ProcessingStep(
        data_type = "MassSpec",
        method = "GenerateFormulas",
        required = c("FindFeatures", "GroupFeatures", "LoadFeaturesMS1", "LoadFeaturesMS2"),
        algorithm = "genform",
        parameters = list(
          MSPeakListsClusterMzWindow = MSPeakListsClusterMzWindow,
          MSPeakListsTopMost = MSPeakListsTopMost,
          MSPeakListsMinIntensityPre = MSPeakListsMinIntensityPre,
          MSPeakListsMinIntensityPost = MSPeakListsMinIntensityPost,
          MSPeakListsAvgFun = MSPeakListsAvgFun,
          MSPeakListsMethod = MSPeakListsMethod,
          relMzDev = as.numeric(relMzDev),
          elements = as.character(elements),
          hetero = as.logical(hetero),
          oc = as.logical(oc),
          thrMS = as.numeric(thrMS),
          thrMSMS = as.numeric(thrMSMS),
          thrComb = as.numeric(thrComb),
          maxCandidates = as.numeric(maxCandidates),
          extraOpts = as.character(extraOpts),
          calculateFeatures = as.logical(calculateFeatures),
          featThreshold = as.numeric(featThreshold),
          featThresholdAnn = as.numeric(featThresholdAnn),
          absAlignMzDev = as.numeric(absAlignMzDev),
          MSMode = as.character(MSMode),
          isolatePrec = as.logical(isolatePrec),
          timeout = as.numeric(timeout),
          topMost = as.integer(topMost),
          batchSize = as.integer(batchSize)
        ),
        number_permitted = 1,
        version = as.character(packageVersion("StreamFind")),
        software = "GenForm",
        developer = "Markus Meringer",
        contact = "Markus.Meringer@Uni-Bayreuth.De",
        link = "https://sourceforge.net/projects/genform/",
        doi = "MATCH Commun. Math. Comput. Chem 65.2 (2011): 259-290."
      )
    )
  },
  validator = function(self) {
    checkmate::assert_choice(self@data_type, "MassSpec")
    checkmate::assert_choice(self@method, "GenerateFormulas")
    checkmate::assert_choice(self@algorithm, "genform")
    checkmate::assert_numeric(self@parameters$MSPeakListsClusterMzWindow, len = 1)
    checkmate::assert_numeric(self@parameters$MSPeakListsTopMost, len = 1)
    checkmate::assert_numeric(self@parameters$MSPeakListsMinIntensityPre, len = 1)
    checkmate::assert_numeric(self@parameters$MSPeakListsMinIntensityPost, len = 1)
    checkmate::assert_character(self@parameters$MSPeakListsAvgFun)
    checkmate::assert_choice(self@parameters$MSPeakListsMethod, c("hclust", "distance"))
    checkmate::assert_number(self@parameters$relMzDev)
    checkmate::assert_character(self@parameters$elements, min.len = 1)
    checkmate::assert_logical(self@parameters$hetero, len = 1)
    checkmate::assert_logical(self@parameters$oc, len = 1)
    checkmate::assert_numeric(self@parameters$thrMS)
    checkmate::assert_numeric(self@parameters$thrMSMS)
    checkmate::assert_numeric(self@parameters$thrComb)
    checkmate::assert_numeric(self@parameters$maxCandidates, len = 1)
    checkmate::assert_character(self@parameters$extraOpts)
    checkmate::assert_logical(self@parameters$calculateFeatures, len = 1)
    checkmate::assert_numeric(self@parameters$featThreshold)
    checkmate::assert_numeric(self@parameters$featThresholdAnn)
    checkmate::assert_numeric(self@parameters$absAlignMzDev)
    checkmate::assert_choice(self@parameters$MSMode, c("MS", "MSMS", "both"))
    checkmate::assert_logical(self@parameters$isolatePrec, len = 1)
    checkmate::assert_numeric(self@parameters$timeout, len = 1)
    checkmate::assert_integer(self@parameters$topMost, len = 1)
    checkmate::assert_integer(self@parameters$batchSize, len = 1)
    NULL
  }
)

#' @export
#' @noRd
S7::method(run, MassSpecMethod_GenerateFormulas_genform) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_results_nts()) {
    warning("No NonTargetAnalysisResults object available! Not done.")
    return(FALSE)
  }

  NonTargetAnalysisResults <- engine$NonTargetAnalysisResults

  if (!NonTargetAnalysisResults@has_groups) {
    warning("NonTargetAnalysisResults object does not have feature groups! Not done.")
    return(FALSE)
  }

  parameters <- x$parameters

  parameters <- lapply(parameters, function(z) {
    if (length(z) == 0) {
      return(NULL)
    }
    z
  })
  
  algorithm <- x$algorithm
  
  fg <- get_patRoon_features(
    NonTargetAnalysisResults,
    filtered = FALSE,
    featureGroups = TRUE
  )
  
  mspl <- get_patRoon_MSPeakLists(
    NonTargetAnalysisResults,
    MSPeakListsClusterMzWindow,
    MSPeakListsTopMost,
    MSPeakListsMinIntensityPre,
    MSPeakListsMinIntensityPost,
    MSPeakListsAvgFun,
    MSPeakListsMethod,
  )

  if (length(mspl) == 0) {
    warning("MSPeakLists empty! Use the load_MSPeakLists to load MS1 and MS2 data. Not done.")
    return(FALSE)
  }

  if ("featureGroupsSet" %in% is(fg)) {
    parameters$adduct <- NULL
  } else {
    pol <- unique(unname(engine$get_spectra_polarity()))

    if ("positive" %in% pol) parameters$adduct <- "[M+H]+"

    if ("negative" %in% pol) parameters$adduct <- "[M-H]-"
  }

  ag <- list(fGroups = fg, MSPeakLists = mspl, algorithm = algorithm)

  pp_fun <- patRoon::generateFormulas

  formulas <- do.call(pp_fun, c(ag, parameters))

  if (length(formulas) == 0) {
    warning("No formulas generated!")
    return(FALSE)
  }

  feature_list <- NonTargetAnalysisResults$feature_list

  formulas_col <- lapply(names(feature_list), function(x, feature_list, formulas) {
    formula <- formulas@featureFormulas[[x]]
    if (!is.null(formula)) {
      fts <- feature_list[[x]]

      if (nrow(fts) > 0) {
        res <- lapply(fts$group, function(z, formula) {
          if (!is.na(z)) {
            return(formula[[z]])
          }
          list(NULL)
        }, formula = formula)
        return(res)
      }
    }
    rep(list(NULL), nrow(feature_list[[x]]))
  }, feature_list = feature_list, formulas = formulas)

  NonTargetAnalysisResults <- .add_features_column(NonTargetAnalysisResults, "formulas", formulas_col)

  NonTargetAnalysisResults$formulas <- formulas

  engine$NonTargetAnalysisResults <- NonTargetAnalysisResults

  message(paste0("\U2713 ", length(formulas), " formulas generated and added!"))

  TRUE
}
