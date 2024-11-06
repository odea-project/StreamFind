# ______________________________________________________________________________________________________________________
# genform -----
# ______________________________________________________________________________________________________________________

#' **MassSpecSettings_GenerateFormulas_genform**
#'
#' @description Settings for generating formulas using the algorithm \href{https://sourceforge.net/projects/genform/}{GenForm}.
#' The algorithm is used via the function \link[patRoon]{generateFormulas} from the package \pkg{patRoon}. Therefore,
#' it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
#'
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
#' @return A `MassSpecSettings_GenerateFormulas_genform` object.
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
MassSpecSettings_GenerateFormulas_genform <- S7::new_class("MassSpecSettings_GenerateFormulas_genform",
  parent = ProcessingSettings,
  package = "StreamFind",
  constructor = function(relMzDev = 5,
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
    S7::new_object(ProcessingSettings(
      engine = "MassSpec",
      method = "GenerateFormulas",
      algorithm = "genform",
      parameters = list(
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
    ))
  },
  validator = function(self) {
    checkmate::assert_choice(self@engine, "MassSpec")
    checkmate::assert_choice(self@method, "GenerateFormulas")
    checkmate::assert_choice(self@algorithm, "genform")
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
S7::method(run, MassSpecSettings_GenerateFormulas_genform) <- function(x, engine = NULL) {
  if (!is(engine, "MassSpecEngine")) {
    warning("Engine is not a MassSpecEngine object!")
    return(FALSE)
  }

  if (!engine$has_analyses()) {
    warning("There are no analyses! Not done.")
    return(FALSE)
  }

  if (!engine$has_nts()) {
    warning("No NTS object available! Not done.")
    return(FALSE)
  }

  nts <- engine$nts

  if (!nts@has_groups) {
    warning("NTS object does not have feature groups! Not done.")
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

  fg <- nts$features

  mspl <- nts$mspl

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

  feature_list <- nts$feature_list

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

  nts <- .add_features_column(nts, "formulas", formulas_col)

  nts$formulas <- formulas

  engine$nts <- nts

  message(paste0("\U2713 ", length(formulas), " formulas generated and added!"))

  TRUE
}
