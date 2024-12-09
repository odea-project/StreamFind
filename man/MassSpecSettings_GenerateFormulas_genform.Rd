% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecSettings_GenerateFormulas.R
\name{MassSpecSettings_GenerateFormulas_genform}
\alias{MassSpecSettings_GenerateFormulas_genform}
\title{\strong{MassSpecSettings_GenerateFormulas_genform}}
\usage{
MassSpecSettings_GenerateFormulas_genform(
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
  batchSize = 8
)
}
\arguments{
\item{relMzDev}{Numeric (length 1) with the relative mass deviation, in ppm.}

\item{elements}{Character vector with the elements to use for formulae annotation. Always try to work with a minimal
set by excluding elements you don't expect.}

\item{hetero}{Logical (length 1) indicating if heteroatoms are allowed in the formulae.}

\item{oc}{Logical (length 1) indicating presence of at least one carbon in the formulae.}

\item{thrMS}{Numeric (length 1) Sets the thresholds for the GenForm MS score (isoScore). Sets the thms command line
options, respectively. Set to NULL for no threshold.}

\item{thrMSMS}{Numeric (length 1) Sets the thresholds for the GenForm MS/MS score (MSMSScore). Sets the thmsms
command line options, respectively. Set to NULL for no threshold.}

\item{thrComb}{Numeric (length 1) Sets the thresholds for the GenForm combined score (combMatch). Sets the thcomb
command line options, respectively. Set to NULL for no threshold.}

\item{maxCandidates}{Numeric (length 1) with the maximum number of candidates to be generated.}

\item{extraOpts}{Character (length 1) with extra CLI options to be passed to the GenForm algorithm.}

\item{calculateFeatures}{Logical (length 1) indicating if features should be calculated.}

\item{featThreshold}{Numeric (length 1). If \code{calculateFeatures} is TRUE the minimum presence (from 0 to 1) of features
with formula annotation to be considered for the respective feature group.}

\item{featThresholdAnn}{Numeric (length 1). As \code{featThreshold}, but only considers features with annotations.}

\item{absAlignMzDev}{Numeric (length 1). When the group formula annotation consensus is made from feature annotations,
the \emph{m/z} values of annotated MS/MS fragments may slightly deviate from those of the corresponding group MS/MS
peak list. The \code{absAlignMzDev} argument specifies the maximum \emph{m/z} window used to re-align the mass peaks.}

\item{MSMode}{Character (length 1) with the MS mode to be used. Possible values are "MS", "MSMS", or "both".}

\item{isolatePrec}{Settings used for isolation of precursor mass peaks and their isotopes. This isolation is highly
important for accurate isotope scoring of candidates, as non-relevant mass peaks will dramatically decrease the score.
The value of \code{isolatePrec} should either be a \code{list} with parameters (see the \code{filter} method for \code{MSPeakLists} for
more details), \code{TRUE} for default parameters or \code{FALSE} for no isolation (e.g. when you already performed isolation
with the filter method). The \code{z} parameter (charge) is automatically deduced from the adduct used for annotation
(unless \code{isolatePrec} is FALSE), hence any custom \code{z} setting is ignored.}

\item{timeout}{Numeric (length 1) with the maximum time in seconds to wait for the GenForm algorithm to finish.}

\item{topMost}{Numeric (length 1) with the maximum number of top candidates to be returned.}

\item{batchSize}{Maximum number of \code{GenForm} commands that should be run sequentially in each parallel process.}
}
\value{
A \code{MassSpecSettings_GenerateFormulas_genform} object.
}
\description{
Settings for generating formulas using the algorithm \href{https://sourceforge.net/projects/genform/}{GenForm}.
The algorithm is used via the function \link[patRoon]{generateFormulas} from the package \pkg{patRoon}. Therefore,
it is highly recommended to check the original documentation of the function in \pkg{patRoon} for more details.
}
\references{
\insertRef{patroon01}{StreamFind}

\insertRef{patroon02}{StreamFind}

\insertRef{genform}{StreamFind}
}
