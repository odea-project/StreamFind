% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_GroupFeatures.R
\name{MassSpecMethod_GroupFeatures_xcms3_peakdensity}
\alias{MassSpecMethod_GroupFeatures_xcms3_peakdensity}
\title{MassSpecMethod_GroupFeatures_xcms3_peakdensity S7 class}
\usage{
MassSpecMethod_GroupFeatures_xcms3_peakdensity(
  bw = 5,
  minFraction = 1,
  minSamples = 1,
  binSize = 0.008,
  maxFeatures = 100
)
}
\arguments{
\item{bw}{numeric(1) defining the bandwidth (standard deviation of the smoothing kernel) to be used. This argument
is passed to the \code{density()} method.}

\item{minFraction}{numeric(1) defining the minimum fraction of analyses in at least one analysis replicate group in
which the features have to be present to be considered as a feature group.}

\item{minSamples}{numeric(1) with the minimum number of analyses in at least one analysis replicate group in which
the features have to be detected to be considered a feature group.}

\item{binSize}{numeric(1) defining the size of the overlapping slices in mz dimension.}

\item{maxFeatures}{numeric(1) with the maximum number of feature groups to be identified in a single mz slice.}
}
\value{
A \code{MassSpecMethod_GroupFeatures_xcms3_peakdensity} object.
}
\description{
Settings for grouping features (i.e., chromatographic peaks) across mzML/mzXML files using the package
\href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms} (version 3) with the algorithm
\href{https://rdrr.io/bioc/xcms/man/groupChromPeaks-density.html}{peakDensity}.
The function uses the package \pkg{patRoon} in the background.
}
\details{
See the \link[patRoon]{groupFeaturesXCMS3} function from the \pkg{patRoon} package for more information and requirements.
}
\references{
\insertRef{patroon01}{StreamFind}

\insertRef{patroon02}{StreamFind}

\insertRef{xcms01}{StreamFind}

\insertRef{xcms02}{StreamFind}

\insertRef{xcms03}{StreamFind}
}
