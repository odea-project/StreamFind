% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_FindFeatures.R
\name{MassSpecMethod_FindFeatures_xcms3_centwave}
\alias{MassSpecMethod_FindFeatures_xcms3_centwave}
\title{MassSpecMethod_FindFeatures_xcms3_centwave S7 class}
\usage{
MassSpecMethod_FindFeatures_xcms3_centwave(
  ppm = 12,
  peakwidth = c(5, 60),
  snthresh = 15,
  prefilter = c(5, 1500),
  mzCenterFun = "wMean",
  integrate = 1,
  mzdiff = -2e-04,
  fitgauss = TRUE,
  noise = 500,
  verboseColumns = TRUE,
  firstBaselineCheck = FALSE,
  extendLengthMSW = FALSE
)
}
\arguments{
\item{ppm}{numeric(1) defining the maximal tolerated m/z deviation in consecutive scans in parts
per million (ppm) for the initial ROI definition.}

\item{peakwidth}{numeric(2) with the expected approximate feature width in chromatographic space.
Given as a range (min, max) in seconds.}

\item{snthresh}{numeric(1) defining the signal to noise ratio cutoff.}

\item{prefilter}{numeric(2): c(k, I) specifying the prefilter step for the first analysis step
(ROI detection). Mass traces are only retained if they contain at least k peaks with
intensity >= I.}

\item{mzCenterFun}{Name of the function to calculate the m/z center of the
chromatographic peak (feature). Allowed are: "wMean": intensity weighted mean
of the peak's m/z values, "mean": mean of the peak's m/z values, "apex": use the
m/z value at the peak apex, "wMeanApex3": intensity weighted mean of the m/z
value at the peak apex and the m/z values left and right of it and
"meanApex3": mean of the m/z value of the peak apex and the m/z values
left and right of it.}

\item{integrate}{Integration method. For integrate = 1 peak limits are found
through descent on the mexican hat filtered data, for integrate = 2 the
descent is done on the real data. The latter method is more accurate but
prone to noise, while the former is more robust, but less exact.}

\item{mzdiff}{numeric(1) representing the minimum difference in m/z dimension
required for peaks with overlapping retention times; can be negative to
allow overlap. During peak post-processing, peaks defined to be overlapping
are reduced to the one peak with the largest signal.}

\item{fitgauss}{logical(1) whether or not a Gaussian should be fitted to each
peak. This affects mostly the retention time position of the peak.}

\item{noise}{numeric(1) allowing to set a minimum intensity required for
centroids to be considered in the first analysis step (centroids with
intensity < noise are omitted from ROI detection).}

\item{verboseColumns}{logical(1) whether additional peak meta data columns
should be returned.}

\item{firstBaselineCheck}{logical(1). If TRUE continuous data within regions
of interest is checked to be above the first baseline.}

\item{extendLengthMSW}{Option to force centWave to use all scales when
running centWave rather than truncating with the EIC length. Uses the
"open" method to extend the EIC to a integer base-2 length prior to being
passed to convolve rather than the default "reflect" method.
See https://github.com/sneumann/xcms/issues/445 for more information.}
}
\value{
A \code{MassSpecMethod_FindFeatures_xcms3_centwave} object.
}
\description{
Method for finding features (i.e., chromatographic peaks) in mass spectrometry files
using the package \href{https://bioconductor.org/packages/release/bioc/html/xcms.html}{xcms}
(version 3) with the algorithm
\href{https://rdrr.io/bioc/xcms/man/findChromPeaks-centWave.html}{centWave}. The function uses
the package \pkg{patRoon} in the background.
}
\details{
See the \link[patRoon]{findFeaturesXCMS3} function from the \pkg{patRoon} package for
more information and requirements.
}
\references{
\insertRef{patroon01}{StreamFind}

\insertRef{patroon02}{StreamFind}

\insertRef{xcms01}{StreamFind}

\insertRef{xcms02}{StreamFind}

\insertRef{xcms03}{StreamFind}
}
