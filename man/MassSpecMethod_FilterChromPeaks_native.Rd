% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_FilterChromPeaks.R
\name{MassSpecMethod_FilterChromPeaks_native}
\alias{MassSpecMethod_FilterChromPeaks_native}
\title{MassSpecMethod_FilterChromPeaks_native S7 class}
\usage{
MassSpecMethod_FilterChromPeaks_native(
  minIntensity = 0,
  retentionTimeRange = c(NA_real_, NA_real_)
)
}
\arguments{
\item{minIntensity}{Numeric (length 1) with the minimum intensity to keep a chromatographic peak.}

\item{retentionTimeRange}{Numeric (length 2) with the retention time range to keep
chromatographic peaks. The first element is the minimum retention time and the second element
is the maximum retention time, in seconds.}
}
\value{
A MassSpecMethod_FilterChromPeaks_native object.
}
\description{
Settings for filtering chromatographic peaks.
}
