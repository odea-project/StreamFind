% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_LoadChromatograms.R
\name{MassSpecMethod_LoadChromatograms_native}
\alias{MassSpecMethod_LoadChromatograms_native}
\title{MassSpecMethod_LoadChromatograms_native}
\usage{
MassSpecMethod_LoadChromatograms_native(
  chromatograms = 1,
  rtmin = 0,
  rtmax = 0,
  minIntensity = 0
)
}
\arguments{
\item{chromatograms}{A numeric or character vector with the chromatogram indices or names to be
used, respectively.}

\item{rtmin}{A numeric vector with the minimum retention time values to be used.}

\item{rtmax}{A numeric vector with the maximum retention time values to be used.}

\item{minIntensity}{A numeric value with the minimum intensity to be used.}
}
\value{
A \code{MassSpecMethod_LoadChromatograms_native} object.
}
\description{
Loads chromatograms from mass spectrometry analyses.
}
