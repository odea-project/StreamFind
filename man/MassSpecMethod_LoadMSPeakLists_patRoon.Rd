% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_LoadMSPeakLists.R
\name{MassSpecMethod_LoadMSPeakLists_patRoon}
\alias{MassSpecMethod_LoadMSPeakLists_patRoon}
\title{MassSpecMethod_LoadMSPeakLists_patRoon S7 class}
\usage{
MassSpecMethod_LoadMSPeakLists_patRoon(
  maxMSRtWindow = 5,
  precursorMzWindow = 4,
  clusterMzWindow = 0.005,
  topMost = 100,
  minIntensityPre = 50,
  minIntensityPost = 50,
  avgFun = "mean",
  method = "hclust",
  retainPrecursorMSMS = TRUE
)
}
\arguments{
\item{maxMSRtWindow}{Maximum chromatographic peak window used for spectrum
averaging (in seconds, +/- retention time). If NULL all spectra from a feature
will be taken into account. Lower to decrease processing time.}

\item{precursorMzWindow}{The m/z window (in Da) to find MS/MS spectra of a precursor.
This is typically used for Data-Dependent like MS/MS data and should correspond to the
isolation m/z window (i.e. +/- the precursor m/z) that was used to collect the data.
For Data-Independent MS/MS experiments, where precursor ions are not isolated prior to
fragmentation (e.g. bbCID, MSe, all-ion, ...) the value should be NULL.}

\item{clusterMzWindow}{m/z window (in Da) used for clustering m/z values
when spectra are averaged. For method="hclust" this corresponds to the
cluster height, while for method="distance" this value is used to find
nearby masses (+/- window). Too small windows will prevent clustering
m/z values (thus erroneously treating equal masses along spectra as
different), whereas too big windows may cluster unrelated m/z values
from different or even the same spectrum together.}

\item{topMost}{Only retain this maximum number of MS peaks when generating
averaged spectra. Lowering this number may exclude more irrelevant (noisy)
MS peaks and decrease processing time, whereas higher values may avoid
excluding lower intense MS peaks that may still be of interest.}

\item{minIntensityPre}{MS peaks with intensities below this value will
be removed (applied prior to selection by \code{topMost}) before averaging.}

\item{minIntensityPost}{MS peaks with intensities below this value will
be removed after averaging.}

\item{avgFun}{Function that is used to calculate average m/z values.}

\item{method}{Method used for producing averaged MS spectra. Valid
values are "hclust", used for hierarchical clustering (using the
fastcluster package), and "distance", to use the between peak distance.
The latter method may reduces processing time and memory requirements,
at the potential cost of reduced accuracy.}

\item{retainPrecursorMSMS}{For MS/MS data only: if TRUE then always
retain the precursor mass peak even if is not among the \code{topMost} peaks.
Note that MS precursor mass peaks are always kept. Furthermore, note
that precursor peaks in both MS and MS/MS data may still be removed by
intensity thresholds (this is unlike the filter method function).}
}
\value{
A \code{MassSpecMethod_LoadMSPeakLists_patRoon} object.
}
\description{
Settings for loading MS2 and MS1 spectra for feature groups.
}
