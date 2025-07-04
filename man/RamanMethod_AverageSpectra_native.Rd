% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_RamanMethod_AverageSpectra.R
\name{RamanMethod_AverageSpectra_native}
\alias{RamanMethod_AverageSpectra_native}
\title{Raman Method to Average Spectra (native algorithm)}
\usage{
RamanMethod_AverageSpectra_native(by = "replicates")
}
\arguments{
\item{by}{Character (length 1) with the grouping variable for averaging. Possible variables are
\code{replicates}, \code{chrom_peaks}, \code{rt}, \code{replicates+chrom_peaks}, \code{replicates+rt}, \code{chrom_peaks+rt},
\code{replicates+chrom_peaks+rt}.}
}
\value{
A RamanMethod_AverageSpectra_native object.
}
\description{
Averages spectra based on variables.
}
