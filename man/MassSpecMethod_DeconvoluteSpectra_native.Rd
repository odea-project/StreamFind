% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_MassSpecMethod_DeconvoluteSpectra.R
\name{MassSpecMethod_DeconvoluteSpectra_native}
\alias{MassSpecMethod_DeconvoluteSpectra_native}
\title{MassSpecMethod_DeconvoluteSpectra_native S7 class}
\usage{
MassSpecMethod_DeconvoluteSpectra_native(clustVal = 0.1, window = 20)
}
\arguments{
\item{clustVal}{Numeric (length 1) with the clustering value for the charge deconvolution.}

\item{window}{Optional numeric (length 1) with the window in \emph{m/z} for collecting traces of
a given charge.}
}
\value{
A MassSpecMethod_DeconvoluteSpectra_native object.
}
\description{
Deconvolutes the spectral mass-to-charge ratio (\emph{m/z}) to mass (Da) after
assignment of charges.
}
