% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_S3_MassSpecAnalysis.R
\name{MassSpecAnalysis}
\alias{MassSpecAnalysis}
\title{\strong{MassSpecAnalysis} S3 class constructor, methods and functions}
\usage{
MassSpecAnalysis(
  name = NA_character_,
  replicate = NA_character_,
  blank = NA_character_,
  file = NA_character_,
  format = NA_character_,
  type = NA_character_,
  spectra_number = NA_integer_,
  spectra_headers = data.table(),
  spectra = data.table(),
  chromatograms_number = NA_integer_,
  chromatograms_headers = data.table(),
  chromatograms = data.table(),
  metadata = list()
)
}
\arguments{
\item{name}{\emph{mzML} or \emph{mzXML} file name without extension.}

\item{replicate}{Character with length one, representing the analysis replicate group name.}

\item{blank}{Character with length one, representing the associated blank replicate group name.}

\item{file}{\emph{mzML} or \emph{mzXML} full file path (with extension).}

\item{format}{Character with length one. One of \emph{mzML} or \emph{mzXML}.}

\item{type}{Character with length one defining the type of analysis.}

\item{spectra_number}{Integer with the number of spectra in the file.}

\item{spectra_headers}{data.table run information for each spectrum.}

\item{spectra}{data.table with the raw spectra (only present if loaded).}

\item{chromatograms_number}{Integer with the number of chromatograms in the file.}

\item{chromatograms_headers}{data.table headers information for each chromatogram.}

\item{chromatograms}{data.table with the raw chromatograms (only present if loaded).}

\item{metadata}{List with flexible storage for experimental metadata (e.g., concentration, location, etc.).}
}
\value{
An \emph{MassSpecAnalysis} S3 class object.
}
\description{
Creates a \emph{MassSpecAnalysis} S3 class object.
}
