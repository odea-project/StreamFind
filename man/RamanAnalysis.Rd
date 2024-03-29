% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_S3_RamanAnalysis.R
\name{RamanAnalysis}
\alias{RamanAnalysis}
\alias{validate.RamanAnalysis}
\alias{print.RamanAnalysis}
\alias{asJSON.RamanAnalysis}
\alias{as.RamanAnalysis}
\title{\strong{RamanAnalysis} S3 class constructor, methods and functions}
\usage{
RamanAnalysis(
  name = NA_character_,
  replicate = NA_character_,
  blank = NA_character_,
  file = NA_character_,
  type = NA_character_,
  metadata = list(),
  spectra = data.table()
)

\method{validate}{RamanAnalysis}(x = NULL)

\method{print}{RamanAnalysis}(x, ...)

\method{asJSON}{RamanAnalysis}(x)

as.RamanAnalysis(value)
}
\arguments{
\item{name}{\emph{asc} file name without extension.}

\item{replicate}{Character with length one, representing the analysis replicate group name.}

\item{blank}{Character with length one, representing the associated blank replicate group name.}

\item{file}{\emph{asc} full file path (with extension).}

\item{type}{}

\item{metadata}{List of analysis metadata taken from the \emph{asc} file or added.}

\item{spectra}{data.table with the raw spectra.}

\item{x}{A \emph{RamanAnalysis} S3 class object.}

\item{...}{Not used.}

\item{value}{A list to be checked and/or converted to \emph{RamanAnalysis} S3 class.}
}
\value{
An \emph{RamanAnalysis} S3 class object.
}
\description{
Creates a \emph{RamanAnalysis} S3 class object.
}
\section{Methods (by generic)}{
\itemize{
\item \code{validate(RamanAnalysis)}: S3 method to validate a \emph{RamanAnalysis} S3 class object, returning a logical value of length one.

\item \code{print(RamanAnalysis)}: S3 method to print the \emph{RamanAnalysis} S3 class object in the console.

\item \code{asJSON(RamanAnalysis)}: S3 method to converts a \emph{RamanAnalysis} S3 class object into a JSON string.

}}
\section{Functions}{
\itemize{
\item \code{as.RamanAnalysis()}: S3 method to convert the argument value in a \emph{RamanAnalysis} S3 class object.

}}
