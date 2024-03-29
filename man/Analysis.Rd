% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_S3_Analysis.R
\name{Analysis}
\alias{Analysis}
\alias{validate.Analysis}
\alias{print.Analysis}
\alias{asJSON.Analysis}
\alias{as.Analysis}
\title{\emph{Analysis} S3 class constructor, methods and functions}
\usage{
Analysis(
  name = NA_character_,
  replicate = NA_character_,
  blank = NA_character_
)

\method{validate}{Analysis}(x = NULL)

\method{print}{Analysis}(x, ...)

\method{asJSON}{Analysis}(x)

as.Analysis(value)
}
\arguments{
\item{name}{Character of length one with the name of the analysis.}

\item{replicate}{Character of length one with the analysis replicate group name.}

\item{blank}{Character of length one with the associated blank replicate group name.}

\item{x}{An \emph{Analysis} S3 class object.}

\item{...}{Not used.}

\item{value}{A list to be checked and/or converted to \emph{Analysis} S3 class.}
}
\value{
An \emph{Analysis} S3 class object.
}
\description{
Creates an Analysis S3 class object.
}
\section{Methods (by generic)}{
\itemize{
\item \code{validate(Analysis)}: S3 method to validate an \emph{Analysis} S3 class object, returning a logical value of length one.

\item \code{print(Analysis)}: S3 method to print an \emph{Analysis} S3 class object in the console.

\item \code{asJSON(Analysis)}: S3 method to convert an \emph{Analysis} S3 class object to a JSON string.

}}
\section{Functions}{
\itemize{
\item \code{as.Analysis()}: S3 method to convert the argument value to an \emph{Analysis} S3 class object.

}}
