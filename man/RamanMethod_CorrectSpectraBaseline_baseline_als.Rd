% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_RamanMethod_CorrectSpectraBaseline.R
\name{RamanMethod_CorrectSpectraBaseline_baseline_als}
\alias{RamanMethod_CorrectSpectraBaseline_baseline_als}
\title{\strong{RamanMethod_CorrectSpectraBaseline_baseline_als}}
\usage{
RamanMethod_CorrectSpectraBaseline_baseline_als(
  lambda = 5,
  p = 0.05,
  maxit = 10
)
}
\arguments{
\item{lambda}{Numeric (length 1) with the 2nd derivative constraint.}

\item{p}{Numeric (length 1) with the weighting of positive residuals.}

\item{maxit}{Integer (length 1) with the maximum number of iterations.}
}
\value{
A RamanMethod_CorrectSpectraBaseline_baseline_als object.
}
\description{
Performs baseline correction to spectra using the Asymmetric Least Squares (ALS) algorithm from the
\pkg{baseline} package.
}
