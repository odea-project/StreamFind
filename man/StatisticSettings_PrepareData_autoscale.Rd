% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_S3_StatisticSettings.R
\name{StatisticSettings_PrepareData_autoscale}
\alias{StatisticSettings_PrepareData_autoscale}
\title{StatisticSettings_PrepareData_autoscale}
\usage{
StatisticSettings_PrepareData_autoscale(center = TRUE, scale = TRUE)
}
\arguments{
\item{center}{Logical (length 1) indicating if the data should be centered.}

\item{scale}{Logical (length 1) indicating if the data should be scaled.}
}
\value{
A ProcessingSettings S3 class object with subclass StatisticSettings_PrepareData_autoscale.
}
\description{
Auto scale and centers data using the \code{prep.autoscale} function from the \pkg{mdatools} package.
}
