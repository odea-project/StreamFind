% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_ProcessingSettings.R
\name{Settings_prepare_classification_knn}
\alias{Settings_prepare_classification_knn}
\title{Settings_prepare_classification_knn}
\usage{
Settings_prepare_classification_knn(k = 3, l = 0)
}
\arguments{
\item{k}{Integer (length 1) with the number of neighbors to be used.}

\item{l}{Integer (length 1) with the minimum vote for definite decision, otherwise doubt.
(More precisely, less than k-l dissenting votes are allowed, even if k is increased by ties.)}
}
\value{
A ProcessingSettings S3 class object with subclass Settings_prepare_classification_knn.
}
\description{
Prepares a classification model using the k-nearest neighbors (knn) algorithm from package \pkg{class}.
}
\references{
\insertRef{class01}{StreamFind}
}
