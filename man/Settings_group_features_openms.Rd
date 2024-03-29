% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_ms_default_ProcessingSettings.R
\name{Settings_group_features_openms}
\alias{Settings_group_features_openms}
\alias{validate.Settings_group_features_openms}
\title{Settings_group_features_openms}
\usage{
Settings_group_features_openms(
  rtalign = FALSE,
  QT = FALSE,
  maxAlignRT = 5,
  maxAlignMZ = 0.008,
  maxGroupRT = 5,
  maxGroupMZ = 0.008,
  extraOptsRT = NULL,
  extraOptsGroup = NULL,
  verbose = FALSE
)

\method{validate}{Settings_group_features_openms}(x)
}
\arguments{
\item{rtalign}{Logical length one. Set to TRUE to enable retention time
alignment.}

\item{QT}{Logical length one. When TRUE the FeatureLinkerUnlabeledQT is used
instead of FeatureLinkerUnlabeled for grouping features.}

\item{maxAlignRT}{Numeric length one. Maximum retention time (in seconds) for
feature pairing when performing retention time alignment.}

\item{maxAlignMZ}{Numeric length one. Maximum \emph{m/z} (in Da) for
feature pairing when performing retention time alignment.}

\item{maxGroupRT}{Numeric length one. Maximum retention time (in seconds) for
feature pairing when performing grouping.}

\item{maxGroupMZ}{Numeric length one. Maximum \emph{m/z} (in Da) for
feature pairing when performing grouping.}

\item{extraOptsRT}{Named list containing extra options that will be passed
to MapAlignerPoseClustering.}

\item{extraOptsGroup}{Named list containing extra options that will be passed
to FeatureLinkerUnlabeledQT/FeatureLinkerUnlabeled.}

\item{verbose}{Logical of length one. When TRUE adds processing information
to the console.}

\item{x}{A Settings_group_features_openms S3 class object.}
}
\value{
A ProcessingSettings S3 class object with subclass
Settings_group_features_openms.
}
\description{
Settings for grouping features (i.e., chromatographic peaks)
in mzML/mzXML files using the \href{https://www.openms.org/}{OpenMS}
(\url{https://abibuilder.cs.uni-tuebingen.de/archive/openms/}) software
with the algorithm
\href{https://abibuilder.cs.uni-tuebingen.de/archive/openms/Documentation/release/3.0.0/html/TOPP_FeatureLinkerUnlabeled.html}{FeatureLinkerUnlabeled}.
The function uses the package \pkg{patRoon} in the background.
}
\details{
See the \link[patRoon]{groupFeaturesOpenMS} function from the
\pkg{patRoon} package for more information and requirements.
}
\section{Methods (by generic)}{
\itemize{
\item \code{validate(Settings_group_features_openms)}: Validates the object structure, returning a logical value of length one.

}}
\references{
\insertRef{patroon01}{StreamFind}

\insertRef{patroon02}{StreamFind}

\insertRef{openms01}{StreamFind}
}
