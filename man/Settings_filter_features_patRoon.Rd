% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_ProcessingSettings.R
\name{Settings_filter_features_patRoon}
\alias{Settings_filter_features_patRoon}
\title{Settings_filter_features_patRoon}
\usage{
Settings_filter_features_patRoon(
  absMinIntensity = NULL,
  relMinIntensity = NULL,
  preAbsMinIntensity = NULL,
  preRelMinIntensity = NULL,
  absMinAnalyses = NULL,
  relMinAnalyses = NULL,
  absMinReplicates = NULL,
  relMinReplicates = NULL,
  absMinFeatures = NULL,
  relMinFeatures = NULL,
  absMinReplicateAbundance = NULL,
  relMinReplicateAbundance = NULL,
  absMinConc = NULL,
  relMinConc = NULL,
  absMaxTox = NULL,
  relMaxTox = NULL,
  absMinConcTox = NULL,
  relMinConcTox = NULL,
  maxReplicateIntRSD = NULL,
  blankThreshold = NULL,
  retentionRange = NULL,
  mzRange = NULL,
  mzDefectRange = NULL,
  chromWidthRange = NULL,
  featQualityRange = NULL,
  groupQualityRange = NULL,
  rGroups = NULL,
  results = NULL,
  removeBlanks = FALSE,
  removeISTDs = FALSE,
  checkFeaturesSession = NULL,
  removeNA = FALSE,
  negate = FALSE
)
}
\arguments{
\item{absMinIntensity}{Numeric length one. Minimum absolute intensity for a feature.}

\item{relMinIntensity}{Numeric length one. Minimum relative intensity for a feature.}

\item{preAbsMinIntensity}{Numeric length one. Minimum absolute intensity for a feature before grouping.}

\item{preRelMinIntensity}{Numeric length one. Minimum relative intensity for a feature before grouping.}

\item{absMinAnalyses}{Numeric length one. Minimum number of analyses a feature must be present in.}

\item{relMinAnalyses}{Numeric length one. Minimum relative number of analyses a feature must be present in.}

\item{absMinReplicates}{Numeric length one. Minimum number of replicates a feature must be present in.}

\item{relMinReplicates}{Numeric length one. Minimum relative number of replicates a feature must be present in.}

\item{absMinFeatures}{Numeric length one. Minimum number of features a feature group must contain.}

\item{relMinFeatures}{Numeric length one. Minimum relative number of features a feature group must contain.}

\item{absMinReplicateAbundance}{Numeric length one. Minimum absolute abundance of a replicate.}

\item{relMinReplicateAbundance}{Numeric length one. Minimum relative abundance of a replicate.}

\item{absMinConc}{Numeric length one. Minimum absolute concentration of a feature.}

\item{relMinConc}{Numeric length one. Minimum relative concentration of a feature.}

\item{absMaxTox}{Numeric length one. Maximum absolute toxicity of a feature.}

\item{relMaxTox}{Numeric length one. Maximum relative toxicity of a feature.}

\item{absMinConcTox}{Numeric length one. Minimum absolute concentration of a feature to be considered toxic.}

\item{relMinConcTox}{Numeric length one. Minimum relative concentration of a feature to be considered toxic.}

\item{maxReplicateIntRSD}{Numeric length one. Maximum relative standard deviation of intensities within a replicate.}

\item{blankThreshold}{Numeric length one. Maximum intensity of a feature to be considered a blank.}

\item{retentionRange}{Numeric length two. Retention time range (in seconds) for a feature.}

\item{mzRange}{Numeric length two. m/z range (in Da) for a feature.}

\item{mzDefectRange}{Numeric length two. m/z defect range (in Da) for a feature.}

\item{chromWidthRange}{Numeric length two. Chromatographic width range (in seconds) for a feature.}

\item{featQualityRange}{Numeric length two. Feature quality range for a feature.}

\item{groupQualityRange}{Numeric length two. Group quality range for a feature group.}

\item{rGroups}{List of replicate groups.}

\item{results}{Only keep feature groups that have results in the object specified by results. See
\code{\link[patRoon]{replicateGroupSubtract}} for further information.}

\item{removeBlanks}{Logical length one. Remove blank samples.}

\item{removeISTDs}{Logical length one. Remove internal standards.}

\item{checkFeaturesSession}{Check features session.}

\item{removeNA}{Logical length one. Remove NA values.}

\item{negate}{Logical length one. Negate the filter.}
}
\value{
A ProcessingSettings S3 class object with subclass Settings_filter_features_patRoon.
}
\description{
Settings for filtering of features and feature groups. A full description of the filtering parameters is
in \code{\link[patRoon]{replicateGroupSubtract}} from patRoon package.
}
\details{
Note that when filters are applied to features or feature groups
these require specific results from processing modules. For instance,
subtracting the blank can only be done after grouping features. Also, some
filters require. Thus, not all filters can be applied to features.
See \code{\link[patRoon]{features-class}} and \code{\link[patRoon]{replicateGroupSubtract}}
for further information.
}
