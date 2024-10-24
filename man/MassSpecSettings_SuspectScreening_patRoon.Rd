% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_S3_MassSpecSettings.R
\name{MassSpecSettings_SuspectScreening_patRoon}
\alias{MassSpecSettings_SuspectScreening_patRoon}
\title{MassSpecSettings_SuspectScreening_patRoon}
\usage{
MassSpecSettings_SuspectScreening_patRoon(
  suspects = NULL,
  rtWindow = 12,
  mzWindow = 0.005,
  filtered = FALSE
)
}
\arguments{
\item{suspects}{A data.frame with suspect information. See section Suspect list format in \link[patRoon]{screenSuspects} for more information.}

\item{rtWindow}{The retention time window (in seconds) that will be used for matching a suspect (+/- feature data).}

\item{mzWindow}{The m/z window that will be used for matching a suspect (+/- feature data)..}

\item{filtered}{Logical (length 1). When \code{TRUE}, filtered features/groups are considered.}
}
\value{
A ProcessingSettings S3 class object with subclass MassSpecSettings_SuspectScreening_patRoon.
}
\description{
Settings for performing suspect screening using the function \link[patRoon]{screenSuspects} from the patRoon R package.
}
\references{
\insertRef{patroon01}{StreamFind}

\insertRef{patroon02}{StreamFind}
}
