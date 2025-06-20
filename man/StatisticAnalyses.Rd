% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Analyses_StatisticAnalyses.R
\name{StatisticAnalyses}
\alias{StatisticAnalyses}
\title{Statistical Analyses (Tabular Data Input)}
\usage{
StatisticAnalyses(
  analyses = NULL,
  classes = character(),
  concentrations = numeric(),
  ...
)
}
\arguments{
\item{analyses}{A \code{character} vector with full file path to \code{.csv} file with variable names as
first row and analyses names as first column or a \code{data.frame} or \code{matrix} object.}

\item{classes}{A \code{character} vector with the classes of the analyses.}

\item{concentrations}{A \code{numeric} vector with the concentrations of the analyses.}

\item{...}{Additional arguments passed to the constructor.}
}
\description{
The StatisticAnalyses class is used for statistical analysis of tabular data.
}
\section{Slots}{

\describe{
\item{\code{analyses}}{A \code{data.frame} with the analyses data.}

\item{\code{type}}{A \code{character} vector with the type of analyses.}

\item{\code{classes}}{A \code{character} vector with the classes of the analyses.}

\item{\code{concentrations}}{A \code{numeric} vector with the concentrations of the analyses.}

\item{\code{info}}{(getter) A \code{data.frame} with the information of the analyses.}

\item{\code{number_variables}}{(getter) A \code{numeric} with the number of variables in the analyses.}

\item{\code{has_data}}{(getter) A \code{logical} indicating if the analyses has data.}

\item{\code{data}}{(getter/setter) A \code{list} with the data of the analyses.}

\item{\code{has_model}}{(getter) A \code{logical} indicating if the analyses has a model.}

\item{\code{model}}{(getter/setter) A \code{list} with the model of the analyses.}

\item{\code{has_test}}{(getter) A \code{logical} indicating if the analyses has a test.}

\item{\code{test}}{(getter) A \code{list} with the test of the analyses.}

\item{\code{has_prediction}}{(getter) A \code{logical} indicating if the analyses has a prediction.}

\item{\code{prediction}}{(getter) A \code{list} with the prediction of the analyses.}

\item{\code{has_quantification}}{(getter) A \code{logical} indicating if the analyses has a quantification.}

\item{\code{quantification}}{(getter/setter) A \code{list} with the quantification of the analyses.}
}}

