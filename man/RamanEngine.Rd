% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engine_RamanEngine.R
\name{RamanEngine}
\alias{RamanEngine}
\title{Raman Spectroscopy Engine}
\description{
The \emph{RamanEngine} R6 class is a framework for parsing, processing, inspecting and
storing Raman spectroscopic data. Raman data can be loaded from "asc", "sif", "json", "wdf",
"sdf", "csv" and "txt" files.
}
\references{
\insertRef{orpl01}{StreamFind}
}
\section{Super class}{
\code{\link[StreamFind:CoreEngine]{StreamFind::CoreEngine}} -> \code{RamanEngine}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{Spectra}}{\code{RamanSpectra} results object for each analysis or replicate.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RamanEngine-new}{\code{RamanEngine$new()}}
\item \href{#method-RamanEngine-get_analysis_names}{\code{RamanEngine$get_analysis_names()}}
\item \href{#method-RamanEngine-get_replicate_names}{\code{RamanEngine$get_replicate_names()}}
\item \href{#method-RamanEngine-get_blank_names}{\code{RamanEngine$get_blank_names()}}
\item \href{#method-RamanEngine-get_files}{\code{RamanEngine$get_files()}}
\item \href{#method-RamanEngine-get_overview}{\code{RamanEngine$get_overview()}}
\item \href{#method-RamanEngine-get_spectra}{\code{RamanEngine$get_spectra()}}
\item \href{#method-RamanEngine-get_spectra_matrix}{\code{RamanEngine$get_spectra_matrix()}}
\item \href{#method-RamanEngine-get_chromatograms_peaks}{\code{RamanEngine$get_chromatograms_peaks()}}
\item \href{#method-RamanEngine-add_analyses}{\code{RamanEngine$add_analyses()}}
\item \href{#method-RamanEngine-remove_analyses}{\code{RamanEngine$remove_analyses()}}
\item \href{#method-RamanEngine-add_replicate_names}{\code{RamanEngine$add_replicate_names()}}
\item \href{#method-RamanEngine-add_blank_names}{\code{RamanEngine$add_blank_names()}}
\item \href{#method-RamanEngine-has_spectra}{\code{RamanEngine$has_spectra()}}
\item \href{#method-RamanEngine-plot_spectra}{\code{RamanEngine$plot_spectra()}}
\item \href{#method-RamanEngine-plot_spectra_3d}{\code{RamanEngine$plot_spectra_3d()}}
\item \href{#method-RamanEngine-plot_spectra_baseline}{\code{RamanEngine$plot_spectra_baseline()}}
\item \href{#method-RamanEngine-plot_chromatograms}{\code{RamanEngine$plot_chromatograms()}}
\item \href{#method-RamanEngine-plot_chromatograms_peaks}{\code{RamanEngine$plot_chromatograms_peaks()}}
\item \href{#method-RamanEngine-clone}{\code{RamanEngine$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="clear_cache"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-clear_cache'><code>StreamFind::CoreEngine$clear_cache()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="clear_results"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-clear_results'><code>StreamFind::CoreEngine$clear_results()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="get_cache_info"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-get_cache_info'><code>StreamFind::CoreEngine$get_cache_info()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="get_cache_size"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-get_cache_size'><code>StreamFind::CoreEngine$get_cache_size()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="has_analyses"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-has_analyses'><code>StreamFind::CoreEngine$has_analyses()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="has_results"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-has_results'><code>StreamFind::CoreEngine$has_results()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="load"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-load'><code>StreamFind::CoreEngine$load()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="print"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-print'><code>StreamFind::CoreEngine$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="run"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-run'><code>StreamFind::CoreEngine$run()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="run_app"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-run_app'><code>StreamFind::CoreEngine$run_app()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="run_workflow"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-run_workflow'><code>StreamFind::CoreEngine$run_workflow()</code></a></span></li>
<li><span class="pkg-link" data-pkg="StreamFind" data-topic="CoreEngine" data-id="save"><a href='../../StreamFind/html/CoreEngine.html#method-CoreEngine-save'><code>StreamFind::CoreEngine$save()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-new"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-new}{}}}
\subsection{Method \code{new()}}{
Creates an R6 class \emph{RamanEngine}. Child of \emph{CoreEngine} R6 class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$new(metadata = NULL, workflow = NULL, analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{metadata}}{A named list or a \code{Metadata} object. Each list element must have length one.
There is no type restrictions except for name (must be type character), author (must be type
character) and date (must be class POSIXct or POSIXt). A file entry must be a character string
with a full path to a file.}

\item{\code{workflow}}{A \code{Workflow} object, a list of \code{ProcessingStep} objects or a character string
with a full path to a \emph{rds} or \emph{json} file containing a \code{Workflow}.}

\item{\code{analyses}}{A \code{RamanAnalyses} S7 class object or a \verb{character vector} with full file
paths to "asc", "sif", "json", "wdf", "sdf", "csv" and/or "txt" raman files or a \code{data.frame}
with colnames \code{file}, \code{replicate} and \code{blank}. The "replicate" column is used to group the
analyses and the "blank" column is used to identify the blank samples. The "file" column is
the full to the raman files.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_analysis_names"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_analysis_names}{}}}
\subsection{Method \code{get_analysis_names()}}{
Gets the analysis replicate names.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_analysis_names(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_replicate_names"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_replicate_names}{}}}
\subsection{Method \code{get_replicate_names()}}{
Gets the analysis replicate names.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_replicate_names(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_blank_names"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_blank_names}{}}}
\subsection{Method \code{get_blank_names()}}{
Gets the analysis blank replicate names.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_blank_names(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_files"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_files}{}}}
\subsection{Method \code{get_files()}}{
Gets the full file paths of each analysis.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_files(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_overview"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_overview}{}}}
\subsection{Method \code{get_overview()}}{
Gets an overview data.frame of all the analyses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_overview()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_spectra"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_spectra}{}}}
\subsection{Method \code{get_spectra()}}{
Gets a list of spectra \code{data.table} objects for each analysis/replicate.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_spectra(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{useRawData}}{Logical of length one. Set to \code{TRUE} for parsing raw data not data in
results/processed.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_spectra_matrix"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_spectra_matrix}{}}}
\subsection{Method \code{get_spectra_matrix()}}{
Gets a matrix with spectra.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_spectra_matrix(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{useRawData}}{Logical of length one. Set to \code{TRUE} for parsing raw data not data in
results/processed.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-get_chromatograms_peaks"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-get_chromatograms_peaks}{}}}
\subsection{Method \code{get_chromatograms_peaks()}}{
Gets a \code{data.table} with chromatographic peaks.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$get_chromatograms_peaks(analyses = NULL, targets = NULL, rt = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-add_analyses"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-add_analyses}{}}}
\subsection{Method \code{add_analyses()}}{
Adds analyses to the engine from Raman files. Note that when adding new files,
any existing results are removed.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$add_analyses(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{A \verb{character vector} with full file paths to "asc", "sif", "json", "wdf",
"sdf", "csv" and/or "txt" raman files.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-remove_analyses"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-remove_analyses}{}}}
\subsection{Method \code{remove_analyses()}}{
Removes analyses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$remove_analyses(analyses = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-add_replicate_names"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-add_replicate_names}{}}}
\subsection{Method \code{add_replicate_names()}}{
Adds replicate names to the analysis.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$add_replicate_names(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Character vector with the replicate names. Must have the same length as the
number of analyses.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-add_blank_names"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-add_blank_names}{}}}
\subsection{Method \code{add_blank_names()}}{
Adds blank names to the analysis.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$add_blank_names(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Character vector with the replicate names. Must have the same length as the
number of analyses and must be one of replicate names.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-has_spectra"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-has_spectra}{}}}
\subsection{Method \code{has_spectra()}}{
Checks if there are spectra, returning \code{TRUE} or \code{FALSE}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$has_spectra()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-plot_spectra"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-plot_spectra}{}}}
\subsection{Method \code{plot_spectra()}}{
Plots spectra.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$plot_spectra(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{useRawData}}{Logical of length one. Set to \code{TRUE} for parsing raw data not data in
results/processed.}

\item{\code{xLab}}{A string with the title for the x axis.}

\item{\code{yLab}}{A string with the title for the y axis.}

\item{\code{title}}{A string with the title.}

\item{\code{colorBy}}{A string defining how to legend the plot. Possible values are \code{analyses}, \code{targets} (the default) or
\code{replicates}.}

\item{\code{interactive}}{Logical (length 1). When \code{TRUE}, the data is plotted interactively using \pkg{plotly}.}

\item{\code{renderEngine}}{The engine to render the data. The default is "webgl".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-plot_spectra_3d"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-plot_spectra_3d}{}}}
\subsection{Method \code{plot_spectra_3d()}}{
Plots spectra in 3D, when a time dimension is available.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$plot_spectra_3d(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE,
  legendNames = TRUE,
  colorBy = "analyses",
  xLab = NULL,
  yLab = NULL,
  zLab = NULL,
  renderEngine = "webgl"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{useRawData}}{Logical of length one. Set to \code{TRUE} for parsing raw data not data in
results/processed.}

\item{\code{legendNames}}{A character vector with the same length as the targets or \code{TRUE} or \code{FALSE} for using the name in
the added targets as legend of the plot.}

\item{\code{colorBy}}{A string defining how to legend the plot. Possible values are \code{analyses}, \code{targets} (the default) or
\code{replicates}.}

\item{\code{xLab}}{A string with the title for the x axis.}

\item{\code{yLab}}{A string with the title for the y axis.}

\item{\code{zLab}}{Character (length 1). The label for the z-axis.}

\item{\code{renderEngine}}{The engine to render the data. The default is "webgl".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-plot_spectra_baseline"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-plot_spectra_baseline}{}}}
\subsection{Method \code{plot_spectra_baseline()}}{
Plots the spectra baseline correction.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$plot_spectra_baseline(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{xLab}}{A string with the title for the x axis.}

\item{\code{yLab}}{A string with the title for the y axis.}

\item{\code{title}}{A string with the title.}

\item{\code{colorBy}}{A string defining how to legend the plot. Possible values are \code{analyses}, \code{targets} (the default) or
\code{replicates}.}

\item{\code{interactive}}{Logical (length 1). When \code{TRUE}, the data is plotted interactively using \pkg{plotly}.}

\item{\code{renderEngine}}{The engine to render the data. The default is "webgl".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-plot_chromatograms"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-plot_chromatograms}{}}}
\subsection{Method \code{plot_chromatograms()}}{
Plots chromatograms from each analysis/replicates by applying a cumulative sum
of the spectrum signals for each time unit.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$plot_chromatograms(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  shift = NULL,
  minIntensity = NULL,
  useRawData = FALSE,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  colorBy = "analyses",
  interactive = TRUE,
  renderEngine = "webgl"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{shift}}{Numeric of length two with the minimum and maximum shift, in cm\if{html}{\out{<sup>}}-1\if{html}{\out{</sup>}}.}

\item{\code{minIntensity}}{Numeric (length 1) with the minimum intensity.}

\item{\code{useRawData}}{Logical of length one. Set to \code{TRUE} for parsing raw data not data in
results/processed.}

\item{\code{xLab}}{A string with the title for the x axis.}

\item{\code{yLab}}{A string with the title for the y axis.}

\item{\code{title}}{A string with the title.}

\item{\code{colorBy}}{A string defining how to legend the plot. Possible values are \code{analyses}, \code{targets} (the default) or
\code{replicates}.}

\item{\code{interactive}}{Logical (length 1). When \code{TRUE}, the data is plotted interactively using \pkg{plotly}.}

\item{\code{renderEngine}}{The engine to render the data. The default is "webgl".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-plot_chromatograms_peaks"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-plot_chromatograms_peaks}{}}}
\subsection{Method \code{plot_chromatograms_peaks()}}{
Plots peaks from chromatograms.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$plot_chromatograms_peaks(
  analyses = NULL,
  targets = NULL,
  rt = NULL,
  title = NULL,
  legendNames = TRUE,
  colorBy = "targets",
  xLab = NULL,
  yLab = NULL,
  interactive = TRUE,
  renderEngine = "webgl"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{analyses}}{Character or numeric vector with names or indexes of analyses in the \code{Analyses}
object.}

\item{\code{targets}}{character vector with names of chromatographic peaks for selective retrieval of
data.}

\item{\code{rt}}{Numeric of length two with the minimum and maximum retention time, in seconds.}

\item{\code{title}}{A string with the title.}

\item{\code{legendNames}}{A character vector with the same length as the targets or \code{TRUE} or \code{FALSE} for using the name in
the added targets as legend of the plot.}

\item{\code{colorBy}}{A string defining how to legend the plot. Possible values are \code{analyses}, \code{targets} (the default) or
\code{replicates}.}

\item{\code{xLab}}{A string with the title for the x axis.}

\item{\code{yLab}}{A string with the title for the y axis.}

\item{\code{interactive}}{Logical (length 1). When \code{TRUE}, the data is plotted interactively using \pkg{plotly}.}

\item{\code{renderEngine}}{The engine to render the data. The default is "webgl".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RamanEngine-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RamanEngine-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RamanEngine$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
