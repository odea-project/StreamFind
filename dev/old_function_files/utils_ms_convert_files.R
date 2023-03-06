
### formats -------------------------------------------------------------------------------------------------

#' @title compatibleFileFormatsForConversion
#'
#' @references
#'
#' \insertRef{proteo01}{streamFind}
#'
#' \insertRef{proteo02}{streamFind}
#'
#' @export
#'
#' @return A data.table with the formats compatible for conversion.
#'
compatibleFileFormatsForConversion <- function() {

  df_formats <- data.table::data.table(
    type = c(rep("ms", 11)),
    vendor = c("agilent", rep("bruker", 5), "sciex", "shimadzu", "thermo scientific", rep("waters",2)),
    format = c(".d", ".d", ".yep",  ".baf", ".fid", ".tdf", ".wiff", ".lcd", ".RAW", ".RAW", "UNIFI")
  )

  return(df_formats)
}

### convertFiles --------------------------------------------------------------------------------------------

#' @title convertFiles
#'
#' @description Function to convert vendor MS file formats to mzML/mzXML.
#'
#' @param files A vector with file full paths for conversion.
#' @param type A character string indicating the type of files for conversion.
#' Possible values are "ms".
#' @param outfile A character string with the format of the output file.
#' Possible values are "mzML" and "mzXML".
#' @param outdir The directory to place the output file. When \code{NULL}
#' the directory of the original file is used.
#' @param opts_list A list of further options to be passed to the respective
#' conversion tool. See \url{https://proteowizard.sourceforge.io/tools/msconvert.html}
#' for commands available for conversion of MS files with
#' \href{https://proteowizard.sourceforge.io/download.html}{msConvert}.
#'
#' @references
#'
#' \insertRef{proteo01}{streamFind}
#'
#' \insertRef{proteo02}{streamFind}
#'
#' @export
#'
convertFiles <- function(files = NULL, type = "ms", outfile = "mzML", outdir = NULL, opts_list = NULL) {

  files <- gsub("\\\\", "\\/", files)

  #check if files are compatible
  files_check <-  lapply(files, function(x, comfor) {
    temp <- comfor[grepl(paste0(".", tools::file_ext(x)), comfor$format, fixed = TRUE), ]
    temp$file <- x
    return(copy(temp))
  }, comfor = compatibleFileFormatsForConversion())

  files_check <- rbindlist(files_check)

  files_check <- unique(files_check, by = c("format", "file"))

  if ("ms" %in% type & nrow(files_check) > 0 & "ms" %in% files_check$type) {

    files_ms <- files_check$file[files_check$type %in% "ms"]

    outfiles_pwiz <- c("mzML", "mzXML", "mz5", "mgf", "text", "ms1", "cms1", "ms2", "cms2")

    if (length(outfile) != 1 & !outfile %in% outfiles_pwiz) {
      warning("Output file format must be mzML or mzXML")
      return()
    }

    # check is msConvert is installed and configured
    version <- system("msconvert --help", intern = TRUE)
    if (!(TRUE %in% grepl("ProteoWizard release:", version))) {
      warning("msConvert from ProteoWizard not found!")
      return()
    }

    version <- version[grepl("ProteoWizard release:", version)]
    version <- sub(".*: ", "", version)
    version <- sub(" \\(.*", "", version)
    version <- gsub("\\.", "", version)

    lapply(files_ms, function(f) {

      cmd_tx <- paste0("msconvert ", f)

      if (!is.null(outdir)) {
        cmd_tx <- paste0(cmd_tx, " -o ", outdir)
      } else {
        cmd_tx <- paste0(cmd_tx, " -o ", dirname(f))
      }

      cmd_tx <- paste0(cmd_tx, " --", outfile)

      if (!is.null(opts_list)) {
        for (i in seq_len(length(opts_list))) {
          cmd_tx <- paste0(cmd_tx, " --", names(opts_list[i]))
          cmd_tx <- paste0(cmd_tx, ' "',opts_list[[i]], '"')
        }
      }

      system(cmd_tx, intern = FALSE)

    })
  }
}
