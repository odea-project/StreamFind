#' @noRd
DataTypes <- function() {
  structure(
    list(
      "types" = c("MassSpec", "Raman", "Statistic"),
      file_formats = list(
        "MassSpec" = c("mzML", "mzXML", "d", "raw"),
        "Raman" = c("asc", "sif", "json", "wdf", "sdf", "csv", "txt"),
        "Statistic" = c("csv")
      )
    ),
    class = "DataTypes"
  )
}
