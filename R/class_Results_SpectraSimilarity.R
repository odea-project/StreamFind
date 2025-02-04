#' @export
#' @noRd
SpectraSimilarity <- S7::new_class(
  name = "SpectraSimilarity",
  package = "StreamFind",
  parent = Results,
  properties = list(
    data = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(data = list()) {
    S7::new_object(
      Results(),
      name = "SpectraSimilarity",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      data = data
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "SpectraSimilarity")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_list(self@data)
    if (length(self@data) > 0) {
      lapply(self@data, checkmate::assert_data_table)
    }
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, SpectraSimilarity) <- function(x) {
  data.table::rbindlist(x@data, idcol = "analysis")[, 1:3]
}
