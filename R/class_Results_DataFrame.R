#' @export
#' @noRd
DataFrame <- S7::new_class(
  name = "DataFrame",
  package = "StreamFind",
  parent = Results,
  properties = list(
    data = S7::new_property(S7::class_data.frame, default = data.frame())
  ),
  
  constructor = function(data = data.frame()) {
    dt <- as.data.frame(data)
    attributes(df) <- c(attributes(df), attributes(data)[-which(names(attributes(data)) == "dim")])
    S7::new_object(
      Results(),
      name = "DataFrame",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      data = as.data.frame(dt)
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "DataFrame")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_data_frame(self@data)
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, DataFrame) <- function(x) {
  cat("DataFrame with ", nrow(x@data), " rows and ", ncol(x@data), " columns")
}
