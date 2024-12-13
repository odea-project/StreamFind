#' @export
#' @noRd
Quantification <- S7::new_class(
  name = "Quantification",
  package = "StreamFind",
  parent = Results,
  
  properties = list(
    compounds = S7::new_property(S7::class_character, default = character()),
    models = S7::new_property(S7::class_list, default = list()),
    quantities = S7::new_property(S7::class_list, default = list())
  ),
  
  constructor = function(compounds = character(), models = list(), quantities = list()) {
    S7::new_object(
      Results(), 
      name = "Quantification",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      compounds = compounds,
      models = models,
      quantities = quantities
    )
  },
  
  validator = function(self) {
    checkmate::assert_true(self@name == "Quantification")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_character(self@compounds)
    checkmate::assert_list(self@models)
    checkmate::assert_list(self@quantities)
    NULL
  }
)

#' @export
#' @noRd
S7::method(show, Quantification) <- function(x) {
  cat("Quantification results\n")
  cat("Compounds: ", paste(x@compounds, collapse = ", "), "\n")
  cat("Models: ", length(x@models), "\n")
  cat("Quantities: ", length(x@quantities))
}
