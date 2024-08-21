
#' @export
#' @noRd
StatisticAnalyses <- S7::new_class("StatisticAnalyses", package = "StreamFind", parent = Analyses,

  properties = list(
   
    analyses = S7::new_property(S7::class_data.frame, default = data.frame()),
    
    length = S7::new_property(S7::class_numeric, getter = function(self) {
      nrow(self@analyses)
    }, default = 0),
    
    names = S7::new_property(S7::class_character, getter = function(self) {
      rownames(self@analyses)
    }, default = character(0)),
    
    info = S7::new_property(S7::class_data.frame, getter = function(self) {
      if (self@length > 0) {
        df <- data.frame(
          "analysis" = self@names,
          "features" = ncol(self@analyses)
        )
       row.names(df) <- seq_len(nrow(df))
       df
      } else {
        data.frame()
      }
    }, default = data.frame())
  ),
  
  constructor = function(analyses = data.frame()) {
    S7::new_object(Analyses(), engine = "StatisticEngine", possible_formats = "csv", analyses = analyses)
  },
  
  validator = function(self) {
    valid <- all(
      checkmate::test_character(self@engine, len = 1),
      checkmate::test_choice(self@engine, choices = "StatisticEngine"),
      checkmate::test_character(self@formats),
      checkmate::test_data_frame(self@analyses),
      checkmate::test_character(rownames(self@analyses), len = nrow(self@analyses)),
      checkmate::test_numeric(as.matrix(self@analyses))
    )
    if (!valid) return(FALSE)
    NULL
  }
)

#' @export
#' @noRd
S7::method(add, StatisticAnalyses) <- function(x, data) {
  
}

#' @export
#' @noRd
S7::method(remove, StatisticAnalyses) <- function(x, value) {
  
}
