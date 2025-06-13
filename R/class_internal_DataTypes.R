#' @noRd
DataTypes <- S7::new_class(
  name = "DataTypes",
  package = "StreamFind",
  properties = list(
    data_types = S7::new_property(S7::class_character, default = NA_character_)
  ),
  
  constructor = function() {
    data_types <- c(
      "MassSpec",
      "Raman",
      "Statistic"
    )
    
    names(data_types) <- c(
      "MassSpec",
      "Raman",
      "Statistic"
    )
    
    S7::new_object(S7::S7_object(), data_types = data_types)
  },

  validator = function(self) {
    
    data_types <- c(
      "MassSpec",
      "Raman",
      "Statistic"
    )
    
    checkmate::assert_character(self@data_types)
    checkmate::assert_true(
      all(self@data_types %in% c("MassSpec", "Raman", "Statistic"))
    )
    NULL
  }
)
