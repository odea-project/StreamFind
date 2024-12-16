#' @noRd
ConfigParameter <- S7::new_class(
  name = "ConfigParameter",
  package = "StreamFind",
  properties = list(
    name = S7::new_property(S7::class_character),
    description = S7::new_property(S7::class_character)
  ),
  constructor = function(name = NA_character_, description = NA_character_) {
    S7::new_object(S7::S7_object(), name = name, description = description)
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    NULL
  }
)

#' @noRd
ConfigDurationNotifications <- S7::new_class(
  name = "ConfigDurationNotifications",
  parent = ConfigParameter,
  package = "StreamFind",
  properties = list(value = S7::new_property(S7::class_numeric)),
  constructor = function(value = 10) {
    S7::new_object(
      S7::S7_object(),
      name = "Duration of pop-up notifications",
      description = "Duration in seconds for pop-up notifications",
      value = as.numeric(value)
    )
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    checkmate::assert_numeric(self@value, max.len = 1)
    NULL
  }
)

#' @noRd
ConfigExtraRoots <- S7::new_class(
  name = "ConfigExtraRoots",
  parent = ConfigParameter,
  package = "StreamFind",
  properties = list(value = S7::new_property(S7::class_character)),
  constructor = function(value = "") {
    S7::new_object(
      S7::S7_object(),
      name = "Extra Root Directories",
      description = "Extra root directories for file selection. Add spaces between directories.",
      value = value
    )
  },
  validator = function(self) {
    checkmate::assert_character(self@name)
    checkmate::assert_character(self@description)
    checkmate::assert_character(self@value)
    if (self@value != "") {
      checkmate::assert_true(all(dir.exists(self@value)))
    }
    NULL
  }
)

#' @noRd
AppConfig <- S7::new_class(
  name = "AppConfig",
  package = "StreamFind",
  properties = list(
    parameters = S7::new_property(S7::class_list),
    config_frame = S7::new_property(
      S7::class_data.frame,
      getter = function(self) {
        data.frame(
          name = sapply(self@parameters, function(x) x@name),
          value = sapply(self@parameters, function(x) as.character(x@value)),
          description = sapply(self@parameters, function(x) x@description)
        )
      }
    )
  ),
  constructor = function() {
    S7::new_object(
      S7::S7_object(),
      parameters = list(
        "ConfigDurationNotifications" = ConfigDurationNotifications(),
        "ConfigExtraRoots" = ConfigExtraRoots()
      )
    )
  },
  validator = function(self) {
    checkmate::assert_list(self@parameters)
    NULL
  }
)
