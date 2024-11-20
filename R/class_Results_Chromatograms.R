#' @export
#' @noRd
Chromatograms <- S7::new_class("Chromatograms", package = "StreamFind", parent = Results,
  
  properties = list(
    # MARK: chromatograms
    ## __chromatograms -----
    chromatograms = S7::new_property(S7::class_list, default = list()),
    
    # MARK: averaged
    ## __averaged -----
    is_averaged = S7::new_property(S7::class_logical, default = FALSE),
    
    # MARK: peaks
    ## __peaks -----
    peaks = S7::new_property(S7::class_list, default = list()),
    
    # MARK: has_peaks
    ## __has_peaks -----
    has_peaks = S7::new_property(S7::class_logical, getter = function(self) length(self@peaks) > 0),
    
    # MARK: calibration_model
    ## __calibration_model -----
    calibration_model = S7::new_property(S7::class_list, default = list())
  ),
  
  # MARK: constructor
  ## __constructor -----
  constructor = function(chromatograms = list(),
                         is_averaged = FALSE,
                         peaks = list(),
                         calibration_model = list()) {
    S7::new_object(
      Results(), 
      name = "Chromatograms",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      chromatograms = chromatograms,
      is_averaged = is_averaged,
      peaks = peaks,
      calibration_model = list()
    )
  },
  
  # MARK: validator
  ## __validator -----
  validator = function(self) {
    checkmate::assert_true(self@name == "Chromatograms")
    checkmate::assert_true(self@software == "StreamFind")
    checkmate::assert_list(self@chromatograms)
    checkmate::assert_list(self@peaks)
    checkmate::assert_logical(self@is_averaged, len = 1)
    checkmate::assert_list(self@calibration_model)
    if (length(self@chromatograms) > 0) {
      for (chromatogram in self@chromatograms) {
        checkmate::assert_data_frame(chromatogram)
      }
    }
    if (length(self@peaks) > 0) {
      for (peak in self@peaks) {
        checkmate::assert_data_frame(peak)
      }
    }
    NULL
  }
)
