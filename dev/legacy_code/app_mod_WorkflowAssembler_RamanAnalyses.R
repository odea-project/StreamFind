#' @export
#' @noRd
.mod_WorkflowAssembler_Analyses_UI.RamanAnalyses <- function(x, id, ns) {
  .mod_WorkflowAssembler_Analyses_UI.MassSpecAnalyses(x, id, ns)
}

#' @noRd
.mod_WorkflowAssembler_Analyses_Server.RamanAnalyses <- function(
  x,
  id,
  ns,
  reactive_analyses,
  reactive_warnings,
  reactive_volumes,
  reactive_config
) {
  .mod_WorkflowAssembler_Analyses_Server.MassSpecAnalyses(
    x,
    id,
    ns,
    reactive_analyses,
    reactive_warnings,
    reactive_volumes,
    reactive_config
  )
}
