#' **CoreEngine_S7** S7 class and methods
#'
#' @description The `CoreEngine_S7` S7 class is a basic data processor with generic methods for handling data.
#'
#' @noRd
#'
# CoreEngine_S7 <- S7::new_class("CoreEngine_S7", package = "StreamFind",
#   
#   properties = list(
#     
#     headers = S7::new_property(ProjectHeaders, default = ProjectHeaders()),
#     
#     workflow = S7::new_property(Workflow, default = Workflow()),
#     
#     analyses = S7::new_property(Analyses, default = Analyses()),
#     
#     results = S7::new_property(S7::class_list, default = list()),
#     
#     file = S7::new_property(EngineSaveFile, default = EngineSaveFile())
#     
#   ),
#   
#   constructor = function(
#     headers = ProjectHeaders(),
#     workflow = Workflow(),
#     analyses = Analyses(),
#     results = list()) {
#     
#     # if (file@is_saved) {
#     #   engine <- load(file@path)
#     #   message("\U2713 Engine loaded!")
#     #   return(engine)
#     # }
#     
#     if (!is.null(headers)) {
#       if (!is(headers, "StreamFind::ProjectHeaders")) {
#         warning("Headers not added! Not valid.")
#       }
#     }
#     
#     if (!is.null(workflow)) {
#       if (!is(workflow, "StreamFind::Workflow")) {
#         warning("Workflow not added! Not valid.")
#       }
#     }
#     
#     if (!is.null(analyses)) {
#       if (!is(analyses, "StreamFind::Analyses")) {
#         warning("Analyses not added! Not valid.")
#       }
#     }
#     
#     if (!is.null(results)) {
#       if (is(results, "list")) {
#         if (!all(vapply(results, is, "StreamFind::Results"))) {
#           warning("Results not added! Not valid.")
#         }
#       } else {
#         warning("Results not added! Not valid.")
#       }
#     }
#     
#     file <- EngineSaveFile()
#     
#     message("\U2713 Engine created!")
#     
#     S7::new_object(S7::S7_object(), headers = headers, workflow = workflow, analyses = analyses, results = results, file = file)
#   }
# )

#' @noRd
# S7::method(load, S7::class_character) <- function(x) {
#   
#   if (tools::file_ext(x) == "sqlite") {
#     
#     if (!file.exists(x)) {
#       warning("File does not exist!")
#       return(NULL)
#     }
#     
#     db <- .openCacheDBScope(file = x)
#     engine_name <- DBI::dbListTables(db)
#     
#     if (length(engine_name) != 0) {
#       
#       if (length(engine_name) > 1) {
#         warning("Engine name is not unique!")
#         return(NULL)
#       }
#       
#       if (engine_name %in% .get_available_engines()) {
#         
#         hash <- .make_hash(engine_name)
#         data <- .load_cache_backend(x, engine_name, hash)
#         
#         if (!is.null(data)) {
#           engine_call <- get(engine_name, envir = .GlobalEnv)
#           engine_call_new <- engine_call[["new"]]
#           
#           res <- do.call(engine_call_new, list(headers = data$headers, workflow = data$workflow, analyses = data$analyses, results = data$results))
#           
#           # res <- CoreEngine_S7(headers = data$headers, workflow = data$workflow, analyses = data$analyses, results = data$results)
#           message("\U2713 Engine data loaded from ", x, "!")
#           return(res)
#         
#         } else {
#           warning("Engine data not loaded!")
#           return(NULL)
#         }
#         
#       } else {
#         warning("Saved engine name is not available!")
#         return(NULL)
#       }
#     } else {
#       warning("No engine name found!")
#       return(NULL)
#     }
#     
#   } else {
#     warning("File is not an sqlite file!")
#     return(NULL)
#   }
# }
