
# ProjectHeaders_S4 <- setClass("ProjectHeaders_S4",
#   representation(headers = "list"),
#   prototype(
#     headers = list(
#       name = NA_character_,
#       author = NA_character_,
#       file = NA_character_,
#       date = Sys.time()
#     )
#   ),
#   validity = function(object) {
#     valid <- FALSE
#     if (is.list(object@headers)) {
#       valid <- TRUE
#       
#       if (any(sapply(object@headers[!names(object@headers) %in% "date"], function(x) !(is.character(x) || is.numeric(x)) || length(x) != 1))) {
#         warning("All headers must be of type character or numeric and of length 1!")
#         valid <- FALSE
#       }
#       
#       if (length(unique(names(object@headers))) != length(object@headers)) {
#         warning("ProjectHeaders must have names and not permitted duplicated names!")
#         valid <- FALSE
#       }
#       
#       if (!all(c("name", "author", "file", "date") %in% names(object@headers))) {
#         warning("ProjectHeaders must contain at least entries name, author, file and date!")
#         valid <- FALSE
#       }
#       
#       if ("name" %in% names(object@headers)) {
#         if (!is.character(object@headers$name)) {
#           warning("ProjectHeaders entry name must be character length 1!")
#           valid <- FALSE
#         }
#       }
#       
#       if ("author" %in% names(object@headers)) {
#         if (!is.character(object@headers$author)) {
#           warning("ProjectHeaders entry author must be character length 1!")
#           valid <- FALSE
#         }
#       }
#       
#       if ("file" %in% names(object@headers)) {
#         if (!is.na(object@headers$file)) {
#           if (!file.exists(object@headers$file)) {
#             warning("ProjectHeaders entry file must exist!")
#             valid <- FALSE
#           }
#         }
#       }
#       
#       if ("date" %in% names(object@headers)) {
#         if (!all(grepl("POSIXct|POSIXt", class(object@headers$date)))) {
#           warning("ProjectHeaders entry date class must be POSIXct or POSIXt length 1!")
#           valid <- FALSE
#         }
#       }
#     }
#     valid
#   }
# )
# 
# setMethod("initialize", "ProjectHeaders_S4", function(.Object, ...) {
#   x <- list(...)
#   if (length(x) == 1) if (is.list(x[[1]])) x <- x[[1]]
#   x_names <- names(x)
#   x <- lapply(x, function(h) {
#     if (any(is.null(h))) h <- NA_character_
#     if (any(is.na(h))) h <- NA_character_
#     h
#   })
#   
#   if ("date" %in% x_names) {
#     x[["date"]] <- as.POSIXct(x[["date"]])
#     attr(x[["date"]], "tzone") <- NULL
#   }
#   
#   if (!"name" %in% x_names) x$name <- NA_character_
#   if (!"author" %in% x_names) x$author <- NA_character_
#   if (!"file" %in% x_names) x$file <- NA_character_
#   if (!"date" %in% x_names) x$date <- Sys.time()
#   
#   if (!is.na(x$file)) {
#     if (!grepl(".sqlite", x$file)) {
#       warning("ProjectHeaders file must have extension .sqlite!")
#       x$file <- NA_character_
#     } else {
#       if (!file.exists(x$file)) {
#         warning(paste0(x$file, " does not exist!"))
#         x$file <- NA_character_
#       }
#     }
#   }
#   
#   .Object <- callNextMethod(.Object, headers = x)
#   .Object
# })
# 
# 
# setMethod("$", "ProjectHeaders_S4", function(x, name) {
#   x@headers[[name]]
# })










