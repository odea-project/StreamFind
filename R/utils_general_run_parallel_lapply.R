
runParallelLapply <- function(obj_list,
                              run_parallel = FALSE,
                              FUN = NULL, ...) {

  #handlers(handler_progress(format="[:bar] :percent :eta :message"))

  if (run_parallel & length(obj_list) > 1) {

    #if (is.null(workers)) workers <- availableWorkers() - 1

    #if (length(obj_list) < workers) workers <- length(obj_list)

    if (future::supportsMulticore()) {

      plan("multicore") #not windows/not RStudio

    } else {

      plan("multisession")  #both Windows and Linux

    }

    #setDTthreads(1)

  } else {

    plan("sequential")

  }

  # with_progress({
  #
  #   p <- progressor(steps = length(obj_list))
  #
  #   list_out <- future_lapply(obj_list, FUN = FUN,
  #                             future.seed = NULL,
  #                             future.scheduling = FALSE,
  #                             future.chunk.size	= 1,
  #                             ...)
  #
  # })

  list_out <- future_lapply(obj_list, FUN = FUN,
                            future.seed = NULL,
                            future.scheduling = FALSE,
                            future.chunk.size	= 1,
                            ...)

  if (run_parallel)  {

    plan("sequential")
    #setDTthreads(0)

  }

  return(list_out)
}
