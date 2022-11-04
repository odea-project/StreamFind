
runParallelLapply <- function(obj_list,
                              run_parallel = FALSE,
                              workers = NULL, FUN = NULL, ...) {

  handlers(handler_progress(format="[:bar] :percent :eta :message"))

  if (run_parallel) {

    if (is.null(workers)) {
      #workers <- length(availableWorkers()) - 1
      workers <- availableCores() -1
    }

    plan("multisession", workers = workers)  #both Windows and Linux
    #plan("multicore", workers) #not windows/not RStudio

  } else {

    plan("sequential")

  }

  with_progress({

    p <- progressor(steps = length(obj_list))

    list_out <- future_lapply(obj_list, FUN = FUN, future.seed = TRUE, ...)

  })

  if (run_parallel)  plan("sequential")

  return(list_out)
}
