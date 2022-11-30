mc_queue = function(x,
                    FUN = identity,
                    mc.cores = getOption("text2vec.mc.cores", parallel::detectCores(logical = FALSE)),
                    poll_sleep = 0.01) {

  if(.Platform$OS.type != "unix")
    stop("calling `mc_queue` on windows platform. Should not happen, please report to https://github.com/dselivanov/text2vec/issues")

  it = x$clone(TRUE)
  result = new.env(parent = emptyenv())
  jobs_in_progress = list()
  # jobs_in_progress = new.env(parent = emptyenv())
  job_id = 0L

  while(!it$is_complete || length(jobs_in_progress) > 0) {
    available_queue_size = mc.cores - length(jobs_in_progress)

    if(available_queue_size > 0 && !it$is_complete) {
      job_id = job_id + 1L
      job = parallel::mcparallel(FUN(it$nextElem()), name = as.character(job_id))
      it$move_cursor()

      jobs_in_progress[[as.character(job_id)]] = job

      available_queue_size = available_queue_size - 1L

      logger$trace(
        "[mc_queue] adding job {%d} with pid=%d. Available queue size %d.",
        job_id,
        job$pid,
        available_queue_size
      )
    }
    finished_jobs = parallel::mccollect(jobs = jobs_in_progress, wait = FALSE)
    finished_jobs_ids = names(finished_jobs)

    for(finished_job_id in finished_jobs_ids) {

      result[[finished_job_id]] = finished_jobs[[finished_job_id]]

      # remove(finished_job_id, envir = jobs_in_progress)
      jobs_in_progress[[finished_job_id]] = NULL

      available_queue_size  = available_queue_size + 1
      logger$trace(
        "[mc_queue] finished job: {%s}, jobs in progress: {%s}",
        finished_job_id,
        paste(names(jobs_in_progress), collapse = ", ")
      )
    }
    Sys.sleep(poll_sleep)
  }
  as.list(result)
}
