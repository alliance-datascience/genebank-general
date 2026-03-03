pacman::p_load(httr2, httpuv, callr, bench)

wait_for_app_to_start <- function(url) { httr2::request(url) |> 
    httr2::req_retry(
      max_seconds = 5,
      backoff = function(attempt) 2 ** attempt
    )
}

measure_mem_usage <- function(work_dir) {
  result_file <- tempfile(fileext = "RDS")
  port <- httpuv::randomPort()
  app_process <- callr::r_bg(
    function(work_dir, result_file, port) {
      on.exit({ 
        saveRDS(bench::bench_process_memory(), result_file) 
      }) 
      
      shiny::runApp(appDir = work_dir, port = port)
    }, args = list(work_dir = work_dir, result_file = result_file, port = port))
  
  on.exit({ 
    if (app_process$is_alive()) {
      app_process$kill() 
    }
  })
  
  app_url <- paste0("http://127.0.0.1:", port)
  
  wait_for_app_to_start(app_url)
  
  utils::browseURL(app_url)
  
  cat ("Press [enter] to finish the test...")
  line <- readline()
  
  app_process$interrupt()
  
  app_process$wait()
  
  readRDS(result_file)
} 


measure_mem_usage(work_dir = "D:/genebank-general/coord_quality_score/")
