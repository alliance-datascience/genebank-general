# suppressMessages(if(!require(plumber)){install.packages('plumber');library(plumber)}else{library(plumber)})

library(plumber)

pr <- plumber::plumb(file.path(Sys.getenv("SCRIPT_DIR"), "REST_api.R"))
pr$run(host = "0.0.0.0", port=8081)


