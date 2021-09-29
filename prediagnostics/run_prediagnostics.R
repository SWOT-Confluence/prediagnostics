source("/app/prediagnostics/input.R")
source("/app/prediagnostics/prediagnostics.R")
source("/app/prediagnostics/output.R")

start <- Sys.time()

input_dir <- file.path("/mnt", "data")
args <- commandArgs(trailingOnly=TRUE)
reaches_json <- ifelse(identical(args, character(0)), "reaches.json", args[1])
run_diagnostics(input_dir, reaches_json)

end <- Sys.time()
print(paste0("Execution time: ", end - start))