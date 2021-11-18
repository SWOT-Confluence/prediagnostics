source("/app/prediagnostics/input.R")
source("/app/prediagnostics/prediagnostics.R")
source("/app/prediagnostics/output.R")

start <- Sys.time()

# Directories
input_dir <- file.path("/mnt", "data", "input")
output_dir <- file.path("/mnt", "data", "output")

# Command line arguments
args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 2) {
  reaches_json <- args[2]
  tolerance <- args[1]
} else if (length(args) == 1) {
  reaches_json <- "reaches.json"
  tolerance <- args[1]
} else {
  reaches_json <- "reaches.json"
  tolerance <- 0.25
}

# Run Diagnostics
index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
run_diagnostics(input_dir, reaches_json, tolerance, index, output_dir)

end <- Sys.time()
print(paste0("Execution time: ", end - start))