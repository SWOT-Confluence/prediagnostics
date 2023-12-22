source("/app/prediagnostics/config.R")
source("/app/prediagnostics/input.R")
source("/app/prediagnostics/prediagnostics.R")
source("/app/prediagnostics/output.R")

start <- Sys.time()

# Directories
input_dir <- file.path("/mnt", "data", "input")
output_dir <- file.path("/mnt", "data", "output")

# Command line arguments
args = commandArgs(trailingOnly=TRUE)
# reaches_json = ifelse(identical(args, character(0)), "reaches.json", args[1])

all_reach_jsons = Sys.glob(file.path(input_dir, 'reaches*'))
reaches_json = all_reach_jsons[strtoi(args[1])]

# Run Diagnostics
index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
run_diagnostics(input_dir, reaches_json, index, output_dir)

end <- Sys.time()
print(paste0("Execution time: ", end - start))