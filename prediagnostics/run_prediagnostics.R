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

if (length(args)>=1){
    reaches_json = file.path(input_dir, paste('reaches_',strtoi(args[1]),'.json'))
} else{
    reaches_json = file.path(input_dir, 'reaches.json')
}

# Run Diagnostics
index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
run_diagnostics(input_dir, reaches_json, index, output_dir)

end <- Sys.time()
print(paste0("Execution time: ", end - start))