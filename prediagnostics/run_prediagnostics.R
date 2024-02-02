source("/app/prediagnostics/config.R")
source("/app/prediagnostics/input.R")
source("/app/prediagnostics/prediagnostics.R")
source("/app/prediagnostics/output.R")

start = Sys.time()

# Directories
input_dir = file.path("/mnt", "data", "input")
output_dir = file.path("/mnt", "data", "output")

# Command line arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)>=2){
    index = strtoi(args[1])
    reaches_json = file.path(input_dir, paste(args[2]))
} else if (length(args)>=1) {
    index = strtoi(args[1])
    reaches_json = file.path(input_dir, 'reaches.json')
} else{
    index = strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
    reaches_json = file.path(input_dir, 'reaches.json')
}

# Run Diagnostics
run_diagnostics(input_dir, reaches_json, index, output_dir)

end = Sys.time()
print(paste0("Execution time: ", end - start))