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
# we want to specify index and reach json for local run
if (length(args)>=2){
    index = strtoi(args[1]) + 1
    reaches_json = file.path(input_dir, paste(args[2]))

    # we want to specify reach json for aws run
    if (length(args)>=3){
        index = strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
    }

# we want to specify only index for local run
} else if (length(args)>=1) {
    index = strtoi(args[1]) + 1
    reaches_json = file.path(input_dir, 'reaches.json')
    # we want to run on default settings for aws
} else{
    index = strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1
    reaches_json = file.path(input_dir, 'reaches.json')
}

# Run Diagnostics
output=run_diagnostics(input_dir, reaches_json, index, output_dir)

end = Sys.time()
print(paste0("Execution time: ", end - start))