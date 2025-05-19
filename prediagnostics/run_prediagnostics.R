library(optparse)
library(reticulate)


source("/app/prediagnostics/input.R")
source("/app/prediagnostics/output.R")
source("/app/prediagnostics/prediagnostics.R")
source("/app/prediagnostics/outlier_filter.R")
source("/app/prediagnostics/apply_flags_reach.R")
source("/app/prediagnostics/apply_flags_node.R")
source("/app/prediagnostics/filter_dxa.R")

CONFIG_FILENAME = "prediagnostics_config.R"
PYTHON_EXE = "/usr/bin/python3"
PYTHON_FILE = "/app/prediagnostics/sos_read/sos_read.py"
TMP_PATH = "/tmp"

start = Sys.time()

# Directories
input_dir = file.path("/mnt", "data", "input")
output_dir = file.path("/mnt", "data", "output")

# Command line arguments
option_list <- list(
  make_option(c("-i", "--index"), type = "integer", default = NULL, help = "Index to run on"),
  make_option(c("-b", "--config_bucket"), type = "character", default = "", help = "Bucket key to find the sos"),
  make_option(c("-r", "--reaches_json"), type = "character", default = "reaches.json", help = "Name of reaches.json")
)
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

index <- opts$index
if (index == -256){
  index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX"))
}
index <- index + 1    # Add 1 to AWS 0-based index

config_bucket <- opts$config_bucket
reaches_json = file.path(input_dir, opts$reaches_json)

# Load Config file from S3
if (config_bucket != "") {
  use_python(PYTHON_EXE)
  source_python(PYTHON_FILE)

  config_filepath = file.path(TMP_PATH, CONFIG_FILENAME)
  download_sos(config_bucket, config_filepath)

  # Run Diagnostics on S3 config file
  if (file.exists(config_filepath)) {
    source(config_filepath)
    output=run_diagnostics(input_dir, reaches_json, index, output_dir)
  } else {
    print("Config file could not be downloaded and prediagnostics will not run.")
  }

 # Run Diagnostics on local config file
} else {
  print("Config file will be run on local config: '/app/prediagnostics/config.R'")
  source("/app/prediagnostics/config.R")
  output=run_diagnostics(input_dir, reaches_json, index, output_dir)
}

end = Sys.time()
print(paste0("Execution time: ", end - start))