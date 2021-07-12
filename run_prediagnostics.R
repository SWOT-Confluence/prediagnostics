source("input.R")
source("prediagnostics.R")
source("output.R")

start <- Sys.time()
input_dir <- "/home/nikki/Documents/confluence/workspace/diagnostics/pre_data"   ## CHANGE ME
run_diagnostics(input_dir)
end <- Sys.time()
print(paste0("Execution time: ", end - start))