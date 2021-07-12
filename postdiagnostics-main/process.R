input_dir <- "D:\\OneDrive -\ University of Massachusetts\\Active papers\\prediagnostics\\postdiagnostics-main\\post_data"    # CHANGE ME
setwd("D:\\OneDrive -\ University of Massachusetts\\Active papers\\prediagnostics\\postdiagnostics-main\\")

source("input.R")
source("output.R")
# Run FLPE diagnostics
data <- get_discharge_flpe(input_dir)
# diag_data_flpe <- run_diagnostics_flpe(data)
# write_data_flpe(diag_data)

# Run Integrator diagnostics - not complete (unsure of integrator output)
# data <- get_discharge_integrator(input_dir)
# diag_data_integrator <- run_diagnostics(data)
# write_data_integrator(diag_data)