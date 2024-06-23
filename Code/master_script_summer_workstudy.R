# Define the custom error handler
stop_on_error <- function() {
  traceback(2)
  stop("Script execution halted due to an error.")
}

# Set the custom error handler
options(error = stop_on_error)

################################################################################################################################
# Create parent subsidiary and M&A historical datasets  
#
# This script is the master script to replicate all data processing and analysis from Work Study 2024  
#
# The script is organized into the following sections:
# 1. Preliminaries
# 2. Create CIQ Parent Subisidiary and M&A datasets 
# 3. Summarize / validate CIQ data 
# 3. Process patent data ... (to be added)
# ...
################################################################################################################################

##################################################
# Preliminaries 
##################################################

job_name = 'Summer_Workstudy_June2024' # name of job 
checkpoint_date = '2024-06-10' # date of run job
latest_vintage = 'June2024' # put year oif latest 
# latest_vintage = '2020' # previous lastest vintage 

# determine machine running on 

on_local =T # mac OS, set to false if on a linux cluster

# create python environment from scratch

create_python_env_from_scratch = T # set to false if replicating 
initialize_python_env_on_new_machine = F # set to true if first time running this code to setup python-R links
	# IGNORE THE PYTHON ENVIRONMENT SETUP FOR NOW ... will be needed for the next phase 

if(on_local){

	# base_path = getwd() MZ: commented this out and need to hard define base_path because keeps changing
  base_path = "C:/Users/zwend/Documents/UofT Material/work study local"
	# if working directory is Code, then walk up one level 
	if(grepl('Code/',base_path)){
		base_path = gsub('Code/','',base_path)
	}

} else {
  base_path = '/project/patent_processing/CultivatingMarketPower/' 
  # .libPaths('/project/patent_processing/library/R')
}

setwd(base_path)



compile_from_scratch=F # set to true if want to transform raw data to final dataset from scratch (conditional on the other compile_x_from_scratch_options)
                       # F uses existing files, so only does the necessary residual work to generate the final dataset

# map directory dependencies and load R packages

source(paste0(base_path,'/code/setup/map_dependencies.R')) # mz: added '/' in front
	# this script contains all the necessary directory mappings, file paths to inputs and outputs, and loads all necessary R / python packages 

# mz: Load utility functions
source(paste0(base_path, '/code/setup/basic_options_and_utility_functions.R'))

#############################################################
# Create firm subsidiary dataset
#############################################################

output_dir <- base_path
subsidiary_dataset_file = paste1(output_dir, 'firm_data/subsidiary_dataset.RData')
# subsidiary_dataset_file = paste0(output_dir, 'firm_data/subsidiary_dataset.RData')

code_dir <- paste0(base_path, '/Code/raw_processing/')
if(!file.exists(subsidiary_dataset_file) | compile_from_scratch==T){
  print('preparing parent subsidiary data - initiating')
  tic('prepare subsidiary data')
  source(paste1(code_dir, 'process_CIQ_data/create_subsidiary_dataset.R')) 
  toc()
  print('preparing firm subsidiary data - complete!')
}




