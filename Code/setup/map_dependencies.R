########################
# Map dependencies
#
#   Set R options / load R-package dependencies
#   Define the directories where the relevant data and source code is located
#   Provide the file names and paths to all relevant inputs / outputs
#   Load R-package dependencies
#
# Last Modified: June 10th, 2024
########################

##############
# Options and packages
##############

# install / load package dependencies 

local_library_path = 'dependencies/R/'
RpackageSnapshot_file = paste0(base_path, "/dependencies/R/RpackageSnapshot_", job_name,".txt") # added '/', added directory as well
repository <- "https://mirror.csclub.uwaterloo.ca/CRAN/"
options(repos=repository)

python_virtual_env_path <- "~/.local/x86_64_bin/.virtualenvs/CMP_Studio_3.8"
python_path = python_virtual_env_path
# python_virtual_env_path  <- "~/.local/x86_64_bin/.virtualenvs/CMP_Studio_3.8"

# python_path = paste(base_path, 'dependencies/python/.venv/bin/python',sep='')

use_RpackageSnapshot = F # set to true if want to load packages from RpackageSnapshot_file

if(file.exists(RpackageSnapshot_file) & use_RpackageSnapshot==T){
	# load packages from the snapshot
	library(Require)
	print(paste0("Loading R libraries from ", RpackageSnapshot_file))
	# Require::clearRequirePackageCache()

	Require::Require(packageVersionFile=RpackageSnapshot_file)
} else {
	# require packages
	source(paste0(base_path, "/Code/setup/RlibraryList.R")) # mz: added '/'
}

# load basic utility functions and options

source(paste0(base_path, "/Code/setup/basic_options_and_utility_functions.R"))  # mz: added '/'

# download uspto data 

download_uspto_data=F # set to true if want to download raw uspto data (F if dta files downloaded locally)

# make Python linkages

python_path = python_virtual_env_path
# python_virtual_env_path  <- "~/.local/x86_64_bin/.virtualenvs/CMP_Studio_3.8"
create_python_env = T # set to true if running for the first time
initialize_python_env_on_new_machine = F

load_fuzzy_string_python_env = F # mz: newly added, temp fix
if(load_fuzzy_string_python_env==T){
	# load python packages and integrate with R  
	source(paste(base_path, 'Code/setup/python_Rsetup.R', sep=''))  
}

##############
# Directories and file paths
##############


# code (sub-)directories

code_dir = paste(base_path, 'Code/',sep='')
string_matching_dir = paste(base_path, 'Code/rempel_firm_string_matching_scripts/',sep='')

# intermediate output

output_dir = paste(base_path, '/processed_data/', sep='') # added '/'
output_data_results_dir =  paste(base_path, '/processed_data/shared/', sep='') # use this directory for data consolidated across local/linstat?

# final output = output for empirical analysis (e.g. final datasets used to create tables / figures for paper)

output_results_dir = paste(base_path, 'Output/', sep='') 
output_slides_dir = paste(base_path, 'Slides/', sep='') 
tables_dir = paste(base_path, 'Tables/', sep='')
figures_dir = paste(base_path, 'Figures/', sep='')

# capital IQ subsidiary / merger data 

subsidiary_data_dir = paste(base_path, '/Data/CIQ/corporate_trees_investments_and_products/',sep='') # mz: changed path name 
# for testing
# subsidiary_data_dir = paste(base_path, '/Captial IQ Data/subsidiary_data_dir/',sep='')
merger_dir = paste(base_path, '/Data/CIQ/mergers/',sep='') # mz: changed path name
mergerSellof_dir =paste(base_path,  '/Data/CIQ/spinoffs/',sep='') # UPDATE TO Spinoff 

capiq_legal_dev_patents_dir=paste(base_path, 'Data/CIQ/CapitalIQ_patent_legal_developments/',sep='')
capiq_law_firm_dir = paste(base_path, 'Data/CIQ/CIQ_law_firm_data/',sep='')

CIQ_gvkey_link_file = paste(base_path, 'Data/CIQ/CIQid_gvkey_link_table.csv', sep='') #  MISSING, NEEDS TO BE MIGRATED OVER 

# final subsidiary dataset
subsidiary_dataset_file = paste1(output_dir, 'firm_data/subsidiary_dataset.RData')

# corporate tree data
corporate_tree_data_file = paste(output_dir, 'firm_data/complete_corporate_tree_dataset_2020.RData',sep='')


# Subsidiary data with merger data

no_direct_match_subsidiaries_file = paste(output_dir, 'firm_data/no_direct_match_subsidiaries.RData',sep='')

# Mergers dataset

CIQ_merger_dataset_file = paste(output_dir, 'firm_data/full_capiq_merger_dataset', latest_vintage,'.RData',sep='')
CIQ_merger_dataset_clean_file = paste(output_dir, 'firm_data/full_capiq_merger_dataset_', latest_vintage,'_clean.RData',sep='')

  # clean is the CIQ merger dataset with the firm name strings processed in preparation for fuzzy matching

initial_collated_merger_data_file = paste(output_dir, 'firm_data/capiq_merger_dataset_', latest_vintage, '.RData',sep='')
CIQ_selloff_dataset_file = paste(output_dir, 'firm_data/capiq_mergerSellOff_dataset_', latest_vintage,'.RData',sep='')

subsidiary_data_merger_history_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_', latest_vintage,'.RData', sep='')

corporate_tree_data_w_merger_transactions_file = paste(output_dir, 'firm_data/raw_merger_history_complete_corporate_tree_dataset_', latest_vintage, '.RData', sep='')

complete_corporate_tree_dataset_2020_fuzzy_matched_orig_file= paste1(output_dir, 'tmp/complete_corporate_tree_dataset_', latest_vintage, '.RData')
  # above file contains the score of fuzzy matching with the merger data


duplicate_acquired_firms_identified_in_tree_file = paste(output_dir, 'firm_data/multiple_acquired_firms_corporate_tree_dataset_', latest_vintage,'.RData', sep='')
full_capiq_merger_dataset_2020_w_cancelled_transactions_file = paste(output_dir, 'firm_data/full_capiq_merger_dataset_', latest_vintage, '_w_cancelled_transactions.RData',sep='')
intermediate_subsidiary_dataset_parent_processed_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_', latest_vintage,'_parent_processed.csv', sep='')



################
## IGNORE BELOW FOR NOW 
################

# FJC litigation data
FJC_data_dir = paste(base_path, 'Data/patent_litigation/FJCdata/',sep='') # find and replace data_dir with FJC_data_dir
patent_lit_data_dir = paste(base_path, 'Data/patent_litigation/',sep='')

# patent data
uspto_patent_dir = paste(base_path, 'Data/patent_data/uspto/', sep='') # convert patent_dir to uspto_patent_dir
NBER_patents_dir = paste(base_path, 'Data/patent_data/NBERpatents/', sep='') 
kpss_patent_dir = paste(base_path, 'Data/patent_data/KPSS_data/', sep='') 
patentsView_dir = paste(base_path, 'Data/patent_data/patentsView/', sep='') 

# compustat and supplemental data

WRDS_data_dir = paste1(base_path, 'Data/WRDS_data/')
industry_def_dir = paste(base_path, 'Data/supplemental_compustat/Industry_Definitions/', sep='') 
HP_folder  = paste(base_path, 'Data/supplemental_compustat/HobergPhillips/', sep='') 
supplemental_compustat_folder  = paste(base_path, 'Data/supplemental_compustat/', sep='') 

#####################
# Input file names
#####################

# USPTO patent assignment dataset 

uspto_patent_assignment_data_file = paste(uspto_patent_dir, "uspto_patent_assignment_dataset.RData",sep='')
uspto_patent_assignee_data_file  = paste(uspto_patent_dir, "uspto_patent_assignee_dataset.RData",sep='')
uspto_patent_assignor_data_file = paste(uspto_patent_dir, "uspto_patent_assignor_dataset.RData",sep='')
uspto_patent_documentid_data_file = paste(uspto_patent_dir, "uspto_patent_documentid_dataset.RData",sep='')
uspto_patent_documentid_admin_data_file = paste(uspto_patent_dir, "uspto_patent_documentid_admin_dataset.RData",sep='')
uspto_conveyance_data_file = paste(uspto_patent_dir, "uspto_patent_assignment_conveyance_dataset.RData",sep='')

# FJC patent litigation data

fjc_data_file =  paste1( FJC_data_dir,'wrds_fjc_integrated_database.dta')

# USPTO trial supplement

patent_cases_file = paste1(patent_lit_data_dir, 'patent_litigation_database/patents.dta')
cases_file = paste1(patent_lit_data_dir, 'patent_litigation_database/cases.dta')
names_file = paste1(patent_lit_data_dir,'patent_litigation_database/names.dta')

# Patent Pilot Program judge list (self-compiled from primary sources)

ppp_initial_judge_file = paste1(patent_lit_data_dir, 'PPP_judges_initial_list.csv')

# WRDS data

exec_file = paste1(WRDS_data_dir, "exec_data.RData")
exec_compustat_data_file = paste(WRDS_data_dir, 'compustat_exec_data.RData', sep='')
exec_compustat_data_raw_file=paste(WRDS_data_dir, "compustat_exec_data_raw.RData",sep='')
capital_iq_compustat_link_data_file = paste1(WRDS_data_dir, "CapitalIQ_compustat_link.dta")
ind_48_zip_file = paste(base_path, 'Data/supplemental_compustat/Industry_Definitions/Siccodes48.txt',sep='')


# KPSS raw data - xi is firm level
kpss_patents_data_xi_file = paste1(base_path, 'Data/patent_data/KPSS_data/patents_xi.csv')

# NBER patent database

nber_patents_linking_2_compustat_data_file = paste(NBER_patents_dir, 'pdpcohdr.dta', sep='')
nber_patents_dynass_file =  paste(NBER_patents_dir, 'dynass.dta', sep='')
nber_patents_patassg_file=  paste(NBER_patents_dir,'patassg.dta', sep='')


# patentsView data

patentsView_patents_file = paste(patentsView_dir, 'patent.RData',sep='') # INPUT FILE


# lawyer data

CIQ_law_firm_cross_section_file=paste(capiq_law_firm_dir, 'legal_firms_crossSection_June3_2021.xls',sep='')

# list of company acronyms for firm string string processing 

firm_acronym_table  <- paste0(code_dir, "fuzzy_match_core_routines/company_type_acronym_table_2023.csv")

###################
# Output file names
###################


########
# I) USPTO patent data processed files
########

# a) combined assignor/assignee USPTO PAD patent assignment datasets 
#		created by process_uspto_patent_assignment_data.R 

uspto_dynamic_assignments_file = paste(output_dir, 'patent_data/uspto_dynamic_assignments.RData',sep='')
	# raw dynamic assignment data = patent transfer ownership transactions with participants

uspto_licensing_file = paste(output_dir, 'patent_data/uspto_licensing_data.RData',sep='')
	# licensing transactions 

# b) cleaned participant names from USPTO PAD patent assignment datasets 
#		created by clean_uspto_assignment_participants.R 

uspto_patent_assignment_participants_file = paste(output_dir, 'patent_data/uspto_patent_assignment_participants.RData',sep='')


  #  list of USPTO participants, after eliminating redundancies 

disambiguated_uspto_participants_file = paste(output_dir, 'patent_data/disambiguated_uspto_participants.RData',sep='')
# patent_assignment_participants_file  = paste(output_dir, 'patent_data/uspto_patent_assignment_firm_participants.RData',sep='')
patent_assignment_participants_file = paste(output_dir, 'patent_data/uspto_patent_assignment_participants.RData',sep='')
uspto_patent_assignment_firm_participants_file= paste(output_dir, 'patent_data/uspto_patent_assignment_firm_participants.RData',sep='')
NBER_CIQ_linked_patent_licensing_transaction_dataset_file = paste1(output_dir,  'merged_data/NBER_CIQ_linked_patent_licensing_transaction_dataset.RData')
uspto_licensing_file = paste(output_dir, 'patent_data/uspto_licensing_data.RData',sep='')
clegal_matched_dataset_lawyer_disambig_file = paste1(output_dir, 'patent_data/clegal_matched_dataset_w_lawyers_disambig.RData')



################
# III) Patent litigation data
################

  # this is the main one
patent_lit_cases_linked_dataset_all_sources_file = paste1(output_dir, 'patent_litigation/patent_lit_cases_linked_dataset_all_sources.RData')

patent_lit_cases_linked_dataset_file = paste1(output_dir,'patent_litigation/patent_lit_cases_linked_dataset.RData')
patent_docket_matched_litigants_file = paste1(output_dir, 'patent_litigation/patent_docket_matched_litigants.RData')
fjc_matched_litigants_file = paste1(output_dir, 'patent_litigation/fjc_matched_litigants.RData')
patent_litigation_fjc_participants_dataset_raw_file = paste1(output_dir, 'patent_litigation/patent_litigation_fjc_participants_dataset_raw.RData')
patent_litigation_fjc_participants_list_file = paste1(output_dir, 'patent_litigation/patent_litigation_fjc_participants_list.RData')
patent_lit_data_base_file = paste1(output_dir,'patent_litigation/patent_lit_data_base.RData')

################
# IV) WRDS based datasets
################

# Compustat firms master link of Compustat, CRSP, CIQ identifiers 

compustat_crsp_capitalIQ_link_data_file = paste(output_dir, 'firm_data/compustat_crsp_capitalIQ_link_data.RData',sep='')
NBER_based_dynamic_patent_assignments_file = paste(output_dir, 'merged_data/nber_dynamic_patent_assignments_w_ids.RData',sep='')
nber_firms_cleaned_file = paste(output_dir, "merged_data/nber_firms_w_ids_cleaned.RData",sep='')
nber_ciq_parent_crosswalk_final_file = paste(base_path, 'processed_data/merged_data/nber_ciq_parent_crosswalk.RData', sep='')
historical_compustat_data_file = paste(output_dir, 'firm_data/historical_compustat_data.RData', sep='')

################
# V) Matched datasets
################

######
# External datasets linked with IDs
#####

# NBER based patent assignments (with CIQ ids) - NBER 2010 dataset
nber_dynamic_patent_assignments_w_ids_file = paste(output_dir, 'merged_data/nber_dynamic_patent_assignments_w_ids.RData',sep='')

# KPSS data (with CIQ ids) 
kpss_data_linked_file = paste(output_dir, 'firm_data/kpss_patents_data_linked.RData',sep='')


######
# My linked patent data sets
#####


# CIQ subsidiary dataset matched to USPTO patents using algorithm


CIQsubsidiary_USPTO_matched_file = paste(output_dir, 'merged_data/capiq_patent_participant_matches_w_ids.RData',sep='')
CIQ_USPTO_matched_file = CIQsubsidiary_USPTO_matched_file 

# my matched NBER dataset - re-matched to USPTO patents using my algorithm

NBER_USPTO_matched_file=paste(output_dir, 'merged_data/rematched_nber_patent_participant_matches.RData',sep='')
NBER_USPTO_matched_w_ids_file=paste(output_dir, 'merged_data/rematched_nber_patent_participant_matches_w_ids.RData',sep='')

# USPTO matched assignees to NBER and subsidiary CIQ firms

uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file = paste1(output_dir, 'merged_data/uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee.RData')
uspto_dynamic_assignments_NBER_CIQ_linked_to_assignors_file = paste(output_dir,'merged_data/uspto_dynamic_assignments_NBER_CIQ_linked_to_assignors.RData', sep='' )

uspto_dynamic_licensing_NBER_CIQ_linked_both_owner_and_licensee_identified_file = paste1(output_dir, 'merged_data/uspto_dynamic_licensing_NBER_CIQ_linked_both_owner_and_licensee_identified.RData')
dynamic_patent_licensing_dataset_final_file = paste1(output_dir, 'patent_data/dynamic_patent_licensing_dataset.RData')

  #OLD: uspto_dynamic_assignments_NBER_CIQ_linked_file=  paste(output_dir,'uspto_dynamic_assignments_NBER_CIQ_linked.RData', sep='' )
  # uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file = uspto_dynamic_assignments_NBER_CIQ_linked_file
  #paste(output_dir,'uspto_dynamic_assignments_NBER_CIQ_linked.RData', sep='' )
#save(uspto_dynamic_assignments_NBER_CIQ_linked, file=paste(output_dir,'uspto_dynamic_assignments_NBER_CIQ_linked.RData', sep='' ))
#uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file


dynamic_patent_licensing_dataset_assignee_side_file = paste1(output_dir, 'merged_data/dynamic_patent_licensing_dataset_assignee_side.RData')
dynamic_patent_licensing_dataset_assignor_side_file = paste1(output_dir, 'merged_data/dynamic_patent_licensing_dataset_assignor_side.RData')

# complete USPTO matched ownership dataset combining all sources:

dynamic_patent_assignment_dataset_file = paste(output_dir, 'merged_data/dynamic_patent_assignment_dataset.RData', sep='')
dynamic_patent_assignment_dataset_final_file = paste1(output_dir, 'merged_data/dynamic_patent_assignment_dataset_final.RData')
# Map OLD to NEW dynamic_patent_assignment_dataset_file = uspto_dynamic_assignments_NBER_CIQ_linked

# complete USPTO matched licensing dataset combining all sources
dynamic_patent_licensing_dataset_file = paste1(output_dir, 'merged_data/dynamic_patent_licensing_dataset.RData')
dynamic_patent_licensing_dataset_final_file = paste1(output_dir, 'merged_data/dynamic_patent_licensing_dataset_final.RData')
#paste(output_dir,'uspto_dynamic_licensing_NBER_CIQ_linked.RData', sep='' ))


# Patent litigation data linked to firms
    # patent litigation dataset with firms linked to identifiers of Compustat, CIQ, disambiguated patent participants id numbers

patent_litigation_dataset_file = paste1(output_dir, 'merged_data/patent_litigation_dataset.RData')

patent_lit_names_cleaned_file = paste(output_dir, 'patent_litigation/patent_litigation_names_cleaned.RData',sep='')

patent_lit_data_linked_to_all_ids_file = paste(output_dir, 'patent_litigation/patent_lit_data_linked_to_all_ids.RData',sep='')


patent_descriptors_dataset_file = paste1(output_dir, 'patent_data/patent_descriptors_dataset.RData')

patent_lit_cases_linked_dataset_final_file = gsub('dataset', 'dataset_updated_NBER_matches',patent_lit_cases_linked_dataset_file)

########################################
# patent distances
########################################


  # distances of patents named on both sides of the litigation case

litigated_patents_and_linked_parents_both_sides_utility_distances_file = paste1(output_dir,'patent_similarity/litigated_patents_and_linked_parents_both_sides_utility_distances.RData')

# distances of patents in rivals portfolios at time of litigation case (and distance from plaintiffs name patents to all defendants)
patent_similarity_portfolio_rival_case_results_file = paste(output_dir, 'patent_similarity/patent_similarity_portfolio_rival_case_results.csv')

tfidf_file = paste(output_dir, 'patent_similarity/patent_tdm_tfidf_noLemma.RData', sep='')
patent_corpus_preLemma_file = paste(output_dir, 'patent_similarity/patent_corpus_preLemma.RData',sep='')


#patent_lit_data_w_distances_file = paste(output_dir,'merged_data/patent_litigation_w_distances.RData',sep='')

#############
# lawyers and CIQ litigation
#############

uspto_in_house_lawyers_disambiguated_file = paste(output_dir, 'patent_data/uspto_in_house_lawyers_disambiguated.RData',sep='')


#############
# datasets created from construct_final_datasets
###############


# patent data with statistics and consolidated parent pdpco snapshot

dynamic_patent_assignment_dataset_w_descriptors_file=paste1(output_dir,'final_datasets/dynamic_patent_assignment_dataset_w_descriptors.RData')

# patent data restricted to just the invented and including legal rep statistics

dynamic_patent_assignment_dataset_w_descriptors_invented_only_and_legal_stats_file = paste1(output_dir, 'final_datasets/dynamic_patent_assignment_dataset_w_descriptors_invented_only_and_legal_stats.R')

# firm x year patent portfolio statistics
firm_patent_portfolio_annual_file = paste1(output_dir, 'final_datasets/firm_patent_portfolio_annual.RData')
parent_patent_portfolio_annual_file = paste1(output_dir, 'final_datasets/parent_patent_portfolio_annual.RData')
parent_legal_investment_fixed_effects_annual_file = paste1(output_dir,'final_datasets/parent_legal_investment_fixed_effects_annual.RData')

    # By firm, year:
    # Age of portfolio (mean, median, 25, 75, 5, 95%)
    # Age |  acquired patents or invented patents
    # Number of patents in each class / subgroup
    # Number of patents allowed to lapse in year | acquired or invented
    # Number of patents not granted, number of in-force patents
    # Number of patents previously purchased, number of patents internally generated


# KPSS patent fitted values panel
kpss_patent_value_legal_rep_panel_file = paste1(output_dir, 'final_datasets/kpss_patent_value_legal_rep_panel.RData')



# merged with compustat

firm_patents_and_litigation_annual_dataset_file = paste1(output_dir,'final_datasets/firm_patents_and_litigation_annual_dataset.RData')
compustat_exec_data_licensing_file = paste1(output_dir,'final_datasets/compustat_exec_data_licensing.RData')
compustat_exec_data_litigation_file = paste1(output_dir,'final_datasets/compustat_exec_data_litigation.RData')
compustat_exec_data_patent_portfolio_file = paste1(output_dir, 'final_datasets/compustat_exec_data_patent_portfolio.RData')
compustat_exec_data_linked_file = paste1(output_dir, 'final_datasets/compustat_exec_data_linked.RData') # raw file 
compustat_exec_data_merged_with_annual_patent_portfolio_file = paste1(output_dir, 'final_datasets/compustat_exec_data_merged_with_annual_patent_portfolio.RData')
compustat_exec_data_w_patents_and_litigation_file= paste1(output_dir, 'final_datasets/compustat_exec_data_w_patents_and_litigation.RData')

############
# estimation data output / intermediate prep files
############

compustat_exec_data_w_product_rival_data_file = paste1(output_dir, 'final_datasets/compustat_exec_data_w_product_rival_data.RData')
#compustat_exec_data_w_product_rival_data_file = paste1(output_estimation_dataset_dir, 'compustat_exec_data_w_product_rival_data.RData')

  # compustat merged with product rival data and HHI
#MZ: commented out below two lines
# patent_lit_cases_linked_dataset_w_IP_by_case_both_sides_file = paste1(output_estimation_dataset_dir, 'patent_lit_cases_linked_dataset_w_IP_by_case_both_sides.RData')
# compustat_litigation_linked_dataset_file = paste1(output_estimation_dataset_dir, 'compustat_litigation_linked_dataset.RData')
  # patent litigation events merged to compustat based on year case terminated and summarized across years
    # created in combine_firm_balancesheets_with_annual_parent_litigation_stats
#MZ commented out
# compustat_patents_linked_dataset_file = paste1(output_estimation_dataset_dir, 'compustat_patents_linked_dataset.RData')




dependency_list = ls()

#################

#setwd(code_dir) Commented out July 3rd, 2021


# local: 
# code_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/Code/'
# data_dir ='/Users/mrempel/Dropbox/CultivatingMarketPower/data/FJCdata/'
# sub_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/CIQ/CapitalIQ_corporate_tree/'
# merger_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/CIQ/CapitalIQ_mergers/MA_downloads_by_date/'
# mergerSellof_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/CIQ/CapitalIQ_mergers/MA_and_sellofs_downloads_by_size/'
# capiq_legal_dev_patents_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/CIQ/CapitalIQ_patent_legal_developments/'
# patent_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/patents/uspto/'
# NBER_patents_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/patents/NBERpatents/'
# kpss_patent_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/patents/KPSS_data/'
# patentsView_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/patents/patentsView/'
# industry_def_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/supplemental_compustat/Industry_Definitions/'
# string_matching_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/Code/rempel_firm_string_matching_scripts/'
# HP_folder <- "/Users/mrempel/Dropbox/CultivatingMarketPower/data/supplemental_compustat/HobergPhillips/"
# output_dir = "/Users/mrempel/Dropbox/CultivatingMarketPower/data/output/"
# output_results_dir ="/Users/mrempel/Dropbox/CultivatingMarketPower/results/"
# output_data_results_dir ="/Users/mrempel/Dropbox/CultivatingMarketPower/data/final_results/"
# python_path = '/usr/local/bin/python3'
# create_python_env = F # set to true if running for the first time

# assume on SSCC server
# options(error=NULL)
# options(stringsAsFactors = F)
# base_path = '/project/patent_processing/CultivatingMarketPower/'
#   code_dir = '/project/patent_processing/CultivatingMarketPower/Code/'
#   data_dir ='/project/patent_processing/CultivatingMarketPower/data/FJCdata/'
#   sub_dir = '/project/patent_processing/CultivatingMarketPower/data/CIQ/CapitalIQ_corporate_tree/'
#   merger_dir = '/project/patent_processing/CultivatingMarketPower/data/CIQ/CapitalIQ_mergers/MA_downloads_by_date/'
#   mergerSellof_dir = '/project/patent_processing/CultivatingMarketPower/data/CIQ/CapitalIQ_mergers/MA_and_sellofs_downloads_by_size/'
#   capiq_legal_dev_patents_dir = '/project/patent_processing/CultivatingMarketPower/data/CIQ/CapitalIQ_patent_legal_developments/'
#   patent_dir = '/project/patent_processing/CultivatingMarketPower/data/patents/uspto/'
#   patentsView_dir = '/project/patent_processing/CultivatingMarketPower/data/patents/patentsView/'
#   NBER_patents_dir = '/project/patent_processing/CultivatingMarketPower/data/patents/NBERpatents/'
#   kpss_patent_dir = '/project/patent_processing/CultivatingMarketPower/data/patents/KPSS_data/'
#   HP_folder <- "/project/patent_processing/CultivatingMarketPower/data/supplemental_compustat/HobergPhillips/"
#   output_dir = "/project/patent_processing/CultivatingMarketPower/data/output/"
#   #string_matching_dir = '/project/patent_processing/CultivatingMarketPower/Code/rempel_firm_string_matching_scripts/'
#   output_data_results_dir ="/project/patent_processing/CultivatingMarketPower/data/final_results/"
# adding library path 
#.libPaths('/project/patent_processing/library/R')
#python_path = '/project/patent_processing/CultivatingMarketPower/bin/python'

#Sys.setenv(RETICULATE_PYTHON=python_path)
#python_library_path = '/project/patent_processing/library/python'


#########
# Doing everything remote via ssh instruction
#########

#linstat <- ssh_connect('mrempel@linstat.ssc.wisc.edu', verbose=2)
#out <- ssh_wait(linstat,command='source("master_cultivating_market_power.R")')

