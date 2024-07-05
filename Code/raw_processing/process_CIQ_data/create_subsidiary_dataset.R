#############################################################
# Generate subsidiary panel dataset using Capital IQ
#
# Combines CIQ corporate trees (a snapshot in time)
#   with M&A data to give dynamic subsidiary list
#
#     - note, have spinoff data, but only useful to know the date when a spinoff occurred
#

####################################  
# create Capital IQ corporate trees
#################################### 
  # I) consolidate the individual corporate trees and direct investments (snapshots) downloaded from CIQ
  # II) consolidate the merger data from CIQ

compile_prepare_subsidiary_data_from_scratch=T
if(!file.exists(corporate_tree_data_w_merger_transactions_file) | compile_prepare_subsidiary_data_from_scratch==T){
  
  print(paste('Preparing subsidiary data as following file not found: \n', corporate_tree_data_w_merger_transactions_file))
  
  # collate subsidiaries (corporate trees), direct investments and M&A data from CIQ
  
  source(paste1(code_dir, 'process_CIQ_data/prepare_subsidiary_data.R'))
  
  print(paste('Preparing subsidiary merger merged data done', corporate_tree_data_w_merger_transactions_file))
}

########################################################################  
# clean the Capital IQ firms w / subsidiary names
########################################################################

source(paste(code_dir, 'fuzzy_match_core_routines/preprocess_firm_name_strings_function.R',sep=''))
if (!exists("compile_fuzzy_string_clean_CIQ_from_scratch")) {
  compile_fuzzy_string_clean_CIQ_from_scratch <-T
}


if(!file.exists(subsidiary_dataset_file) | compile_fuzzy_string_clean_CIQ_from_scratch==T){
  
  print('starting cleaning of merger history corporate tree (CIQ firms)')
  load(corporate_tree_data_w_merger_transactions_file)
  merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>% ungroup()
  merger_history_complete_corporate_tree_dataset_2020 = preprocess_firm_name_strings(merger_history_complete_corporate_tree_dataset_2020, 'parent_name',remove_trailing_phrases=F)
  merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
    rename(parent_company_name = company_name)
  
  fwrite(merger_history_complete_corporate_tree_dataset_2020, file=intermediate_subsidiary_dataset_parent_processed_file)
  
  
  subsidiary_dataset=fread(intermediate_subsidiary_dataset_parent_processed_file)
    #intermediate_subsidiary_dataset_parent_processed_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_2020_parent_processed.csv', sep='')
  
  
  subsidiary_dataset = preprocess_firm_name_strings(subsidiary_dataset, 'Company.Name',remove_trailing_phrases=F)
  
  # make one manual modification for the microelectronics division of IBM which was acquired by global foundries and the string processing drops the micro electronic division part
  
  subsidiary_dataset$company_name[which(grepl('globalfoundries', subsidiary_dataset$parent_company_name) & grepl('international business machines',  subsidiary_dataset$company_name))] = 'international business machines microelectronics business'
  
  subsidiary_dataset = subsidiary_dataset %>% mutate(company_name = gsub('NA', '', company_name))

  save(subsidiary_dataset, file=subsidiary_dataset_file)
}
