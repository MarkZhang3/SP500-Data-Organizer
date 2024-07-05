##############################################################################
# Clean and combine subsidiary / direct investment data / M&A data from Capital IQ
#     Output:
#       complete_corporate_tree_dataset_2020.RData
#
#
#   Last modified: October 14 2020
##############################################################################

#sub_dir = '/Users/mrempel/Dropbox/CultivatingMarketPower/data/subsidiary_data/CapitalIQ_corporate_tree'


####
# Collate subsidiary / direct investment list of firms
####
compile_trees_from_scratch = T # mz

if(!file.exists(corporate_tree_data_file) | compile_trees_from_scratch==T){ #corporate_tree_data_file = paste(output_dir, 'firm_data/complete_corporate_tree_dataset_2020.RData',sep='')

  print('processing corporate tree raw data')
  source(paste(code_dir, 'process_CIQ_data/process_capitalIQ_corporate_tree_raw_data.R',sep=''))  
		# gives current snapshot of parent subsidiary relationships (i.e. static as of time of downloading - 2024)
} else {
  print(paste0('loading corporate tree data', corporate_tree_data_file))
  load(corporate_tree_data_file)   #load(paste(output_dir, 'firm_data/complete_corporate_tree_dataset_2020.RData',sep='')) 

}
print('finished first if statement (prepare_subsidiary_data)')
####
# Collate merger / sell off transactions
####
##MZ added if statement
if (!exists("compile_mergers_from_scratch")) {
  compile_mergers_from_scratch <- T
}
if(!file.exists(CIQ_merger_dataset_file) | compile_mergers_from_scratch==T){ #CIQ_merger_dataset_file = paste(output_dir, 'firm_data/full_capiq_merger_dataset_2020.RData',sep='')

  print('processing capiq merger raw data')
  source(paste(code_dir, 'process_CIQ_data/process_capitalIQ_merger_data_raw.R', sep=''))
} else {
  #print('loading capiq merger data')
  #load(CIQ_merger_dataset_file)
  #load(paste(output_dir, 'firm_data/full_capiq_merger_dataset_2020.RData',sep=''))
  #full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% filter(!`Transaction Status`=='Cancelled') %>% filter(!`Transaction Status`=='Announced')
}


####################################
# Combine M&A data with the complete corporate tree 
####################################
if (!exists("compile_from_scratch")) {
  compile_from_scratch <- T
}
if(!file.exists(corporate_tree_data_w_merger_transactions_file)|compile_from_scratch==T){ #subsidiary_dataset_file = paste1(output_dir, 'firm_data/subsidiary_dataset.RData')

  source(paste(code_dir, 'process_CIQ_data/merge_capiq_mergers_and_corporate_tree_data.R',sep=''))  
		# creates the final Dataset 1 that we want - dynamic (i.e. over time/historical) parent subsidiary relationships
} else {
  load(corporate_tree_data_w_merger_transactions_file)
}



########
# Summarize / check output
#######

# 1) look for handful of notable transactions: e.g. Youtube
# 2) look for seeming contradictions (identified as acquisition, but labelled by Capiq as not, or vice versa)
# 3) look for duplicates (e.g. same merger date, but slightly different transaction details)

# make summary table
#   number of subsidiaries per firm
#   average growth in new subsidiaries each year
#   average size of subsidiary
#   average age of subsidiary (since point of acquisition)
#   share of subsidiaries acquired vs home-grown

#source(paste(code_dir, 'process_CIQ_data/summarize_subsidiary_data.R',sep=''))
