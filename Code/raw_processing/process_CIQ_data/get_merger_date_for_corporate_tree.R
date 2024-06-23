################################################################
# Get merger date from corporate tree / M&A transaction data
################################################################

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_ind = ifelse(!is.na(Transactions) & grepl('Merger', Transactions),1,0)) %>%
  mutate(acquired_date = substr(Transactions, 7, 17))%>% 
  mutate(acquired_date = as.Date(acquired_date, format='%b-%d-%Y'))

acquired_size = sapply(merger_history_complete_corporate_tree_dataset_2020$Transactions, function(x){
  substr(x,  regexpr('Size ($mm): ', x, fixed=T) + 12, regexpr('Status', x)-3)
})

merger_history_complete_corporate_tree_dataset_2020$acquired_size = acquired_size
merger_history_complete_corporate_tree_dataset_2020$acquired_size = as.numeric(merger_history_complete_corporate_tree_dataset_2020$acquired_size)  

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_ind = ifelse(!is.na(`All Transactions Announced Date`), 1, acquired_ind)) 

################
# Identify additional acquisition dates from the business descriptions of the corporate tree
################

# Extract from business description strings merger date like
# "As of Month DD, YYYY, X was acquired"; "X was formerly a subsidiary.. As a result of Y's acquisition by Z on Month DD, YYYY", etc.

source(paste(code_dir, 'process_CIQ_data/process_capiq_business_description_for_merger_dates.R',sep=''))

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(merged_entity_ind = ifelse(Relationship.Type=='Merged Entity', 1,0)) %>%
  mutate(acquired_ind = ifelse(!is.na(consolidate_acquisition_date), 1, acquired_ind)) 

#save(merger_history_complete_corporate_tree_dataset_2020, file=paste(output_dir, 'raw_merger_history_complete_corporate_tree_dataset_2020.RData', sep=''))
