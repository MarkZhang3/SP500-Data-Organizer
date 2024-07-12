#########################################################################
# Clean up the consolidated corporate tree dataset parent names / tickers
#########################################################################
# Restored Aug 4, 2023 from Feb 20, 2021 commit ("finished fixing subsidiary code")


#load(paste(output_dir, 'raw_merger_history_complete_corporate_tree_dataset_2020.RData', sep=''))
#fix a few garbled firms
# PG:
tmp  = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(grepl('The Procter & Gamble Company (NYSE:PG)', Ultimate.Corporate.Parent, fixed=T)) %>%
  mutate(parent_name = Ultimate.Corporate.Parent) %>%
  mutate(parent_id = paste0(unique(parent_id), collapse=''))

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!grepl('The Procter & Gamble Company (NYSE:PG)', Ultimate.Corporate.Parent, fixed=T))

merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)

# EL:
tmp = merger_history_complete_corporate_tree_dataset_2020 %>% filter(stringr::str_detect(parent_name,'auder Comp')) %>%
  mutate(parent_name='The Estée Lauder Companies Inc')
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!stringr::str_detect(parent_name,'auder Comp'))
merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)


# Ericsson Telecommunications
tmp = merger_history_complete_corporate_tree_dataset_2020 %>% filter(grepl('Ericsson Tele', parent_name)) %>%
  mutate(parent_name='Ericsson Telecommunications')
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!grepl('Ericsson Tele', parent_name, fixed=T))
merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)

# fixe salesforcecom inc vs salesforce com inc
tmp = merger_history_complete_corporate_tree_dataset_2020 %>% filter(grepl('salesforce', parent_name)) %>%
  mutate(parent_name='salesforce com inc')
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!grepl('salesforce', parent_name, fixed=T))
merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)


# Taiwan Semiconductor Manufacturing Company LimiteC 2330
tmp = merger_history_complete_corporate_tree_dataset_2020 %>% filter(grepl('Taiwan Semiconductor', parent_name)) %>%
  mutate(parent_name='Taiwan Semiconductor Manufacturing Company Limited')
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!grepl('Taiwan Semiconductor', parent_name, fixed=T))
merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)



####
# Fill in parent name / ids for Merged Entities which are missing these details in the raw documents for some reason
####


merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>% 
  mutate(parent_ticker =stringr::str_extract(parent_name, '\\(.+\\)')) %>%
  relocate(parent_ticker) %>%
  mutate(parent_ticker = gsub('\\(', '', parent_ticker))%>% 
  mutate(parent_ticker = gsub('\\)', '', parent_ticker))

ticker_list = gsub('REIT', '', names(table(merger_history_complete_corporate_tree_dataset_2020$parent_ticker))) %>%
  str_extract('^.+:') 
ticker_list = gsub(':','', ticker_list)
ticker_list = gsub(' ','', ticker_list)
ticker_list = unique(ticker_list)

# loop over the ticker

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(parent_name_clean = parent_name)
for(i in ticker_list){
  merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
    mutate(parent_name_clean = ifelse(grepl(i, parent_name), stringr::str_sub(parent_name_clean, 1, regexpr(i, parent_name_clean) -1), parent_name_clean)) %>%
    mutate(parent_ticker = ifelse(grepl(i, parent_name), stringr::str_sub(parent_name,  regexpr(i, parent_name), regexpr(i, parent_name) + nchar(i)+4), parent_ticker))
}

#MZ changed
library(dplyr)
library(stringi)
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  relocate(parent_name_clean, parent_ticker, parent_name, parent_id) %>% arrange(parent_name) %>%
  mutate(parent_name_clean = stri_trans_general(parent_name_clean, "Latin-ASCII")) %>%  ## mz added
  mutate(parent_name_clean = gsub('[[:punct:]]', ' ', parent_name_clean)) %>% 
  mutate(parent_ticker = gsub(' I$', '', parent_ticker)) %>%
  mutate(parent_name_clean = gsub(' I$', '', parent_name_clean)) %>%
  mutate(parent_ticker = gsub('\\)', '', parent_ticker)) %>%
  mutate(parent_ticker = gsub(':', ' ', parent_ticker)) %>%
  mutate(parent_name_clean = str_squish(parent_name_clean))



# now group by firm name

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  group_by(parent_name_clean) %>%
  mutate(parent_id = paste0(unique(parent_id), collapse=''))

# fix Taiwan ticker
tmp = merger_history_complete_corporate_tree_dataset_2020 %>% filter(grepl('Taiwan Semiconductor', parent_name_clean)) %>%
  mutate(parent_ticker='NYSE TSM')
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  filter(!grepl('Taiwan Semiconductor', parent_name_clean, fixed=T))
merger_history_complete_corporate_tree_dataset_2020 = rbind(merger_history_complete_corporate_tree_dataset_2020, tmp)


#chk = merger_history_complete_corporate_tree_dataset_2020 %>% select(parent_name_clean, parent_ticker, parent_name, parent_id) %>% distinct()
#View(chk)

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  rename(parent_name_orig1=parent_name) %>% rename(parent_name = parent_name_clean)

# deal with duplicate companies 
#   sometimes virtually identical observations (some small text spacing difference)
#   target firm acquired multiple times can end up matched to the same firm 

multiple_acquired_firms = merger_history_complete_corporate_tree_dataset_2020 %>%
  group_by(parent_name, Company.Name) %>%
  arrange(desc(consolidate_acquisition_date)) %>%
  filter(!is.na(consolidate_acquisition_date)) %>%
  mutate(num_obs = n()) %>%
  filter(num_obs > 1)


save(multiple_acquired_firms, file=duplicate_acquired_firms_identified_in_tree_file)
  #duplicate_acquired_firms_identified_in_tree_file = paste(output_dir, 'firm_data/multiple_acquired_firms_corporate_tree_dataset_2020.RData', sep='')

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  group_by(parent_name, Company.Name) %>%
  arrange(desc(consolidate_acquisition_date)) %>%
  filter(row_number()==1) %>% ungroup()

#subsidiary_data_merger_history = merger_history_complete_corporate_tree_dataset_2020
#save(merger_history_complete_corporate_tree_dataset_2020,file=subsidiary_data_merger_history_file)

#subsidiary_data_merger_history_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_2020.RData', sep='')
  # subsidiary_data_merger_history_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_2020.RData', sep='')
  #corporate_tree_data_w_merger_transactions_base_file = paste(output_dir, 'firm_data/merger_history_complete_corporate_tree_dataset_2020.RData', sep='')
