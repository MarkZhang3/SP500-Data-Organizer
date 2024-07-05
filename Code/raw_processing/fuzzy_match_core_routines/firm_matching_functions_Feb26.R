##############################
# Match dataset to NBER, CIQ and USPTO patent participant firms
##############################

match_to_NBER_firms <- function(df, nber_firm_complete_list=NULL, keep_all_matches=F){
  # assume df has company_name and company_name_raw
  
  stopifnot('id' %in% names(df))
  stopifnot('df_name_raw' %in% names(df))
  stopifnot('company_name' %in% names(df))
  
  if(is.null(nber_firm_complete_list)){
    load(nber_dynamic_patent_assignments_w_ids_file)   
    nber_patent_firm_list = nber_dynamic_patent_assignments_w_ids %>% 
      select(companyname, pdpco, pdpass, permno, companyid, gvkey.x) %>% 
      distinct() %>% filter(!is.na(companyname))
    
    nber_patent_firm_list = preprocess_firm_name_strings(nber_patent_firm_list, 'companyname', remove_trailing_phrases = T)
    nber_patent_firm_list = nber_patent_firm_list %>% relocate(company_name)
    nber_patent_firm_list_complete = nber_patent_firm_list
    nber_patent_firm_list = nber_patent_firm_list %>% select(-gvkey.x, -firm_name) # have mixed types which python has memory issue with
  }
  
  
  # fuzzy match the nber firm companyname's with the company_name's in the dataset
  
  df_matches = fuzzy_string_matching_function(nber_patent_firm_list, 
                                              df, 
                                              keep_all_matches=keep_all_matches,use_1st_word_matches=T, 
                                              python_path = python_path)
  
  
  # merge the matches with the df raw firm names
  
  df_matches = left_join(df_matches,df, by=c('namesB'='company_name') )
  df_matches = df_matches %>% rename(df_name = namesB)
  #df_matches = df_matches %>% rename(df_firm_name = firm_name, df_name = namesB)
  
  # merge the matches with the 
  df_matches = left_join(df_matches,nber_patent_firm_list_complete, by=c('namesA'='company_name') )
  df_matches = df_matches %>% rename(nber_firm_name = firm_name, nber_name = namesA)
  
  # now resolve the multiple matches of NBER identifiers to participant_myid's
  
  df_matches = df_matches %>% 
    mutate(num_char_in_firm_name = nchar(df_firm_name), num_char_in_participant_name=nchar(df_name_raw)) %>% 
    mutate(lcs_sim = stringdist::stringsim(nber_firm_name, df_firm_name, method='lcs')) 
  
  # two dimensions of duplicates: 1) multiple matches of uspto firms to a given NBER firm, 2) multiple NBER firms to a given uspto firm
  # amongst the NBER firms
  #   - will select the best match for a given participant_id 
  
  df_matches = df_matches %>% ungroup() %>%  
    group_by(id) %>% arrange(desc(lcs_sim),num_char_in_firm_name) %>%
    filter(row_number()==1) %>% relocate(df_name, df_firm_name, nber_name, nber_firm_name, df_name_raw)
  
  
  df_matches = df_matches %>%
    mutate(df_match_source = 'NBER')
    # mutate(company_pdpco = ifelse(!is.na(as.character(pdpco)), paste('NBER_', as.character(pdpco), sep=''), NA_character_)) %>%
    # mutate(parent_pdpco = company_pdpco) %>%
    # mutate(my_pdpco = ifelse(!is.na(parent_pdpco), parent_pdpco, company_pdpco))
  print(paste('number of matches with NBER ', nrow(df_matches), ' for a share ',nrow(df_matches)/ nrow(df)))
  return(df_matches)
  
  
}

match_to_CIQ_firms <- function(df, subsidiary_firm_list=NULL, keep_all_matches=F, extra_similarity_cutoff = 0){
  # assume df has company_name and company_name_raw
  stopifnot('id' %in% names(df))
  stopifnot('df_name_raw' %in% names(df))
  stopifnot('company_name' %in% names(df))
  
  if(is.null(subsidiary_firm_list)){
    load(subsidiary_dataset_file) # assumes that map_dependencies.R is already called
    subsidiary_firm_list = subsidiary_dataset %>%
      mutate(parent_id = gsub('NA','', parent_id))%>% 
      select(company_name, Company.Name,parent_company_name,parent_name, parent_id,CIQ.Company.ID , consolidate_acquisition_date, X..Owned, Stake.Type, Relationship.Type,parent_ticker) %>%
      mutate(ticker_name = stringr::word(parent_ticker,2)) %>% mutate(ticker_name = tolower(ticker_name))
    subsidiary_firm_list = subsidiary_firm_list %>% mutate(firm_name = Company.Name)
  }
  
  # A) match company names
  
  df_matches = fuzzy_string_matching_function(subsidiary_firm_list,  df, keep_all_matches=F,use_1st_word_matches=T)
  
  # link back the matches to the subsidiary data and the uspto raw firm names
  
  df_matches = left_join(df_matches,df, by=c('namesB'='company_name') )#%>% rename(df_firm_name = firm_name)
  
  # this increases the number of observations substantially due to many uspto firms having the same string processed company_name with different participant_myids / firm_names
  
  df_matches_cnames = subsidiary_firm_list %>% 
    inner_join(df_matches, by=c('company_name'='namesA')) %>% mutate(CIQ_to_df_match_type='company') %>%  mutate(firm_name = Company.Name) 
  
  # now determine the best matches from this set
  #     re-ordering the participant matches 
  #     compute a string distance score between the (mostly) raw firm names from the two sources
  # taking the longest common string in the firm names
  
  df_matches_cnames = df_matches_cnames %>% 
    mutate(num_char_in_firm_name = nchar(df_firm_name), num_char_in_participant_name=nchar(df_name_raw)) %>% 
    mutate(lcs_sim = stringdist::stringsim(firm_name, df_firm_name, method='lcs')) %>% 
    arrange(company_name,desc(lcs_sim),num_char_in_firm_name)
  
  # two dimensions of duplicates: 1) multiple matches of uspto firms to a given CIQ firm, 2) multiple CIQ firms to a given uspto firm
  # amongst the CIQ firms
  #   - will select the best match for a given participant_id 
  
  df_matches_cnames = df_matches_cnames %>% ungroup() %>%  
    group_by(company_name, id) %>% arrange(company_name,desc(lcs_sim),num_char_in_firm_name) %>%
    filter(row_number()==1) %>% relocate(company_name, firm_name, df_firm_name, df_name_raw, Company.Name, parent_name)
  
  print(paste('number of matches with company names', nrow(df_matches_cnames), ' for a share ',nrow(df_matches_cnames)/ nrow(df)))
  
  
  
  # B) match parent names
  
  #############
  # Matching based on parent_names
  #############
  
  
  # do the same match with the parent names only
  
  subsidiary_dataset_subset = subsidiary_firm_list %>% 
    rename(company_company_name = company_name) %>% rename(company_name = parent_company_name) %>% mutate(firm_name = parent_name) %>%
    select(company_name, Company.Name,parent_name, parent_id,CIQ.Company.ID , consolidate_acquisition_date, X..Owned, Stake.Type, Relationship.Type,ticker_name, parent_ticker) 
  
  df_matches = fuzzy_string_matching_function(subsidiary_dataset_subset, 
                                                                                                   df, 
                                                                                                   keep_all_matches=F,use_1st_word_matches=T)
  
  
  # link back the matches to the subsidiary data and the uspto raw firm names
  
  df_matches = left_join(df_matches,df, by=c('namesB'='company_name') ) #%>% rename(df_firm_name = firm_name)
  
  # this increases the number of observations substantially due to many uspto firms having the same string processed company_name with different participant_myids / firm_names
  
  df_matches_pnames = subsidiary_firm_list %>% group_by(parent_company_name) %>% mutate(is_parent_ind = ifelse(CIQ.Company.ID==parent_id,1,0)) %>% 
    ungroup() %>%filter(is_parent_ind==1) %>%select(-is_parent_ind)  %>%
    inner_join(df_matches, by=c('parent_company_name'='namesA')) %>% mutate(CIQ_to_df_match_type='parent')%>%  mutate(firm_name = parent_name) 
  
  # now determine the best matches from this set
  #     re-ordering the participant matches 
  #     compute a string distance score between the (mostly) raw firm names from the two sources
  # taking the longest common string in the firm names
  
  df_matches_pnames = df_matches_pnames %>% 
    mutate(num_char_in_firm_name = nchar(df_firm_name), num_char_in_participant_name=nchar(df_name_raw)) %>% 
    mutate(lcs_sim = stringdist::stringsim(firm_name, df_firm_name, method='lcs')) 
  
  # two dimensions of duplicates: 1) multiple matches of uspto firms to a given CIQ firm, 2) multiple CIQ firms to a given uspto firm
  # amongst the CIQ firms
  #   - will select the best match for a given participant_id 
  
  df_matches_pnames = df_matches_pnames %>% ungroup() %>%  
    group_by(company_name, id) %>% arrange(company_name,desc(lcs_sim),num_char_in_firm_name) %>%
    filter(row_number()==1) 
  
  print(paste('number of matches with parent company names', nrow(df_matches_pnames), ' for a share ',nrow(df_matches_pnames)/ nrow(df)))
  
  
  
  
  
  
  # C) ticker matches
  
  subsidiary_dataset_subset = subsidiary_firm_list %>% 
    rename(company_company_name = company_name) %>% rename(company_name = ticker_name) %>% mutate(firm_name = ticker_name) %>%
    select(company_name, Company.Name,parent_name, parent_id,CIQ.Company.ID , consolidate_acquisition_date, X..Owned, Stake.Type, Relationship.Type,ticker_name, parent_ticker) 
  df_matches = fuzzy_string_matching_function(subsidiary_dataset_subset, 
                                                                                             df, 
                                                                                             keep_all_matches=F,use_1st_word_matches=T)
  
  
  
  
  
  
  # restrict similarity scores to be above 85 for the ticker due to the short strings
  df_matches = df_matches %>% filter(similarity >= .85)
  
  
  
  # link back the matches to the subsidiary data and the uspto raw firm names
  
  df_matches = left_join(df_matches,df, by=c('namesB'='company_name') ) #%>% rename(df_firm_name = firm_name)
  
  # this increases the number of observations substantially due to many uspto firms having the same string processed company_name with different participant_myids / firm_names
  
  df_matches_tnames = subsidiary_firm_list %>% group_by(parent_company_name) %>% mutate(is_parent_ind = ifelse(CIQ.Company.ID==parent_id,1,0)) %>% 
    ungroup() %>%filter(is_parent_ind==1) %>% select(-is_parent_ind) %>%
    inner_join(df_matches, by=c('ticker_name'='namesA')) %>% mutate(CIQ_to_df_match_type='ticker')%>%  mutate(firm_name = ticker_name) 
  
  # now determine the best matches from this set
  #     re-ordering the participant matches 
  #     compute a string distance score between the (mostly) raw firm names from the two sources
  # taking the longest common string in the firm names
  save.image(paste1(output_dir,'tmp/temporary_workspace_in_firm_matching_functions.RData'))
  
  df_matches_tnames = df_matches_tnames %>% 
    mutate(num_char_in_firm_name = nchar(df_firm_name), num_char_in_participant_name=nchar(df_name_raw)) %>% 
    mutate(lcs_sim = stringdist::stringsim(firm_name, df_firm_name, method='lcs')) 
  
  # two dimensions of duplicates: 1) multiple matches of uspto firms to a given CIQ firm, 2) multiple CIQ firms to a given uspto firm
  # amongst the CIQ firms
  #   - will select the best match for a given participant_id 
  
  df_matches_tnames = df_matches_tnames %>% ungroup() %>%  
    group_by(company_name, id) %>% arrange(company_name,desc(lcs_sim),num_char_in_firm_name) %>%
    filter(row_number()==1)  %>% ungroup()
  
  print(paste('number of matches with parent tickers', nrow(df_matches_tnames), ' for a share ',nrow(df_matches_tnames)/ nrow(df)))
 
  # combine the three source of matches
  
  df_all_matches_CIQ = rbind(df_matches_cnames, df_matches_pnames)
  df_all_matches_CIQ = rbind(df_all_matches_CIQ,df_matches_tnames)
  
  # take the best match for a given df id 
  df_all_matches_CIQ = df_all_matches_CIQ %>% 
    group_by(id) %>% arrange(df_name_raw,(CIQ_to_df_match_type), desc(lcs_sim), desc(similarity), jw_dist, lv_dist,lcs_dist, jw_dist_word1) %>% filter(row_number()==1) %>% ungroup()
  
  # rename process company names used in matching
  
  df_all_matches_CIQ = df_all_matches_CIQ %>% rename(capiq_company_name = company_name)%>% rename(df_name = namesB ) %>%
    select(capiq_company_name, df_name, parent_company_name,ticker_name, parent_ticker, CIQ.Company.ID, parent_id,consolidate_acquisition_date, 
           Company.Name,df_name, df_firm_name, id,similarity, lcs_sim,CIQ_to_df_match_type) %>%
    distinct()
  
  df_all_matches_CIQ = df_all_matches_CIQ %>%
    mutate(df_match_source = 'CIQ')
  
  
  # # create the pdpco variables
  # df_all_matches_CIQ = df_all_matches_CIQ %>%
  #   mutate(company_pdpco = ifelse(!is.na(CIQ.Company.ID), paste('CIQ_', CIQ.Company.ID, sep=''), NA_character_),
  #          parent_pdpco =ifelse(!is.na(parent_id), paste('CIQ_', parent_id, sep=''), NA_character_)) %>%
  #   mutate(company_pdpco = gsub('NA','', company_pdpco)) %>%
  #   mutate(parent_pdpco = gsub('NA','', parent_pdpco)) %>%
  #   mutate(my_pdpco = ifelse(!is.na(parent_pdpco), parent_pdpco, company_pdpco)) %>%
  #   mutate(parent_id = gsub('NA', '', parent_id)) %>%
  #   mutate(CIQ.Company.ID = gsub('NA', '', CIQ.Company.ID)) %>%
  #   mutate(parent_id = as.numeric(parent_id)) %>%
  #   mutate(CIQ.Company.ID = as.numeric(CIQ.Company.ID)) 
  
  return(df_all_matches_CIQ)
  
  
}

match_to_USPTO_firms <- function(df, uspto_unique_firm_names_w_ids=NULL, keep_all_matches=F){
  # assume df has company_name and company_name_raw
  stopifnot('id' %in% names(df))
  stopifnot('df_name_raw' %in% names(df))
  stopifnot('company_name' %in% names(df))
  
  
  if(is.null(uspto_unique_firm_names_w_ids)){
    if(file.exists(uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file)){
      load(uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file) # Feb 14, 2021
      uspto_dynamic_assignments_NBER_CIQ_linked = uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee %>% ungroup()  %>%
        select(company_name, my_pdpco,permno, parent_company_name, parent_pdpco, orig_name,participant_name, ee_name_disambig_group, ee_name_disambig_id, ee_name) %>% distinct()
    } else {
      load(uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee_file)
    }
    uspto_unique_firm_names_w_ids = uspto_dynamic_assignments_NBER_CIQ_linked_to_assignee %>% ungroup() %>% select(company_name, my_pdpco,permno, parent_company_name, parent_pdpco, orig_name,participant_name, ee_name_disambig_group, ee_name_disambig_id, ee_name) %>% distinct()
  }
  
  
  
  # fuzzy match the nber firm companyname's with the company_name's in the dataset
  
  df_matches = fuzzy_string_matching_function(uspto_unique_firm_names_w_ids, 
                                              df, 
                                              keep_all_matches=keep_all_matches,use_1st_word_matches=T, 
                                              python_path = python_path)
  
  
  # merge the matches with the df raw firm names
  
  df_matches = left_join(df_matches,df, by=c('namesB'='company_name') )
  df_matches = df_matches %>% rename(df_firm_name = firm_name, df_name = namesB)
  
  # merge the matches with the 
  df_matches = left_join(df_matches,uspto_unique_firm_names_w_ids, by=c('namesA'='company_name') )
  df_matches = df_matches %>% rename(uspto_firm_name = firm_name, uspto_name = namesA)
  
  # now resolve the multiple matches of NBER identifiers to participant_myid's
  
  df_matches = df_matches %>% 
    mutate(num_char_in_firm_name = nchar(df_firm_name), num_char_in_participant_name=nchar(df_name_raw)) %>% 
    mutate(lcs_sim = stringdist::stringsim(nber_firm_name, df_firm_name, method='lcs')) 
  
  # two dimensions of duplicates: 1) multiple matches of uspto firms to a given NBER firm, 2) multiple NBER firms to a given uspto firm
  # amongst the NBER firms
  #   - will select the best match for a given participant_id 
  
  df_matches = df_matches %>% ungroup() %>%  
    group_by(id) %>% arrange(desc(lcs_sim),num_char_in_firm_name) %>%
    filter(row_number()==1) %>% relocate(df_name, df_firm_name, uspto_name, uspto_firm_name, df_name_raw)
  
  df_matches = df_matches %>%
    mutate(df_match_source = 'USPTO')
  
  print(paste('number of matches with USPTO ', nrow(df_matches), ' for a share ',nrow(df_matches)/ nrow(df)))
  return(df_matches)
  
  
}

match_litigants_to_firms <- function(df, subsidiary_firm_list, nber_firm_list, uspto_unique_firm_names_w_ids){
  # type is parent, company
  source(paste(code_dir, 'fuzzy_match_core_routines/preprocess_firm_name_strings_function.R',sep=''))
  source(paste(code_dir, 'fuzzy_match_core_routines/fuzzy_string_matching_function.R',sep=''))
  
  # I) match with NBER
  
  tictoc::tic('matching NBER') # takes 5 minutes for litigation data
  nber_matches = match_to_NBER_firms(df, nber_firm_list)
  
  nber_unmatched = nber_matches %>% filter(is.na(companyname))
  nber_matches = nber_matches %>% filter(!is.na(companyname)) 
  toc()
  
  print('done matching NBER')
  
  # II) match with CIQ
  
  tictoc::tic('matching CIQ')# takes 5 minutes for litigation data
  CIQ_matches = match_to_CIQ_firms(df, subsidiary_firm_list)
  CIQ_unmatched = CIQ_matches %>% filter(is.na(Company.Name)) 
  CIQ_matches = CIQ_matches %>% filter(!is.na(Company.Name)) 
  toc()
  print('done matching CIQ')
  
  # III) match with USPTO
  
  tictoc::tic('matching USPTO')
  USPTO_matches = match_to_USPTO_firms(df, uspto_unique_firm_names_w_ids)
  toc()
  print('done matching USPTO')
  
  
  # Combine all matches
  
  all_matches = merge(df, nber_matches,by=c('id'), all=T)
  all_matches = merge(all_matches, CIQ_matches,by=c('id'), all=T)
  all_matches = merge(all_matches, USPTO_matches,by=c('id'), all=T)
  
  
  # now determine the best matches from this set
  #     re-ordering the participant matches 
  #     compute a string distance score between the (mostly) raw firm names from the two sources
  # taking the longest common string in the firm names
  
  all_matches = all_matches %>% 
    group_by(id) %>% arrange(df_name, desc(lcs_sim), desc(similarity), jw_dist, lv_dist,lcs_dist, jw_dist_word1) %>% filter(row_number()==1) %>% ungroup()
  
  # here work on fixing my_pdpco?
  
  return(all_matches)
  
}

