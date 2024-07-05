########################################################################
# Pairwise fuzzy string matching
#   Github revert 2023
########################################################################

# Assumes that company names are supplied in 
#   A: paste(output_dir, 'company_names_to_match_series_A.csv',sep='')
#   B: paste(output_dir, 'company_names_to_match_series_B.csv',sep='')
# output: pairwise_fuzzy_output data frame

#write_csv(nber_dynamic_patent_assignments_w_ids, path=paste(output_dir, 'company_names_to_match_series_A.csv',sep=''))
#write_csv(uspto_patent_assignment_participants, path=paste(output_dir, 'company_names_to_match_series_B.csv',sep=''))

chkA = file.exists(paste(output_dir, 'company_names_to_match_series_A.csv',sep=''))
chkB = file.exists(paste(output_dir, 'company_names_to_match_series_B.csv',sep=''))

if(chkA==F | chkB==F){
  stop('missing inputs for pairwise fuzzy matching - e.g. company_names_to_match_series_A.csv')
}

require(reticulate)
python_path = '/usr/local/bin/python3'
use_python(python_path)
# provide input files for script


py_run_file("firm_string_matching_python_pairwise_script.py") # takes input company_names_to_match.csv and outputs

# read in output from python script
pairwise_fuzzy_output = read_csv(paste(output_dir, 'company_names_matched.csv',sep=''))

# determine matches based on similarity

pairwise_fuzzy_output = pairwise_fuzzy_output %>% 
  mutate(jw_dist = stringdist(left_side, right_side, method='jw')) %>%
  mutate(lv_dist = stringdist(left_side, right_side, method='lv')) %>%
  mutate(lcs_dist = stringdist(left_side, right_side, method='lcs')) %>%
  mutate(osa_dist = stringdist(left_side, right_side, method='osa')) %>%
  mutate(soundex_dist = stringdist(left_side, right_side, method='soundex')) %>%
  mutate(j_dist = stringdist(left_side, right_side, method='jaccard'))


pairwise_fuzzy_output = pairwise_fuzzy_output %>% arrange(-similarity, left_side)

pairwise_fuzzy_output = pairwise_fuzzy_output %>% distinct()

pairwise_fuzzy_output = pairwise_fuzzy_output %>% mutate(is_match = ifelse(similarity >=.85,1,0)) %>%
  mutate(is_match = ifelse(soundex_dist==0,1,is_match)) %>%
  mutate(is_match = ifelse(jw_dist < 0.03,1,is_match))  %>%
  mutate(is_match = ifelse(lv_dist ==1,1,is_match)) 


#uspto_new_nber_patent_participant_matches = uspto_new_nber_patent_participant_matches %>% filter(is_match==1)