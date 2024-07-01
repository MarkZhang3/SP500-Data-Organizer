########################################################################
# pre-process names for fuzzy matching function
#   reverted Github 2023
########################################################################
# replace Incorporated with Inc, etc. and use as indication of firm


pre_process_company_name_firm_suffixes <- function(dat, company_type_acronymns_table_file){
  chk = grepl('company_name', names(dat))
  if(chk==F){
    disp('Error in pre_process_company_name_firm_suffixes, data.frame provided is missing company_name column')
    stopifnot(chk==T)
  }
  
  
  # load table of common company acronymns / types
  company_type_acronymns_table = read_csv(company_type_acronymns_table_file)
  #company_type_acronymns_table = read_csv(paste(output_dir, 'company_type_acronym_table.csv',sep=''))
  
  
  company_type_acronymns_table=company_type_acronymns_table[,c(1,2)]
  names(company_type_acronymns_table) = c('company_type', 'acronym')
  company_type_acronymns_table = company_type_acronymns_table %>% mutate(company_type = tolower(company_type), acronym=tolower(acronym))
  
  for(i in 1:nrow(company_type_acronymns_table)){
    company_type_string = company_type_acronymns_table[i,1]
    company_type_acronym = company_type_acronymns_table[i,2]
    # replace the company_type_string with the acronym if available
    if(!is.na(company_type_acronym) & !is.na(company_type_string) ){
      nber_dynamic_patent_assignments_w_ids = nber_dynamic_patent_assignments_w_ids %>%
        mutate(company_name = gsub(company_type_string, company_type_acronym,company_name))
    }
  }
  # this line is to keep in line with outdated code (which had swapped & for '' rather than ' and ')
  dat =dat %>%
    mutate(company_name = gsub(' and ', ' ', company_name))
  
  return(dat)
  
}