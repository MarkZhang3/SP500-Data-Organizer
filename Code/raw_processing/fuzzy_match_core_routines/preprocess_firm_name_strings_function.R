####################################
# String cleaning routine for firm names prior to fuzzy string matching
#####################################
# Last modified: Sept 3, 2023 (converted to data.table for speed improvements)
# 	Previously: Oct 14, 2020 
# Github 2023 reverted


##############################
# Utility functions
##############################

Require::Require("textclean", repo=repository)
# clean firm name
#   convert to lower case, remove punctuation
#   remove any bracketed details

clean_string_fun = function(x) {
  x %>%
    # make text lower case
    str_to_lower %>%
    # remove non-alphanumeric symbols
    str_replace_all("&", " and ") %>%
    str_replace_all("\\-", " ") %>%
    str_replace_all("/", " ") %>%
    str_replace(" \\(.*\\)", "") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_replace_all("\\s+", " ") # collapse multiple spaces
  #x = base::trimws(x, which='both')
}

load_company_acronym_table <- function(table_path=NULL){
  if(is.null(table_path)){
    output_dir=paste(code_dir,'fuzzy_match_core_routines/',sep='') # default is hard-coded path of the fuzzy_match_core_routines
    company_type_acronymns_table = read_csv(paste(code_dir,'fuzzy_match_core_routines/', 'company_type_acronym_table_2023.csv',sep=''))
  } else {
    company_type_acronymns_table = read_csv(table_path)
  }
  
  company_type_acronymns_table=company_type_acronymns_table[,c(1,2)]
  names(company_type_acronymns_table) = c('company_type', 'acronym')
  company_type_acronymns_table = company_type_acronymns_table %>% mutate(company_type = tolower(company_type), acronym=tolower(acronym))
  
}

# Added Dec 2023

load_common_business_association_types_table  <- function(table_path=NULL){

  if(is.null(table_path)){
	output_dir=paste(code_dir,'fuzzy_match_core_routines/',sep='') # default is hard-coded path of the fuzzy_match_core_routines
	common_business_association_types_table = read_csv(paste(code_dir,'fuzzy_match_core_routines/', 'common_business_association_types.csv',sep=''))
  } else {
	common_business_association_types_table = read_csv(table_path)
  }
  
  return(common_business_association_types_table)
}

remove_phrases = function(y, phrase, adjust=-1, trailing=T){
  stringpos = regexpr(phrase, y)
  
  out = y
  if(stringpos > 0 & !is.na(stringpos)){
    if(trailing==T){
      out = substr(y , 1, stringpos+adjust)    # adjust = -1 => take string just preceding the occurrence
    }  else {
      # drop leading phrase
      if(stringpos - adjust > 0){
        out = substr(y ,stringpos+adjust, nchar(y))
      } else {
        out = substr(y ,1, nchar(y))
      }
      
    }
  }
  
  
  return(out)
}

conv_to_ascii <- function(x) {
	to_ascii <- iconv(x, "latin1", "ASCII", sub="")
	return(to_ascii)
}


# USPTO trailing phrases to remove in participant names

remove_trailing_phrases_uspto <- function(x, include_inc=F){
  x = sapply(x, function(y) remove_phrases(y,phrase=' prior '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' merge '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' merges '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' a corp'))
  x = sapply(x, function(y) remove_phrases(y,phrase=' corp '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' a [a-z]+ corporation ')) # a swedish corporation
  x = sapply(x, function(y) remove_phrases(y,phrase=' a [a-z]+ corp ')) # a swedish corporation
  x = sapply(x, function(y) remove_phrases(y,phrase='formerly'))
  x = sapply(x, function(y) remove_phrases(y,phrase=' fka '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' f k a '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' dba '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' d b a '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' doing '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' also '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' personal rep'))
  x = sapply(x, function(y) remove_phrases(y,phrase=' as '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' an '))
  x = sapply(x, function(y) remove_phrases(y,phrase=' a division '))
  # remove addresses (sometimes with line-spacing they get lumped into name)
  #   e.g IBM inc 1234 Joe Street City State
  #     will assume that any 
  x = sapply(x, function(y) remove_phrases(y,phrase=' co [0-9]+', adjust=3)) # remove address
  x = sapply(x, function(y) remove_phrases(y,phrase=' box [0-9]+'))
  x = sapply(x, function(y) remove_phrases(y,phrase='po box [0-9]+'))
  
  
  
  x = sapply(x, function(y) remove_phrases(y,phrase='[0-9]+$', adjust=-1)) # remove trailing numbers
  x = sapply(x, function(y) remove_phrases(y,phrase='^and ', adjust=4, trailing=F)) # remove leading abd
  x = sapply(x, function(y) remove_phrases(y,phrase='^the ', adjust=-1, trailing=F)) # remove leading the
  x = sapply(x, function(y) remove_phrases(y,phrase=' the$', adjust=4, trailing=T)) # remove ending the
  x = sapply(x, function(y) remove_phrases(y,phrase='^[0-9]{2} interest ', adjust=11, trailing=F)) # remove  40% interest (but % already dropped)
  x = sapply(x, function(y) remove_phrases(y,phrase='^[0-9]{1} interest ', adjust=10, trailing=F)) # 
  
  return(x)
}

remove_leading_phrases_uspto <- function(x){
  x = sapply(x, function(y) remove_phrases(y,phrase='^research foundation of', adjust=-1,trailing=F)) # remove leading phrases
  x = sapply(x, function(y) remove_phrases(y,phrase='^trustees of', adjust=-1, trailing=F)) # remove leading phrases
  x = sapply(x, function(y) remove_phrases(y,phrase='^the ', adjust=-1, trailing=F)) # remove leading phrases
  
}

remove_trailing_phrases_uspto_abridged <- function(x){
  x = sapply(x, function(y) remove_phrases(y,phrase=' co [0-9]+', adjust=3)) # remove address
  return(x)
}

##############################
# master function
##############################

# abbreviate common firm identifiers,
#   remove punctuation, extra spacing, etc.
preprocess_firm_name_strings = function(df, firm_name_col='firm_name', remove_trailing_phrases=F, output_df=T, tmp_output_dir = temp_dir, tmp_output_filename='RS_tmp_df_preprocess'){

  # output_df = T => return data.table (default)
	# set output_df = F  for pbsapply parallel processing 
  # load table of common company acronymns / types
  company_type_acronymns_table = load_company_acronym_table()
  
  # if(!'data.table' %in% class(df)){
  	setDT(df)	# convert to data.table for speed
  # }

  if(!(firm_name_col =='firm_name')){
	df[,firm_name := get(firm_name_col)]
  }
 
  # do initial cleaning of strings (e.g. remove puncuation, make lower case, extra padding, sub "&" for "and")

  df[,company_name := clean_string_fun(firm_name),] # defined in line 18 in this script
  
  # i) identify firm observations and replace long strings  with acronyms (e.g. LIMITED with LTD)
  
  df[,is_firm := 0]
  df[,company_acronym := '']

  for(i in 1:nrow(company_type_acronymns_table)){
    company_type_string = as.character(company_type_acronymns_table[i,1])
    company_type_acronym = as.character(company_type_acronymns_table[i,2])
    if(!is.na(company_type_string) ){
		# identify participant is a firm if it has company string in name
		df[,is_firm:= fifelse(company_name %like% paste(' ', company_type_string, ' ', sep=''),1, is_firm)]
		df[,is_firm:= fifelse(company_name %like% paste(' ', company_type_string, '$', sep=''),1, is_firm)]
		df[,is_firm:= fifelse(company_name %like% paste('^', company_type_string, ' ', sep=''),1, is_firm)]

		# df[,is_firm := fifelse(grepl(paste(' ', company_type_string, ' ', sep=''), company_name),1, is_firm)]
		# df[,is_firm := fifelse(grepl(paste(' ', company_type_string, '$', sep=''), company_name),1, is_firm)]
		df[,company_acronym := fifelse(company_name %like% paste(' ', company_type_string, ' ', sep=''),company_type_acronym, company_acronym)]
		# df[,company_acronym := fifelse(grepl(paste(' ', company_type_string, ' ', sep=''), company_name),company_type_acronym, company_acronym)]
		df[,company_acronym := fifelse(company_name %like% paste(' ', company_type_string, '$', sep=''),company_type_acronym, company_acronym)]
		# df[,company_acronym := fifelse(grepl(paste(' ', company_type_string, '$', sep=''), company_name),company_type_acronym, company_acronym)]
		df[,company_acronym := fifelse(company_name %like% paste('^', company_type_string, ' ', sep=''),company_type_acronym, company_acronym)]

	}
		

    if(!is.na(company_type_acronym) ){
      	# identify participant is a firm if it has a company acronym in name 
		# df[,is_firm := fifelse(grepl(paste(' ', company_type_acronym, ' ', sep=''), company_name),1, is_firm)]
		# df[,is_firm := fifelse(grepl(paste(' ', company_type_acronym, '$', sep=''), company_name),1, is_firm)]
		# df[,is_firm := fifelse(grepl(paste('^', company_type_acronym, ' ', sep=''), company_name),1, is_firm)]   
		df[,is_firm := fifelse(company_name 	%like% paste(' ', company_type_acronym, ' ', sep=''),1, is_firm)]
		df[,is_firm := fifelse(company_name 	%like% paste(' ', company_type_acronym, '$', sep=''),1, is_firm)]
		df[,is_firm := fifelse(company_name 	%like% paste('^', company_type_acronym, ' ', sep=''),1, is_firm)]
		# rewrite above four lines using %like% operator
		df[,company_acronym := fifelse(company_name %like% paste(' ', company_type_acronym, ' ', sep=''),company_type_acronym, company_acronym)]
		df[,company_acronym := fifelse(company_name %like% paste(' ', company_type_acronym, '$', sep=''),company_type_acronym, company_acronym)]	
		df[,company_acronym := fifelse(company_name %like% paste('^', company_type_acronym, ' ', sep=''),company_type_acronym, company_acronym)]
	}

		# df[,company_acronym := fifelse(grepl(paste(' ', company_type_acronym, ' ', sep=''), company_name),company_type_acronym, company_acronym)]
		# df[,company_acronym := fifelse(grepl(paste(' ', company_type_acronym, '$', sep=''), company_name),company_type_acronym, company_acronym)]
		# df[,company_acronym := fifelse(grepl(paste('^', company_type_acronym, ' ', sep=''), company_name),company_type_acronym, company_acronym)]

    # replace the company_type_string with the acronym if available
    if(!is.na(company_type_acronym) & !is.na(company_type_string) ){
	
		# replace company name string if it is in the middle of the string
		df[,company_name := gsub(paste(' ', company_type_string, ' ', sep=''),paste(' ', company_type_acronym, ' ', sep=''),company_name)]
		# replace company name string if it is at the end of the string
		string1 = paste0(' ', company_type_string, '$', sep='')
		string2=paste0(' ', company_type_acronym, '', sep='')
		df[,company_name := gsub(string1, string2,company_name)]
		# replace company name string if it is at the beginning of the string
		string1 = paste0('^', company_type_string, ' ', sep='')
		string2=paste0(company_type_acronym, ' ', sep='')
		df[,company_name := gsub(string1, string2,company_name)]	

    }
    
    
  } # end of loop over company_type_acronymns_table
 
  # identify alternative firm name (doing business as or formerly known as)

	df[,dba_ind := 0]
	df[,fka_ind := 0]
	df[,merge_ind := 0]
	df[,division_of_ind := 0]
	df[,dba_ind := fifelse(company_name %like% ' dba ',1, dba_ind)]
	df[,dba_ind := fifelse(company_name %like% ' d b a ',1, dba_ind)]
	df[,dba_ind := fifelse(company_name %like% ' doing business as ',1, dba_ind)]
	df[,dba_ind := fifelse(company_name %like% ' also ',1, dba_ind)]
	df[,dba_ind := fifelse(company_name %like% ' operating as ',1, dba_ind)]
	df[,dba_ind	:= fifelse(company_name %like% ' operating under ',1, dba_ind)]
	df[,fka_ind := fifelse(company_name %like% ' fka ',1, fka_ind)]
	df[,fka_ind := fifelse(company_name %like% ' f k a ',1, fka_ind)]
	df[,fka_ind := fifelse(company_name %like% ' formerly known as ',1, fka_ind)]
	df[,fka_ind := fifelse(company_name %like% ' formerly ',1, fka_ind)]
	df[,merge_ind := fifelse(company_name %like% ' merge ',1, merge_ind)]
	df[,merge_ind := fifelse(company_name %like% ' merges ',1, merge_ind)]
	df[,merge_ind := fifelse(company_name %like% ' merger ',1, merge_ind)]
	df[,merge_ind := fifelse(company_name %like% ' merged ',1, merge_ind)]
	df[,merge_ind := fifelse(company_name %like% ' merging ',1, merge_ind)]
	df[,division_of_ind	:= fifelse(company_name %like% ' division of ',1, division_of_ind)]
	df[,division_of_ind	:= fifelse(company_name %like% ' division ',1, division_of_ind)]
	df[,division_of_ind	:= fifelse(company_name %like% ' div ',1, division_of_ind)]

	# remove string preceding dba 
	
	df[,dba_name1 := strsplit(company_name, ' dba ')[[1]][2], by=1:nrow(df)]
	df[,dba_name2 := strsplit(company_name, ' d b a ')[[1]][2], by=1:nrow(df)]
	df[,dba_name3 := strsplit(company_name, ' doing business as ')[[1]][2], by=1:nrow(df)]
	df[,dba_name4 := strsplit(company_name, ' also ')[[1]][2], by=1:nrow(df)]
	df[,dba_name5 := strsplit(company_name, ' operating as ')[[1]][2], by=1:nrow(df)]
	df[,dba_name6 := strsplit(company_name, ' operating under ')[[1]][2], by=1:nrow(df)]
	# remove string preceding fka 

	df[,fka_name1 := strsplit(company_name, ' fka ')[[1]][2], by=1:nrow(df)]
	df[,fka_name2 := strsplit(company_name, ' f k a ')[[1]][2], by=1:nrow(df)]
	df[,fka_name3 := strsplit(company_name, ' formerly known as ')[[1]][2], by=1:nrow(df)]
	df[,fka_name4 := strsplit(company_name, ' formerly ')[[1]][2], by=1:nrow(df)]

	# remove string preceding merge 

	df[,merge_name1 := strsplit(company_name, ' merge ')[[1]][2], by=1:nrow(df)]
	df[,merge_name2 := strsplit(company_name, ' merges into')[[1]][2], by=1:nrow(df)]
	df[,merge_name3 := strsplit(company_name, ' merger ')[[1]][2], by=1:nrow(df)]
	df[,merge_name4 := strsplit(company_name, ' merged into')[[1]][2], by=1:nrow(df)]
	df[,merge_name5 := strsplit(company_name, ' merging into ')[[1]][2], by=1:nrow(df)]
	df[,merge_name6 := strsplit(company_name, ' merging ')[[1]][2], by=1:nrow(df)]
	df[,merge_name7 := strsplit(company_name, ' merged with')[[1]][2], by=1:nrow(df)]
	df[,merge_name8 := strsplit(company_name, ' merged ')[[1]][2], by=1:nrow(df)]
	df[,merge_name9 := strsplit(company_name, ' merges ')[[1]][2], by=1:nrow(df)]

	# remove string preceding division of 

	df[	,division_of_name1 := strsplit(company_name, ' division of ')[[1]][2], by=1:nrow(df)]
	df[	,division_of_name2 := strsplit(company_name, ' division ')[[1]][2], by=1:nrow(df)]
	df[	,division_of_name3 := strsplit(company_name, ' div ')[[1]][2], by=1:nrow(df)]

	# consolidate dba name 
	df[,dba_name := '']	
	df[,dba_name := fifelse(!is.na(dba_name1), dba_name1, dba_name)]
	df[,dba_name := fifelse(!is.na(dba_name2) & is.na(dba_name), dba_name2, dba_name)]
	df[,dba_name := fifelse(!is.na(dba_name3) & is.na(dba_name), dba_name3, dba_name)]
	df[,dba_name := fifelse(!is.na(dba_name4) & is.na(dba_name), dba_name4, dba_name)]
	df[,dba_name := fifelse(!is.na(dba_name5) & is.na(dba_name), dba_name5, dba_name)]

	# consolidate fka name
	df[,fka_name := '']
	df[,fka_name := fifelse(!is.na(fka_name1), fka_name1, fka_name)]
	df[,fka_name := fifelse(!is.na(fka_name2) & is.na(fka_name), fka_name2, fka_name)]
	df[,fka_name := fifelse(!is.na(fka_name3) & is.na(fka_name), fka_name3, fka_name)]
	df[,fka_name := fifelse(!is.na(fka_name4) & is.na(fka_name), fka_name4, fka_name)]
	
	# consolidate merge name 
	df[,merge_name := '']
	df[,merge_name := fifelse(!is.na(merge_name1), merge_name1, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name2) & is.na(merge_name), merge_name2, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name3) & is.na(merge_name), merge_name3, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name4) & is.na(merge_name), merge_name4, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name5) & is.na(merge_name), merge_name5, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name6) & is.na(merge_name), merge_name6, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name7) & is.na(merge_name), merge_name7, merge_name)]
	df[,merge_name := fifelse(!is.na(merge_name8) & is.na(merge_name), merge_name8, merge_name)]
	
	# consolidate division name 
	df[,division_of_name := '']
	df[,division_of_name := fifelse(!is.na(division_of_name1), division_of_name1, division_of_name)]
	df[,division_of_name := fifelse(!is.na(division_of_name2) & is.na(division_of_name), division_of_name2, division_of_name)]
	df[,division_of_name := fifelse(!is.na(division_of_name3) & is.na(division_of_name), division_of_name3, division_of_name)]

	# drop the temporary columns
	
	df[,dba_name1:=NULL]
	df[,dba_name2:=NULL]
	df[,dba_name3:=NULL]
	df[,dba_name4:=NULL]
	df[,dba_name5:=NULL]
	df[,fka_name1:=NULL]
	df[,fka_name2:=NULL]
	df[,fka_name3:=NULL]
	df[,fka_name4:=NULL]
	
	df[,merge_name1 := NULL]
	df[,merge_name2 := NULL]
	df[,merge_name3 := NULL]
	df[,merge_name4 := NULL]
	df[,merge_name5 := NULL]
	df[,merge_name6 := NULL]
	df[,merge_name7 := NULL]
	df[,merge_name8 := NULL]
	df[,merge_name9 := NULL]

	df[,division_of_name1 := NULL]
	df[,division_of_name2 := NULL]
	df[,division_of_name3 := NULL]

	# remove the dba / fka string from the dba_name / fka_name 
	
	df[,dba_name:=gsub(' dba ', '', dba_name)]
	df[,dba_name:=gsub(' d b a ', '', dba_name)]
	df[,dba_name:=gsub(' doing business as ', '', dba_name)]
	df[,fka_name:=gsub(' fka ', '', fka_name)]
	df[,fka_name:=gsub(' f k a ', '', fka_name)]
	df[,fka_name:=gsub(' formerly known as ', '', fka_name)]
	df[,fka_name:=gsub(' formerly ', '', fka_name)]
	df[,merge_name:=gsub(' merges into ', '', merge_name)]
	df[,merge_name:=gsub(' merger ', '', merge_name)]
	df[,merge_name:=gsub(' merged into ', '', merge_name)]
	df[,merge_name:=gsub(' merging into ', '', merge_name)]
	df[,merge_name:=gsub(' merged ', '', merge_name)]
	df[,merge_name:=gsub(' merges ', '', merge_name)]
	df[,merge_name:=gsub(' merging into ', '', merge_name)]
	df[,merge_name:=gsub(' merged with ', '', merge_name)]
	df[,merge_name:=gsub(' merge ', '', merge_name)]
	df[,merge_name:=gsub(' merging ', '', merge_name)]
	df[,merge_name:=gsub(' merged ', '', merge_name)]
	df[,merge_name:=gsub(' merges ', '', merge_name)]
	df[,division_of_name:=gsub(' division of ', '', division_of_name)]
	df[,division_of_name:=gsub(' division ', '', division_of_name)]
	df[,division_of_name:=gsub(' div ', '', division_of_name)]


	# identify trustees of / research foundation of 

	df[,trustees_of_ind := 0]
	df[,research_foundation_of_ind := 0]
	df[,trustees_of_ind := fifelse(company_name %like% ' trustees of ',1, trustees_of_ind)]
	df[,research_foundation_of_ind := fifelse(company_name %like% ' research foundation of ',1, research_foundation_of_ind)]
	df[,trustees_of_name := strsplit(company_name, ' trustees of ')[[1]][2], by=1:nrow(df)]
	df[,research_foundation_of_name := strsplit(company_name, ' research foundation of ')[[1]][2], by=1:nrow(df)]
	df[,trustees_of_name := fifelse(!is.na(trustees_of_name), trustees_of_name, '')]
	df[,research_foundation_of_name := fifelse(!is.na(research_foundation_of_name), research_foundation_of_name, '')]
	df[,trustees_of_name := gsub(' trustees of ', '', trustees_of_name)]
	df[,research_foundation_of_name := gsub(' research foundation of ', '', research_foundation_of_name)]

	# identify university or institute

	df[,university_ind := 0]
	df[,institute_ind := 0]
	df[,center_ind := 0]
	df[,alumni_ind := 0]
	df[,foundation_ind :=0]
	df[,admin_ind := 0]
	df[,academy_of_ind := 0]
	df[,ecole_ind := 0]
	df[,university_ind := fifelse(company_name %like% ' universit',1, university_ind)]
	df[,ecole_ind	:= fifelse(company_name %like% ' ecole ',1, ecole_ind)]
	df[,institute_ind := fifelse(company_name %like% ' institut',1, institute_ind)]
	df[,center_ind := fifelse(company_name %like% ' center',1, center_ind)]
	df[,alumni_ind := fifelse(company_name %like% ' alumni ',1, alumni_ind)]
	df[,academy_of_ind := fifelse(company_name %like% ' academy of ',1, academy_of_ind)]
	df[,foundation_ind := fifelse(company_name %like% ' foundation ',1, foundation_ind)]
	df[,admin_ind := fifelse(company_name %like% ' admin ',1, admin_ind)]
	df[,university_name := fifelse(university_ind==1, firm_name, '')]
	df[,institute_name := fifelse(institute_ind==1, firm_name, '')]
	df[,center_name := fifelse(center_ind==1, firm_name, '')]
	df[,alumni_name := fifelse(alumni_ind==1, firm_name, '')]
	df[,ecole_name := fifelse(ecole_ind==1, firm_name, '')]
	df[,foundation_name := fifelse(foundation_ind==1, firm_name, '')]
	df[,academy_of_name := fifelse(academy_of_ind==1, firm_name, '')]

	# df[,admin_name := fifelse(admin_ind==1, firm_name, '')]

	# consolidate into one non-profit research 
	df[,non_profit_research_name := fifelse(university_ind==1, firm_name, '')]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & institute_ind==1, firm_name, non_profit_research_name)]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & center_ind==1, firm_name, non_profit_research_name)]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & alumni_ind==1, firm_name, non_profit_research_name)]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & foundation_ind==1, firm_name, non_profit_research_name)]
	# df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & admin_ind==1, firm_name, non_profit_research_name)]))]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & academy_of_ind==1, firm_name, non_profit_research_name)]
	df[,non_profit_research_name := fifelse(is.na(non_profit_research_name) & ecole_ind==1, firm_name, non_profit_research_name)]


	# identify government 

	# identify individual professionals PhD, Dr, MD, 
	df[,phd_ind := 0]
	df[,dr_ind := 0]
	df[,md_ind := 0]
	df[,prof_ind := 0]
	df[,phd_ind := fifelse(firm_name %like% ' phd ',1, phd_ind)]
	df[,dr_ind := fifelse(firm_name %like% ' dr ',1, dr_ind)]
	df[,md_ind := fifelse(firm_name %like% ' md ',1, md_ind)]
	df[,prof_ind := fifelse(firm_name %like% ' prof ',1, prof_ind)]
	df[,phd_name := strsplit(firm_name, ' phd ')[[1]][2], by=1:nrow(df)]
	df[,prof_name := strsplit(firm_name, ' prof ')[[1]][2], by=1:nrow(df)]
	df[,dr_name := strsplit(firm_name, ' dr ')[[1]][2], by=1:nrow(df)]
	df[,md_name := strsplit(firm_name, ' md ')[[1]][2], by=1:nrow(df)]
	# consolidate professional names 
	df[,professional_name := fifelse(!is.na(phd_name), phd_name, '')]
	df[,professional_name := fifelse(!is.na(prof_name) & is.na(professional_name), prof_name, professional_name)]
	df[,professional_name := fifelse(!is.na(dr_name) & is.na(professional_name), dr_name, professional_name)]
	df[,professional_name := fifelse(!is.na(md_name) & is.na(professional_name), md_name, professional_name)]


	# identify partial interest by leading number and percent sign 
	df[,partial_interest_ind := 0]
	df[,partial_interest_ind := fifelse(firm_name %like% '[0-9]{2}%',1, partial_interest_ind)]
	df[,partial_interest_amount := str_extract(firm_name, '[0-9]{2}%'), by=1:nrow(df)]

  # remove leading phrases 'trustees of' and 'research foundation of', 'the', etc.
  

	df[,company_name:=remove_leading_phrases_uspto(company_name),]
	df[,fka_name_clean := remove_leading_phrases_uspto(fka_name),]
	df[,dba_name_clean := remove_leading_phrases_uspto(dba_name),]
	df[,merge_name_clean := remove_leading_phrases_uspto(merge_name),]
	df[,division_of_name_clean := remove_leading_phrases_uspto(division_of_name),]

  # look for overly long strings (firm name with address pasted to it)
  #   if characters more than 30, look for 3+ nuumeric digits to indicate an address

	df[,num_characters := nchar(company_name),]
	df[,num_characters_greater_than_30_ind := fifelse(num_characters>30,1,0) ]

	fwrite(df, file=paste(tmp_output_dir, tmp_output_filename, "_pre.csv", sep=""))
  # df_old = df
  # rm(df)
  # df = read_csv(file=paste(output_dir, 'tmp/tmp_df_pre.csv', sep=''))
  # setDT(df)
 
	# remove string following 3-4 digit numbers if the string is too long
	df[,company_name_tmp:= remove_phrases(company_name, '[0-9]{3,4} ', trailing=T), by=1:nrow(df)]
	df[,company_name:=fifelse(num_characters_greater_than_30_ind ==1,company_name_tmp ,company_name),]
	df[,company_name:=stringr::str_trim(company_name),]

	# remove any leading punctuation in firm name (this is just minimal cleanup of the original firm name for comparison after) 
	df[,raw_firm_name := firm_name]
	df[,firm_name:=str_replace(firm_name,'^[:punct:]+',''),]
	  
  
  # remove trailing / leading whitespace and convert to ascii 
   
  df[,company_name:=conv_to_ascii(company_name),]
  df[,company_name:=paste(company_name, ' ', sep=''),]


  if(remove_trailing_phrases==T){
    # remove trailing phrases 
    #   i) remove trailing strings after corp, inc, etc.
    
    df[,company_name:=remove_trailing_phrases_uspto(company_name),]
   
	# remove acronyms from the string but have separate column for them 

    for(i in 1:nrow(company_type_acronymns_table)){
      #print(paste('acronym type ', i))
      company_type_string = as.character(company_type_acronymns_table[i,1])
      company_type_acronym = as.character(company_type_acronymns_table[i,2])
      if(!is.na(company_type_acronym) ){
        
        phrase = paste(' ', company_type_acronym, ' ', sep='')
        df[,company_name:=remove_phrases(company_name, phrase), by=1:nrow(df)]
        phrase = paste(' ', company_type_acronym, '$', sep='')
        df[,company_name:=remove_phrases(company_name, phrase), by=1:nrow(df)]
        phrase = paste('^', company_type_acronym, ' ', sep='')
        df[,company_name:=remove_phrases(company_name, phrase, trailing = F), by=1:nrow(df)]
      }
      
    }
    
    ###
    # do a final loop of typos in the firm legal identifiers
    ###

    df[,last_word :=stringr::word(company_name, -1),]
    df[,num_words :=sapply(strsplit(company_name,' '),length ),]
    df[,first_words :=paste(stringr::word(company_name,start=1, end=num_words-1), sep=' '),]
    df[,first_word :=stringr::word(company_name, 1),]
    df[,last_words :=paste(stringr::word(company_name,start=2, end=num_words), sep=' '),]
    df[,first_words :=fifelse(is.na(first_words), '', first_words),]
    df[,last_words :=fifelse(is.na(last_words), '', last_words),]
    df[,first_words:=gsub('NA', '', first_words),]
    df[,last_words:=gsub('NA', '', last_words),]
    df[,last_word:=gsub('NA', '', last_word),]
    df[,first_word:=gsub('NA', '', first_word),]
    df[,last_word:= as.character(last_word),]
    df[,last_word_ind:= 0,]
    df[,max_last_word_dist:= 0,]
    df[,last_word_dist:= 0,]

	fwrite(df, file=paste(tmp_output_dir, tmp_output_filename, ".csv", sep=""))

    #df = as.data.table(df)
    # fwrite(df, file=paste(output_dir, 'tmp/tmp_df.csv', sep=''))
	# fwrite(df, file=paste(tmp_output_dir, tmp_output_filename, ".csv", sep=""))
  # df_old = df

    # df_old = df
    # rm(df)
    # df = read_csv(file=paste(output_dir, 'tmp/tmp_df.csv', sep=''))
    # setDT(df)

	
    for(i in 1:nrow(company_type_acronymns_table)){
      company_type_string = as.character(company_type_acronymns_table[i,1])
      company_type_acronym = as.character(company_type_acronymns_table[i,2])
      if(!is.na(company_type_string)){
        
        df[, last_word_dist := stringsim(as.character(last_word), as.character(company_type_string), method='lv'),]
        df[,max_last_word_dist :=max(max_last_word_dist, last_word_dist,na.rm=T), by=1:nrow(df)]
        df[,last_word_ind:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,1,last_word_ind),]
        df[,last_word:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,'',last_word),]
		df[,company_acronym:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,company_type_acronym,company_acronym),]
      }
      
      
      if(!is.na(company_type_acronym)){
        df[, last_word_dist := stringsim(as.character(last_word), as.character(company_type_acronym), method='lv')]
        df[,max_last_word_dist :=max(max_last_word_dist, last_word_dist,na.rm=T), by=1:nrow(df)]
        df[,last_word_ind:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,1,last_word_ind),]
        df[,last_word:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,'',last_word),]
		df[,company_acronym:=fifelse(!is.na(last_word_dist)& last_word_dist>.8,company_type_acronym,company_acronym),]
      }
      
      
    }
    
    # do the same with the first word
    df[,company_name2:=stringr::str_trim(paste(first_words, last_word, sep=' ')),]
    df[,max_first_word_dist:=0,]
    df[,first_word_ind:=0,]
    
    
    for(i in 1:nrow(company_type_acronymns_table)){
      company_type_string = as.character(company_type_acronymns_table[i,1])
      company_type_acronym = as.character(company_type_acronymns_table[i,2])
      
      
      
      if(!is.na(company_type_string)){
        df[, first_word_dist := stringsim(as.character(first_word), as.character(company_type_string), method='lv')]
        df[,max_first_word_dist :=max(max_first_word_dist, first_word_dist,na.rm=T), by=1:nrow(df)]
        df[,first_word_ind:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,1,first_word_ind),]
        df[,first_word:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,'',first_word),]
		df[,company_acronym:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,company_type_acronym,company_acronym),]
      }
      
      if(!is.na(company_type_acronym)){
        df[, first_word_dist := stringsim(as.character(first_word), as.character(company_type_acronym), method='lv')]
        df[,max_first_word_dist :=max(max_first_word_dist, first_word_dist,na.rm=T), by=1:nrow(df)]
        df[,first_word_ind:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,1,first_word_ind),]
        df[,first_word:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,'',first_word),]
        df[,company_acronym:=fifelse(!is.na(first_word_dist)& first_word_dist>.8,company_type_acronym,company_acronym),]
      }
      
    }

    df[,company_name3:=stringr::str_trim(paste(first_word, last_words, sep=' ')),]
    
    # now consolidate across the three strings by taking 
    
    df[,company_name:= fifelse(last_word_ind==1, company_name2, company_name) ]
    df[,company_name:= fifelse(first_word_ind==1, company_name3, company_name) ]
    
    df[,company_name:=stringr::str_trim(company_name),]
    # temporary output
	fwrite(df, file=paste(tmp_output_dir, tmp_output_filename, "_pre_blank.csv", sep=""))

# fwrite(df, file=paste(output_dir, 'tmp/tmp_df_preprocess_pre_blank.csv', sep=''))
    # df_old = df
    # rm(df)
    # df = read_csv(file=paste(output_dir, 'tmp/tmp_df_preprocess_pre_blank.csv', sep=''))
    
    # above strips some firm names
	blank_names = df[nchar(company_name)==0,]
    if(nrow(blank_names)>0){
      blank_names = preprocess_firm_name_strings(blank_names,  sym(firm_name_col))  
		blank_names[,company_name:=remove_trailing_phrases_uspto_abridged(company_name),]
		nonblank_names = df[nchar(company_name)>0,]		
      	df = rbind(blank_names,nonblank_names)
    }
    
    
    
    
  } # end of remove_trailing_phrases==T 

  # setDT(df)
  df[,company_name:=gsub('NA', '', company_name, ignore.case=F),]
  df[,company_name:=stringr::str_squish(company_name),] 
	# get current date in yyyy-mm-dd format
	date_td = Sys.Date()
	date_td = format(date_td, '%Y-%m-%d')
	fwrite(df, file=paste(tmp_output_dir, tmp_output_filename, "_preprocess_final.csv", sep=""), append=T)
  # fwrite(df, file=paste(output_dir, 'tmp/', 'tmp_df_preprocess_final', date_td, '.csv', sep=''),append=T)
  # df_old = df
  # rm(df)
  # df = read_csv(file=paste(output_dir, 'tmp/tmp_df_preprocess_final.csv', sep=''))
  return(df)
}
