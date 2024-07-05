########################################################################
# Fuzzy-string matching function routine
########################################################################
# Author: Mark Rempel
# last modified: January 2024
# last modified by: Joyce Wang

library(reticulate)

stopifnot(exists("code_dir"))
# stopifnot(exists("python_venv_machine_name"))

pairwise_fuzzy_string_matching = function(df, left_name, right_name, tfidf_score_exists =F,
										similarity_cutoff = .85, jw_cutoff = 0.03, similarity_metric_list=c('similarity','soundex', 'jw', 'lv', 'osa', 'dl'  )){

	# rename columns for convenience
	Require::Require("stringdist")	
	df = df %>% mutate(left_side = !!sym(left_name), right_side = !!sym(right_name))


	 # df = df %>% rename(left_side = left_company_name, right_side = right_company_name) # June 6, 2021 (new update to python script backend?)
	#
	# if('index' %in% names(df) & 'left_index' %in% names(df)){
	# 	df = df %>% rename(right_index = index)
	# }

	setDT(df)
		# remove any remaining non-ascii characters

	df[, left_side := iconv(left_side, "latin1", "ASCII", sub="")]
	df[, right_side := iconv(right_side, "latin1", "ASCII", sub="")]
	df[, jw_dist := stringdist(left_side, right_side, method='jw')]
	df[, lv_dist := stringdist(left_side, right_side, method='lv')]
	df[, lv_sim := stringdist::stringsim(left_side, right_side, method='lv')]
	df[, dl_sim := stringdist::stringsim(left_side, right_side, method='dl')]
	df[, cosine_sim := stringdist::stringsim(left_side, right_side, method='cosine')]
	df[,osa_sim := stringdist::stringsim(left_side, right_side, method='osa')]
	df[,j_sim := stringdist::stringsim(left_side, right_side, method='jaccard')]

	df[,jw_sim := stringdist::stringsim(left_side, right_side, method='jw')]
	df[, lcs_dist := stringdist(left_side, right_side, method='lcs')]

	df[, lcs_dist := stringdist(left_side, right_side, method='lcs')]
	df[, lcs_dist := 2*lcs_dist/(nchar(left_side) + nchar(right_side))]
	df[, osa_dist := stringdist(left_side, right_side, method='osa')]
	df[, soundex_dist := stringdist(left_side, right_side, method='soundex')]
	df[, j_dist := stringdist(left_side, right_side, method='jaccard')]
	df[, first_letter_same := ifelse(substr(left_side,1,1)==substr(right_side,1,1),1,0)]

	# rewrite below lines using data.table syntax

	df[, lhs_word1 := stringr::word(left_side,1)]
	df[, lhs_word2 := stringr::word(left_side,2)]
	df[, rhs_word1 := stringr::word(right_side,1)]
	df[, rhs_word2 := stringr::word(right_side,2)]

	df[, jw_dist_word1 := stringdist(lhs_word1, rhs_word1, method='jw')]
	df[, lv_dist_word1 := stringdist(lhs_word1, rhs_word1, method='lv')]
	df[, lcs_dist_word1 := stringdist(lhs_word1, rhs_word1, method='lcs')]
	df[, lcs_dist_word1 := 2*lcs_dist/(nchar(lhs_word1) + nchar(rhs_word1))]
	df[, osa_dist_word1 := stringdist(lhs_word1, rhs_word1, method='osa')]
	df[, soundex_dist_word1 := stringdist(lhs_word1, rhs_word1, method='soundex')]
	df[, j_dist_word1 := stringdist(lhs_word1, rhs_word1, method='jaccard')]

	# arrange matches by descending similarity, soundex, and increasing alphabetical order of left_side, right_side names 

	if(tfidf_score_exists == T ){
		# df[, similarity := (1-jw_dist)*100]


	 df[, is_match := fifelse(similarity >=similarity_cutoff,1,0)]
	} else {
		df[, similarity := NA_real_]
		df[, is_match := 0]

		# df[, similarity := (1-jw_dist)]
		# similarity_cutoff = jw_cutoff
	}

	if(!'index' %in% names(df) & tfidf_score_exists == T){
		setkeyv(df, c('left_side', 'right_side', 'left_index', 'right_index'))
	} else {
		setkeyv(df, c('left_side', 'right_side'))
	}

	# 	df = df %>% rename(right_index = index)
	# }


	setorder(df, -similarity, -soundex_dist, left_side, right_side)

	# remove duplicates

	df=unique(df)
 	

	# now make classification of matches 

	# df[, is_match := fifelse(similarity >=similarity_cutoff,1,0)]
	df[, is_match := fifelse(soundex_dist==0,1,is_match)]
	df[, is_match := fifelse(jw_dist < jw_cutoff,1,is_match)]

	df[, is_match := ifelse(lv_dist ==1 & first_letter_same==1 ,1,is_match)] # if single character, not the first character discrepancy then treat as a match 


	# rewrite below lines using data.table syntax

	df[, is_match_1st_word := fifelse(j_dist_word1 < jw_cutoff,1,0)]
	df[, is_match_1st_word := fifelse(soundex_dist_word1==0,1,is_match_1st_word)]
	df[, is_match_1st_word := fifelse(lv_dist_word1 ==1 & first_letter_same==1 ,1,is_match_1st_word)] # if single character, not the first character discrepancy then treat as a match 


 
	# remove columns which are no longer needed

	df[,left_side:=NULL]
	df[,right_side:=NULL]

  return(df)
}



fuzzy_string_matching_function = function(companynamesA, companynamesB,use_reticulate=F, # edited out "=NULL"
										  python_path='/usr/local/bin/python3',
										  best_match_only =T,
										  keep_all_matches=F, use_1st_word_matches=F, tmp_dir=getwd()){
  
	Require::Require("reticulate")
  Require::Require("stringdist")
  
 	setwd(tmp_dir) # for python scripts, set directory

  if(!is.null(companynamesB)){
    # string matching across A and B
    
    # provide input files for script
    write_csv(companynamesA, path=paste(tmp_dir, '/company_names_to_match_series_A.csv',sep=''))
    write_csv(companynamesB, path=paste(tmp_dir, '/company_names_to_match_series_B.csv',sep=''))
    
    # run python script
	  py_script_name = paste(code_dir, "fuzzy_match_core_routines/firm_string_matching_python_pairwise_script.py",sep='') ####### LOOK AT TWO DATASETS TRY TO MATCH (ACROSS)
    py_run_file(py_script_name) # takes input company_names_to_match.csv and outputs
	
    # read in output form python script (this is for across matching of A and B) # edited in Jan 14, 2024
    matches = read_csv(paste(tmp_dir, '/company_names_matched_pairwise.csv',sep=''))
    
  } else {
    
    # string match within list of names in A
    write_csv(companynamesA, path=paste(tmp_dir, "/company_names_to_match_series_all.csv",sep='')) # 
    
	  py_script_name  <- paste(code_dir, "fuzzy_match_core_routines/firm_string_matching_python_script.py",sep='')

	  if(use_reticulate==T){
		  py_run_file(py_script_name) # using reticulate -  takes input company_names_to_match.csv and outputs
	  } else {
		  system(paste0("pipenv run python ", py_script_name))
	  }
	  
	  # read in output form python script (this is for within matching of A) # edited in Jan 14, 2024
	  matches = read_csv(paste(tmp_dir, '/company_names_matched.csv',sep=''))
	  
  }
  
 	matches = matches %>% rename(left_side = left_company_name, right_side = right_company_name) # edited in Jan 14, 2024
 	
 	if('index' %in% names(matches) & 'left_index' %in% names(matches)){
 	  matches = matches %>% rename(right_index = index)
 	}

	setDT(matches)
	matches[, jw_dist := stringdist(left_side, right_side, method='jw')]
	matches[, lv_dist := stringdist(left_side, right_side, method='lv')]
	matches[, lcs_dist := stringdist(left_side, right_side, method='lcs')]
	matches[, lcs_dist := 2*lcs_dist/(nchar(left_side) + nchar(right_side))]
	matches[, osa_dist := stringdist(left_side, right_side, method='osa')]
	matches[, soundex_dist := stringdist(left_side, right_side, method='soundex')]
	matches[, j_dist := stringdist(left_side, right_side, method='jaccard')]
	matches[, first_letter_same := ifelse(substr(left_side,1,1)==substr(right_side,1,1),1,0)]

	# rewrite below lines using data.table syntax
	matches[, lhs_word1 := stringr::word(left_side,1)]
	matches[, lhs_word2 := stringr::word(left_side,2)]
	matches[, rhs_word1 := stringr::word(right_side,1)]
	matches[, rhs_word2 := stringr::word(right_side,2)]

	matches[, jw_dist_word1 := stringdist(lhs_word1, rhs_word1, method='jw')]
	matches[, lv_dist_word1 := stringdist(lhs_word1, rhs_word1, method='lv')]
	matches[, lcs_dist_word1 := stringdist(lhs_word1, rhs_word1, method='lcs')]
	matches[, lcs_dist_word1 := 2*lcs_dist/(nchar(lhs_word1) + nchar(rhs_word1))]
	matches[, osa_dist_word1 := stringdist(lhs_word1, rhs_word1, method='osa')]
	matches[, soundex_dist_word1 := stringdist(lhs_word1, rhs_word1, method='soundex')]
	matches[, j_dist_word1 := stringdist(lhs_word1, rhs_word1, method='jaccard')]

	# arrange matches by descending similarity, soundex, and increasing alphabetical order of left_side, right_side names

	setkeyv(matches, c('left_side', 'right_side', 'left_index', 'right_index'))
	setorder(matches, -similarity, -soundex_dist, left_side, right_side)

	# remove duplicates

	matches=unique(matches)


	# now make classification of matches

	matches[, is_match := ifelse(similarity >=.85,1,0)]
	matches[, is_match := ifelse(soundex_dist==0,1,is_match)]
	matches[, is_match := ifelse(jw_dist < 0.03,1,is_match)]

	matches[, is_match := ifelse(lv_dist ==1 & first_letter_same==1 ,1,is_match)] # if single character, not the first character discrepancy then treat as a match


	# rewrite below lines using data.table syntax

	matches[, is_match_1st_word := ifelse(j_dist_word1 <.03,1,0)]
	matches[, is_match_1st_word := ifelse(soundex_dist_word1==0,1,is_match_1st_word)]
	matches[, is_match_1st_word := ifelse(lv_dist_word1 ==1 & first_letter_same==1 ,1,is_match_1st_word)] # if single character, not the first character discrepancy then treat as a match


	# keep matches which are confirmed by both full string and first word match


	if(keep_all_matches==F & use_1st_word_matches==F){
			matches = subset(matches, is_match==1)
  	} else {
    	if(keep_all_matches==F & use_1st_word_matches==T){

			matches[, lhs_match_indx := .I]
			matches[, rhs_match_indx := .I]
			matches[, is_match_for_lhs := ifelse(is_match==1, 1, 0)]
			matches[, there_is_match_for_lhs := max(is_match_for_lhs,na.rm=T)] # now add match if no confirmed match then check for is match from 1st word
			matches[, is_match_for_lhs := ifelse(there_is_match_for_lhs==0 &  is_match_1st_word==1,1,is_match_for_lhs )]
			matches[, lhs_match_count := .N, by=left_side]

			matches[, rhs_match_indx := .I, by=right_side]
			matches[, is_match_for_rhs := ifelse( is_match==1, 1, 0)]
			matches[, there_is_match_for_rhs := max(is_match_for_rhs)] # now add match if no confirmed match then check for is match from 1st words
			matches[, is_match_for_rhs := ifelse(there_is_match_for_rhs==0 &  is_match_1st_word==1,1,is_match_for_rhs )]
			matches[, rhs_match_count := .N, by=right_side]

			matches = subset(matches, is_match_for_rhs==1 & is_match_for_lhs==1)

    	}
  	}

	# Now rename columns if supplied with two lists of names to match

  if(!is.null(companynamesB)){

    # if matching across two lists of names
  	matches[, namesA := left_side]
  	matches[, namesB := right_side]

  } else {
    # get ride of the tautologies in matching within a list
    # take the closest match for each left side company name

		if(best_match_only==T){
		  # restrict to the closest match for each company name that isn't identical to itself
			matches = subset(matches, !(left_side==right_side))
			matches = matches[, .SD[which.max(similarity)], by=left_side]
		}
  }
  
  return(matches)
  
}

fuzzy_string_matching_function_nov2020 = function(companynamesA, companynamesB=NULL, python_path='/usr/local/bin/python3', keep_all_matches=F, use_1st_word_matches=F){
  # updated version with two round matching
  #   after doing the full string tf-idf match
  #     split words of the proposed matches
  #       look for identical matches in the first word
  
  #   make python connection
  require(reticulate)
  use_python(python_path)
  
  if(!is.null(companynamesB)){
    # string matching across A and B
      
      # provide input files for script
      write_csv(companynamesA, path=paste(output_dir, 'company_names_to_match_series_A.csv',sep=''))
      write_csv(companynamesB, path=paste(output_dir, 'company_names_to_match_series_B.csv',sep=''))
      
      # run python script
      py_run_file(paste(code_dir, "fuzzy_match_core_routines/firm_string_matching_python_pairwise_script.py",sep='')) # takes input company_names_to_match.csv and outputs
  } else {
    
    # string match within list of names in A
    write_csv(companynamesA, path=paste(output_dir, 'tmp/company_names_to_match_series_all.csv',sep=''))# 
    
    py_run_file(paste(code_dir, "fuzzy_match_core_routines/firm_string_matching_python_script.py",sep='')) # takes input company_names_to_match.csv and outputs
  }
    
      # read in output from python script
      matches = read_csv(paste(output_dir, 'tmp/company_names_matched.csv',sep=''))
      
      matches = matches %>% 
        mutate(jw_dist = stringdist(left_side, right_side, method='jw')) %>%
        mutate(lv_dist = stringdist(left_side, right_side, method='lv')) %>%
        mutate(lcs_dist = stringdist(left_side, right_side, method='lcs')) %>%
        mutate(lcs_dist = 2*lcs_dist/(nchar(left_side) + nchar(right_side))) %>%
        mutate(osa_dist = stringdist(left_side, right_side, method='osa')) %>%
        mutate(soundex_dist = stringdist(left_side, right_side, method='soundex')) %>%
        mutate(j_dist = stringdist(left_side, right_side, method='jaccard')) %>%
        mutate(first_letter_same = ifelse(substr(left_side,1,1)==substr(right_side,1,1),1,0))
      
      # now do comparison of the first word
      matches = matches %>% 
        mutate(lhs_word1 = stringr::word(left_side,1), lhs_word2 = stringr::word(left_side,2),
               rhs_word1 = stringr::word(right_side,1), rhs_word2 = stringr::word(right_side,2),
        )
      matches = matches %>% 
        mutate(jw_dist_word1 = stringdist(lhs_word1, rhs_word1, method='jw')) %>%
        mutate(lv_dist_word1 = stringdist(lhs_word1, rhs_word1, method='lv')) %>%
        mutate(lcs_dist_word1 = stringdist(lhs_word1, rhs_word1, method='lcs')) %>%
        mutate(lcs_dist_word1 = 2*lcs_dist/(nchar(lhs_word1) + nchar(rhs_word1))) %>%
        mutate(osa_dist_word1 = stringdist(lhs_word1, rhs_word1, method='osa')) %>%
        mutate(soundex_dist_word1 = stringdist(lhs_word1, rhs_word1, method='soundex')) %>%
        mutate(j_dist_word1 = stringdist(lhs_word1, rhs_word1, method='jaccard')) 
      
      matches = matches %>% arrange(desc(similarity), desc(soundex_dist), left_side, right_side)
      
      matches = matches %>% distinct()
      
      matches = matches %>% mutate(is_match = ifelse(similarity >=.85,1,0)) %>%
        mutate(is_match = ifelse(soundex_dist==0,1,is_match)) %>%
        mutate(is_match = ifelse(jw_dist < 0.03,1,is_match))  %>%
        mutate(is_match = ifelse(lv_dist ==1 & first_letter_same==1 ,1,is_match)) # if single character, not the first character discrepancy then treat as a match
      
      matches = matches %>% mutate(is_match_1st_word = ifelse(j_dist_word1 <.03,1,0)) %>%
        mutate(is_match_1st_word = ifelse(soundex_dist_word1==0,1,is_match_1st_word)) %>%
        mutate(is_match_1st_word = ifelse(jw_dist_word1 < 0.03,1,is_match_1st_word))  %>%
        mutate(is_match_1st_word = ifelse(lv_dist_word1 ==1 & first_letter_same==1 ,1,is_match_1st_word)) # if single character, not the first character discrepancy then treat as a match
      
      if(keep_all_matches==F & use_1st_word_matches==F){
        matches = matches %>% filter(is_match==1)  
      } else {
        if(keep_all_matches==F & use_1st_word_matches==T){
          matches = matches %>% ungroup() %>%
            group_by(left_side) %>% arrange(desc(is_match_1st_word), desc(is_match), desc(similarity)) %>%
            mutate(lhs_match_indx= row_number())%>%
            mutate(is_match_for_lhs = ifelse(is_match==1, 1, 0)) %>%
            mutate(there_is_match_for_lhs= max(is_match_for_lhs,na.rm=T)) %>% # now add match if no confirmed match then check for is match from 1st word
            mutate(is_match_for_lhs = ifelse(there_is_match_for_lhs==0 &  is_match_1st_word==1,1,is_match_for_lhs )) %>%
            mutate(lhs_match_count = n())
          
          matches = matches %>% ungroup() %>% 
            group_by(right_side) %>% arrange(desc(is_match_1st_word), desc(is_match), desc(similarity)) %>% 
            mutate(rhs_match_indx=row_number()) %>%
            mutate(is_match_for_rhs = ifelse( is_match==1, 1, 0)) %>%
            mutate(there_is_match_for_rhs = max(is_match_for_rhs)) %>% # now add match if no confirmed match then check for is match from 1st word
            mutate(is_match_for_rhs = ifelse(there_is_match_for_rhs==0 &  is_match_1st_word==1,1,is_match_for_rhs )) %>%
            mutate(rhs_match_count = n())
          matches = matches %>% filter(is_match_for_rhs==1 & is_match_for_lhs==1)
        }
      }
      

    if(!is.null(companynamesB)){
      # if matching across two lists
      matches = matches %>% rename(namesA=left_side, namesB=right_side)
    } else {
      # get ride of the tautologies in matching within a list
      #   take the closest match for each left side company name
      matches = matches %>%
         filter(!(left_side==right_side)) %>% 
         group_by(left_side) %>% filter(similarity==max(similarity,na.rm=T))
    }
      
  return(matches)
    
}

fuzzy_string_matching_function_original = function(companynamesA, companynamesB=NULL, python_path='/usr/local/bin/python3', keep_all_matches=F){
  
  #   make python connection
  require(reticulate)
  use_python(python_path)
  
  if(!is.null(companynamesB)){
    # string matching across A and B
    
    # provide input files for script
    write_csv(companynamesA, path=paste(output_dir, 'company_names_to_match_series_A.csv',sep=''))
    write_csv(companynamesB, path=paste(output_dir, 'company_names_to_match_series_B.csv',sep=''))
    
    # run python script
    py_run_file("firm_string_matching_python_pairwise_script.py") # takes input company_names_to_match.csv and outputs
  } else {
    
    # string match within list of names in A
    write_csv(companynamesA, path=paste(output_dir, 'company_names_to_match_series_all.csv',sep=''))# 
    
    py_run_file("firm_string_matching_python_script.py") # takes input company_names_to_match.csv and outputs
  }
  
  # read in output from python script
  matches = read_csv(paste(output_dir, 'company_names_matched.csv',sep=''))
  
  matches = matches %>% 
    mutate(jw_dist = stringdist(left_side, right_side, method='jw')) %>%
    mutate(lv_dist = stringdist(left_side, right_side, method='lv')) %>%
    mutate(lcs_dist = stringdist(left_side, right_side, method='lcs')) %>%
    mutate(lcs_dist = 2*lcs_dist/(nchar(left_side) + nchar(right_side))) %>%
    mutate(osa_dist = stringdist(left_side, right_side, method='osa')) %>%
    mutate(soundex_dist = stringdist(left_side, right_side, method='soundex')) %>%
    mutate(j_dist = stringdist(left_side, right_side, method='jaccard')) %>%
    mutate(first_letter_same = ifelse(substr(left_side,1,1)==substr(right_side,1,1),1,0))
  
  
  matches = matches %>% arrange(desc(similarity), desc(soundex_dist), left_side, right_side)
  
  matches = matches %>% distinct()
  
  matches = matches %>% mutate(is_match = ifelse(similarity >=.85,1,0)) %>%
    mutate(is_match = ifelse(soundex_dist==0,1,is_match)) %>%
    mutate(is_match = ifelse(jw_dist < 0.03,1,is_match))  %>%
    mutate(is_match = ifelse(lv_dist ==1 & first_letter_same==1 ,1,is_match)) # if single character, not the first character discrepancy then treat as a match
  
  if(keep_all_matches==F){
    matches = matches %>% filter(is_match==1)  
  }
  
  
  if(!is.null(companynamesB)){
    # if matching across two lists
    matches = matches %>% rename(namesA=left_side, namesB=right_side)
  } else {
    # get ride of the tautologies in matching within a list
    #   take the closest match for each left side company name
    matches = matches %>%
      filter(!(left_side==right_side)) %>% 
      group_by(left_side) %>% filter(similarity==max(similarity,na.rm=T))
  }
  
  return(matches)
  
}

