##########################################
# Process Capiq Business Descriptions for Acquisition Dates
#
# Last Modified: Sept 27, 2020
##########################################
# Restored Aug 4, 2023 from commit Feb 20, 2021 "finished fixing subsidiary code - removed vistigial chk"
# A) two common business description strings:
# "As of Month DD, YYYY, X was acquired"; "X was formerly a subsidiary.. As a result of Y's acquisition by Z on Month DD, YYYY" 

# 1) look for business descriptions with "acquisition" in the text
# 2) Look for first sentence which starts with "As " 
# 3) check for "acquisition by .. on"
# 4) extract Month DD, YYYY


identify_acq_date_from_acquisition_altA <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  date_string = NA_character_
  start_date_indx = regexpr('as of ', x, ignore.case=T) + 5
  end_date_indx = regexpr(', [0-9]{4}', x) + 5
  if(!is.na(start_date_indx) & !is.na(end_date_indx)){
    if(start_date_indx < end_date_indx){
      date_string = substr(x, start_date_indx,end_date_indx)
      date_string = substr(date_string, 1,20)
    }  
  }
  date_string = gsub('^ ', '', date_string)
  date_string = gsub(' $', '', date_string)
  date_string = as.Date(date_string, format='%B %d, %Y')
  return(date_string)
}
identify_acq_date_from_acquisition_altA_wrapper <- function(vec){
  out = lapply(vec, identify_acq_date_from_acquisition_altA)
  out = do.call(c,out)
}


merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_in_bus_description = ifelse(grepl('acquisition', `Business.Description`),1,0)) %>%
  mutate(acquisition_date = identify_acq_date_from_acquisition_altA_wrapper(`Business.Description`))

# B) Another common string
#   "X was acquired by Z in YYYY"

identify_acq_date_from_acquisition_altB <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  #date_string = NA_character_
  date_string = NA_real_
  start_date_indx = regexpr('in [0-9]{4}', x) + 3
  #end_date_indx = regexpr(', [0-9]{4}', x) + 6
  if(!is.na(start_date_indx)){
    #if(start_date_indx < end_date_indx){
    date_string = substr(x, start_date_indx,start_date_indx+4)
    date_string = as.numeric(date_string)
  }
  # if(as.numeric(date_string) < 100){
  #   date_string=paste('01-01-20', date_string)
  # } else {
  #   date_string = paste('01-01-', date_string, sep='')
  # }
  
  #date_string = as.Date(date_string, format='%d-%m-%Y')
  #date_string = as.Date(as.character(date_string), format='%d-%m-%Y')
  return(date_string)
}

identify_acq_date_from_acquisition_altB_wrapper <- function(vec){
  out = lapply(vec, identify_acq_date_from_acquisition_altB)
  out = do.call(c,out)
}


merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_in_bus_description = ifelse(grepl('acquisition', `Business.Description`),1,acquisition_in_bus_description)) %>%
  mutate(acquisition_dateB = identify_acq_date_from_acquisition_altB_wrapper(`Business.Description`)) 

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_dateB = as.numeric(acquisition_dateB)) %>%
  mutate(acquisition_dateB = as.Date(paste('01-01-',as.character(acquisition_dateB),sep=''), format='%d-%m-%Y') )

identify_acq_date_from_acquisition_altC <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  date_string = NA_character_
  start_date_indx = regexpr('on [A-z][a-z]+ [0-9]+, [0-9]{4}', x) + 3
  end_date_indx = regexpr('[0-9]{4}', x) + 4
  if(!is.na(start_date_indx)){
    if(start_date_indx < end_date_indx){
      date_string = substr(x, start_date_indx,end_date_indx)
    }
  }
  return(date_string)
}

identify_acq_date_from_acquisition_altC_wrapper <- function(vec){
  out = lapply(vec, identify_acq_date_from_acquisition_altC)
  out = do.call(c,out)
}

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_in_bus_description = ifelse(grepl('acquisition', `Business.Description`),1,acquisition_in_bus_description)) %>%
  mutate(acquisition_dateC = identify_acq_date_from_acquisition_altC_wrapper(`Business.Description`))

identify_acq_date_from_acquisition_altD <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  date_string = NA_character_
  start_date_indx = regexpr('o[a-z] [0-9]+/[0-9]{2}/[0-9]{4}', x) + 3
  end_date_indx = start_date_indx + 10
  if(!is.na(start_date_indx)){
    if(start_date_indx < end_date_indx){
      date_string = substr(x, start_date_indx,end_date_indx)
    }
    date_string = gsub(',', '', date_string)
    date_string = gsub('.', '', date_string, fixed=T)
  }
  date_string = as.Date(date_string, format='%m/%d/%Y')
  return(date_string)
}

identify_acq_date_from_acquisition_altD_wrapper <- function(vec){
  out = lapply(vec, identify_acq_date_from_acquisition_altD)
  out = do.call(c,out)
}
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_in_bus_description = ifelse(grepl('acquisition', `Business.Description`),1,acquisition_in_bus_description)) %>%
  mutate(acquisition_dateD = identify_acq_date_from_acquisition_altD_wrapper(`Business.Description`))

# E - identify dates with Day missing, 

identify_acq_date_from_acquisition_altE <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  date_string = NA_character_
  start_date_indx = regexpr('[A-Z][a-z]+ [0-9]{4}', x, ignore.case=T)
  end_date_indx = regexpr('[0-9]{4}', x) + 3
  
  if(!is.na(start_date_indx) & !is.na(end_date_indx)){
    if(start_date_indx < end_date_indx){
      date_string = substr(x, start_date_indx,end_date_indx)
      date_string = substr(date_string, 1,20)
    }  
  }
  date_string = gsub('^ ', '', date_string)
  date_string = gsub(' $', '', date_string)
  date_string = paste('28', date_string)
  date_string = as.Date(date_string, format='%d %B %Y')
  return(date_string)
}
identify_acq_date_from_acquisition_altE_wrapper <- function(vec){
  out = lapply(vec, identify_acq_date_from_acquisition_altE)
  out = do.call(c,out)
}
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_in_bus_description = ifelse(grepl('acquisition', `Business.Description`),1,acquisition_in_bus_description)) %>%
  mutate(acquisition_dateE = identify_acq_date_from_acquisition_altE_wrapper(`Business.Description`))


identify_found_date_from_acquisition  <- function(x){
  # x = Business.Description[1]
  #x = df$Business
  date_string = NA_character_
  start_date_indx = regexpr('founded in [0-9]{4}', x) + 11
  end_date_indx = start_date_indx + 4
  if(!is.na(start_date_indx)){
    if(start_date_indx < end_date_indx){
      date_string = substr(x, start_date_indx,end_date_indx)
    }
  }
  date_string = paste('01-01-', date_string, sep='')
  date_string = as.Date(date_string, format='%d-%m-%Y')
  return(date_string)
}

identify_found_date_from_acquisition_wrapper <- function(vec){
  out = sapply(vec, identify_found_date_from_acquisition)
}
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(founded_in_bus_description = ifelse(grepl('founded', `Business.Description`),1,0)) %>%
  mutate(founded_date = identify_found_date_from_acquisition_wrapper(`Business.Description`))


#######
# combine and collapse acquisition date
######

# merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
#   mutate(acquisition_date = substr(acquisition_date, 1, 20))
# merger_history_complete_corporate_tree_dataset_2020$acquisition_date =
#   as.Date(merger_history_complete_corporate_tree_dataset_2020$acquisition_date,  format='%B %d, %Y')

# merger_history_complete_corporate_tree_dataset_2020$acquisition_dateB =
#   as.Date(paste('01 01', merger_history_complete_corporate_tree_dataset_2020$acquisition_dateB),  format='%m %d, %Y')

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_dateC = substr(acquisition_dateC, 1, 20))

merger_history_complete_corporate_tree_dataset_2020$acquisition_dateC =
  as.Date(merger_history_complete_corporate_tree_dataset_2020$acquisition_dateC,  format='%B %d, %Y')

# combine the various identified acquisition dates together giving precedence
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_date_final = acquisition_date) %>%
  mutate(acquisition_date_final = ifelse(is.na(acquisition_date_final) & !is.na(acquisition_dateB), acquisition_dateB,acquisition_date_final )) %>%
  mutate(acquisition_date_final = ifelse(is.na(acquisition_date_final) & !is.na(acquisition_dateC), acquisition_dateC,acquisition_date_final )) %>%
  mutate(acquisition_date_final = ifelse(is.na(acquisition_date_final) & !is.na(acquisition_dateD), acquisition_dateD,acquisition_date_final )) %>%
  mutate(acquisition_date_final = ifelse(is.na(acquisition_date_final) & !is.na(acquisition_dateE), as.Date(acquisition_dateE),acquisition_date_final )) 
#mutate(acquisition_date_final = as.Date(acquisition_date_final, origin='1970-01-01')) # APPLE R default origin date

# merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
#   mutate(acquisition_date_final_tmp = as.Date(acquisition_date_final, origin='1970-01-01'))
  
# merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
#   mutate(acquisition_date_final_tmp = as.numeric(acquisition_date_final)) %>%
#   mutate(acquisition_date_final_tmp = as.Date(acquisition_date_final_tmp, origin='1970-01-01')) %>%
#   mutate(acquisition_date_final = as.Date(acquisition_date_final, format='%Y-%m-%d')) %>%
#   mutate(acquisition_date_final = ifelse(!is.na(acquisition_date_final_tmp), acquisition_date_final_tmp,acquisition_date_final ))

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquisition_date_final = as.Date(acquisition_date_final,  origin='1970-01-01'))

# merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
#   mutate(acquisition_date_final = as.Date(acquisition_date_final,  origin='1970-01-01'))

# chk = merger_history_complete_corporate_tree_dataset_2020 %>% filter(!is.na(acquisition_date) | !is.na(acquisition_dateB) | !is.na(acquisition_dateC) | !is.na(acquisition_dateD)) %>% 
#   select(acquisition_date_final,acquisition_date_final_tmp,acquisition_date, acquisition_dateB, acquisition_dateC, acquisition_dateD, `Business.Description`)
# #View(chk)
# 
# chk = merger_history_complete_corporate_tree_dataset_2020 %>% filter(!is.na(acquisition_date_final))%>% select(acquisition_date_final,acquisition_date, acquisition_dateB, acquisition_dateC, acquisition_dateD,acquisition_dateE, `Business.Description`)
# #View(chk)
# 
# chk = merger_history_complete_corporate_tree_dataset_2020 %>% filter(nchar(Business.Description)>0)%>% select(acquisition_date_final,acquisition_date, acquisition_dateB, acquisition_dateC, acquisition_dateD,acquisition_dateE, `Business.Description`)
# #View(chk)

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_ind = ifelse(!is.na(acquisition_date_final), 1, acquired_ind))


merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(consolidate_acquisition_date = acquired_date) %>% # giving first precedence to the acquired date identified from the merger data 
  mutate(consolidate_acquisition_date = ifelse(is.na(consolidate_acquisition_date), acquired_date, consolidate_acquisition_date)) %>%
  mutate(consolidate_acquisition_date = as.Date(consolidate_acquisition_date, origin='1970-01-01'))

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(merged_entity_ind = ifelse(Relationship.Type=='Merged Entity', 1,0)) %>%
  mutate(acquired_ind = ifelse(!is.na(consolidate_acquisition_date), 1, acquired_ind)) 
