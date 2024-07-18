####################################
# Combine M&A data with the complete corporate tree 
# Last modified: Oct 11, 2020
####################################
# Restored Aug 4, 2023 from Feb 20, 2021 ("relabeled subsidiary <- dataset <- history, ...")
#
# Goal of doing this matching of mergers to subsidiaries, is to indicate the relevant date from which all 
#   patents owned by a current subsidiary of the parent can be attributed to the parent

# Matching strategy:
#
#   I) Direct match mergers 
#     A) target ID = CIQ of subsidiary AND ParentCompany Name == Buyer Name 
#       OR target Name = CIQ Company.Name AND ParentCompany Name == Buyer Name 
#     B) target ID = CIQ of subsidiary AND buyer ID %in% corporate tree of parent
#
#
#   II) Exact match of buyer, indirect match of target
#           A) fuzzy match of target
#           - this case is useful to identify acquisitions of large parent companies whose subsidiaries are the objects transferred
#               e.g. Whole Foods acquired by Amazon has a number of subsidiaries, who all get assigned at the same time
#           
#
#           B) Take list of matched targets, merge with parent.Company in corporate tree, assign all the subsidiaries with parent_acquired_date = ..




# the fuzzy matching below has been commented out (it is too aggressive - identifies many homegrown subsidiaries as acquired)
#
#   III)  Exact match of target, fuzzy match of buyer
#           - this case is useful if typos in buyer name or buyer name changed?
#
#   IV)  Fuzzy match of both target and buyer
#           - in this case, if the acquisition is 100% and the value is large, 


################
# Preliminaries 
################

  # load dependencies

require(tidyverse)
require(stargazer)
load(CIQ_merger_dataset_file)
  #load(paste(output_dir, 'firm_data/full_capiq_merger_dataset_2020.RData',sep=''))

load(corporate_tree_data_file)
  #load(paste(output_dir, 'firm_data/complete_corporate_tree_dataset_2020.RData',sep='')) 

  #  ensure merger data is clean

full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% filter(!`Transaction Status`=='Cancelled')

# eliminate `duplicate` merger transactions, where only difference is wording in  target security type
# MZ comment out, no target security types column
# full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% select(-`Target Security Types`)
# full_capiq_merger_dataset_2020 = unique(full_capiq_merger_dataset_2020)

full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% group_by(`CIQ Transaction ID`) %>% mutate(num_obs_per_trans_id = n())
full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% mutate(non_controlling_acq = ifelse(`Percent Sought (%)` < 50,1,0))
full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% mutate(buyer_company_name = `Buyers/Investors`) %>% ungroup()


# create synthetic IQ numbers for those with missing transaction numbers
# full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>%
#   mutate(`Original CIQ Transaction ID ` = `CIQ Transaction ID` ) %>%
#   mutate(CIQ_transaction_id = `CIQ Transaction ID` )

full_capiq_merger_dataset_2020_no_id = full_capiq_merger_dataset_2020 %>% filter(is.na(`CIQ Transaction ID`))
full_capiq_merger_dataset_2020_w_id = full_capiq_merger_dataset_2020 %>% filter(!is.na(`CIQ Transaction ID`))

stopifnot(nrow(full_capiq_merger_dataset_2020_no_id)==0)


##################
# (I) Direct match mergers where target ID = CIQ of subsidiary AND ParentCompany Name == Buyer Name
##################
  # Case A: 

complete_corporate_tree_dataset_2020 = complete_corporate_tree_dataset_2020 %>% distinct()
base_merger_complete_corporate_tree_dataset_2020 = left_join(complete_corporate_tree_dataset_2020, full_capiq_merger_dataset_2020, by=c('CIQ.Company.ID'='Excel Company ID [Target/Issuer]', 'Parent.Company'='Buyers/Investors'))

base_merger_complete_corporate_tree_dataset_2020 = base_merger_complete_corporate_tree_dataset_2020 %>% mutate(`Buyers/Investors`=Parent.Company, `Excel Company ID [Target/Issuer]`= CIQ.Company.ID)
  
# eliminate  duplicates here driven by two merger transaction observations which are identical except for small immaterial details in merger docs 
base_merger_complete_corporate_tree_dataset_2020 = base_merger_complete_corporate_tree_dataset_2020 %>%
  group_by(parent_name, Company.Name, CIQ.Company.ID,`All Transactions Announced Date`, `CIQ Transaction ID`) %>% filter(row_number()==1)


direct_match_subsidiaries = base_merger_complete_corporate_tree_dataset_2020 %>% filter(!is.na(`CIQ Transaction ID`))
nodirect_match_subsidiaries = base_merger_complete_corporate_tree_dataset_2020 %>% filter(is.na(`CIQ Transaction ID`))
nodirect_match_subsidiaries_tree = nodirect_match_subsidiaries %>% ungroup() %>% select(names(complete_corporate_tree_dataset_2020))

print(paste('Number of direct match subsidiaries:', nrow(direct_match_subsidiaries), ' which accounts for fraction ', nrow(direct_match_subsidiaries)/nrow(complete_corporate_tree_dataset_2020)))
direct_match_subsidiaries = direct_match_subsidiaries %>% 
  mutate(merge_match_type='IAi')

# stop if there's rows missing
stopifnot(nrow(direct_match_subsidiaries) + nrow(nodirect_match_subsidiaries_tree) == nrow(base_merger_complete_corporate_tree_dataset_2020))


#######
# Aii) Direct match using Target string and Buyer Name (No more target ID = CIQ of subsidiary)
######

  # match the unmatched from the above
base_merger_complete_corporate_tree_dataset_2020 = left_join(nodirect_match_subsidiaries_tree, full_capiq_merger_dataset_2020, by=c('Company.Name'='Target/Issuer', 'Parent.Company'='Buyers/Investors'))
base_merger_complete_corporate_tree_dataset_2020 = base_merger_complete_corporate_tree_dataset_2020 %>% mutate(`Target/Issuer`=Company.Name, `Buyers/Investors`= Parent.Company)
nodirect_match_subsidiaries = base_merger_complete_corporate_tree_dataset_2020 %>% filter(is.na(`CIQ Transaction ID`)) %>% distinct()
direct_match_subsidiariesAii = base_merger_complete_corporate_tree_dataset_2020 %>% filter(!is.na(`CIQ Transaction ID`))
direct_match_subsidiariesAii = direct_match_subsidiariesAii %>% 
  mutate(merge_match_type='IAii')


# combine A and Aii 
# gives dataset combining M & A transactions with Corporate Tree, where:
#       target ID = CIQ of subsidiary AND Parent Company Name == Buyer Name
#       OR Target = Buyer name 

cnames = intersect(names(direct_match_subsidiaries), names(direct_match_subsidiariesAii))
direct_match_subsidiaries = direct_match_subsidiaries[,cnames]
direct_match_subsidiariesAii = direct_match_subsidiariesAii[,cnames]

direct_match_subsidiaries = rbind(direct_match_subsidiaries,direct_match_subsidiariesAii)
nodirect_match_subsidiaries_tree = nodirect_match_subsidiaries %>% ungroup() %>% select(names(complete_corporate_tree_dataset_2020)) # store ones that had were unable to match from corp tree 

##############
# Case IB: CIQ ID = Target ID
# determine if buyer is ParentCompany / Ultimate Parent Company / other sub of Ultimate parent
# - buyer ID is in the list of subsidiaries of the ultimate parent 

# exact_target_match_subsidiaries = left_join(nodirect_match_subsidiaries_tree, full_capiq_merger_dataset_2020, by=c('CIQ.Company.ID'='Excel Company ID [Target/Issuer]'))
exact_target_match_subsidiaries = merge(nodirect_match_subsidiaries_tree, full_capiq_merger_dataset_2020, by.x=c('CIQ.Company.ID'),by.y=c('Excel Company ID [Target/Issuer]'))
exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% mutate(`Excel Company ID [Target/Issuer]`=`CIQ.Company.ID`)
exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% distinct()

Check = exact_target_match_subsidiaries[,c(1:12, which(names(exact_target_match_subsidiaries)=='parent_id'),
                                           which(names(exact_target_match_subsidiaries)=='Excel Company ID [Buyers/Investors]'))]

tmp1 = function(x, y=parent_id){
  # x is a CIQ ID of a buyer
  # look up the corporate tree
  result_ind = 0
  # print(paste("x", x, "y", y))
  if(!is.na(x)){
    buyer_subsidiary = complete_corporate_tree_dataset_2020 %>% filter(CIQ.Company.ID==x)  
    # print(paste("x", x, "y", y))
    # print(buyer_subsidiary)
    if(nrow(buyer_subsidiary)>0){
      #  found at least one match of buyer, now check that they have the same parent 
      if(any(buyer_subsidiary$parent_id==y)){
        # print("found true")
        result_ind = 1  
      }
    }
  }
  return(result_ind)
}
# check that  buyer ID is in the list of subsidiaries of the ultimate parent 

exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% group_by(Company.Name, CIQ.Company.ID, `CIQ Transaction ID`) %>%
  mutate(buyer_ID_in_ult_parent_tree_list = max(sapply(strsplit(`Excel Company ID [Buyers/Investors]`, '[:punct:]'), function(x) tmp1(x, y=parent_id)),na.rm=T))

# check number of rows in exact_target_match here 
# make log file to print out how many rows and all error messages 
# see how mergers work for one big company 

# exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% group_by(Company.Name, CIQ.Company.ID, `CIQ Transaction ID`) %>%
#   mutate(buyer_ID_in_ult_parent_tree_list = max(sapply(strsplit(`Excel Company ID [Buyers/Investors]`, '[:punct:]'), function(x, y=parent_id){
#     # x is a CIQ ID of a buyer
#     # look up the corporate tree
#     result_ind = 0
#     if(!is.na(x)){
#       buyer_subsidiary = complete_corporate_tree_dataset_2020 %>% filter(CIQ.Company.ID==x)  
#       if(nrow(buyer_subsidiary)>0){
#         #  found at least one match of buyer, now check that they have the same parent 
#         if(buyer_subsidiary$parent_id==y){
#           result_ind = 1  
#         }
#       }
#     }
#     return(result_ind)
#   }),na.rm=T))

# duplicate observations possible based on substantively identical merger observations paired to the same subsidiary

exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% group_by(CIQ.Company.ID, `CIQ Transaction ID`, Company.Name) %>%
  filter(row_number()==1) %>% ungroup()

# noexact_target_match_subsidiaries = exact_target_match_subsidiaries %>% filter(buyer_ID_in_ult_parent_tree_list==0 | buyer_is_parent_ind==0 | `Percent Sought (%)` < 50)
# exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% filter(!(buyer_ID_in_ult_parent_tree_list==0 | buyer_is_parent_ind==0 | `Percent Sought (%)` < 50))
######MZ: this causes exact_target_match_subsidiaries to be 0
noexact_target_match_subsidiaries = exact_target_match_subsidiaries %>% filter(buyer_ID_in_ult_parent_tree_list==0 | `Percent Sought (%)` < 50)
exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% filter(!(buyer_ID_in_ult_parent_tree_list==0 |  `Percent Sought (%)` < 50))
exact_target_match_subsidiaries = exact_target_match_subsidiaries %>% mutate(merge_match_type='IB')

print(paste('Number of match type IB subsidiaries (exact ID match of buyer in corp tree) and target:', nrow(exact_target_match_subsidiaries), ' which accounts for fraction ', nrow(exact_target_match_subsidiaries)/nrow(complete_corporate_tree_dataset_2020)))


noexact_target_match_subsidiaries_tree = noexact_target_match_subsidiaries %>% ungroup() %>% select(names(complete_corporate_tree_dataset_2020))

#MZ added
# Function to remove columns with names like ...[int]
remove_dot_int_columns <- function(df) {
  df %>% select(-matches("^\\.\\.\\.\\d+$"))
}

# Remove columns with names like ...[int] from both data frames
direct_match_subsidiaries <- remove_dot_int_columns(direct_match_subsidiaries)
exact_target_match_subsidiaries <- remove_dot_int_columns(exact_target_match_subsidiaries)

cnames = intersect(names(exact_target_match_subsidiaries), names(direct_match_subsidiaries))

## MZ:
# Get column names and types for both data frames
# direct_types <- sapply(direct_match_subsidiaries, function(x) class(x)[1])
# exact_types <- sapply(exact_target_match_subsidiaries, function(x) class(x)[1])
# 
# # Identify columns with mismatched types
# mismatched_columns <- names(direct_types)[direct_types != exact_types]
# print(mismatched_columns)
# cnames <- setdiff(cnames, mismatched_columns)
# 
# direct_match_subsidiaries = direct_match_subsidiaries[,cnames]
# exact_target_match_subsidiaries = exact_target_match_subsidiaries[,cnames]


matched_subsidiaries_I = rbind(direct_match_subsidiaries,exact_target_match_subsidiaries)

# matchedII got scraped 
## MZ: I took out 'Deal Resolution' from the list, it didnt exist
matched_subsidiaries_IandII = matched_subsidiaries_I
matched_subsidiaries_IandII = matched_subsidiaries_IandII %>% relocate(`CIQ Transaction ID`, `All Transactions Announced Date`,
                                                                       `Transaction Comments`, Company.Name,
                                                                       `Target/Issuer`, Parent.Company, `Buyers/Investors`,
                                                                       parent_name, Ultimate.Corporate.Parent) %>% 
  arrange(parent_id, `All Transactions Announced Date`,Company.Name) %>% 
  filter(`Percent Sought (%)`>=50)


nrow(matched_subsidiaries_IandII)/nrow(complete_corporate_tree_dataset_2020)

# since I dropped the type II matching, 

noexact_parent_id_match_subsidiaries = exact_target_match_subsidiaries # this is 0

###############################
# III) fill in subsidiaries of
#   E.g. subsidiaries of Whole Foods, where Whole Foods Inc. identified as acquired, but the subsidiaries are not
#     look through matched_subsidiaries the target name, match it with parent Company names in the corporate tree
#         assign the acquisition date of the parent
###############################

noexact_parent_id_match_subsidiaries_tree = noexact_parent_id_match_subsidiaries %>% ungroup() %>% select(names(complete_corporate_tree_dataset_2020))

parentCompany_acquired_merge = left_join(noexact_parent_id_match_subsidiaries_tree,matched_subsidiaries_IandII, by=c('Parent.Company'='Target/Issuer')) 
parentCompany_acquired_merge  = parentCompany_acquired_merge %>% mutate(`Target/Issuer`=Parent.Company)
# noparentCompany_acquired_merge  = parentCompany_acquired_merge %>% mutate(`Target/Issuer`=Parent.Company) %>% filter(is.na(`CIQ Transaction ID`))
# 
# parentCompany_acquired_merge  = parentCompany_acquired_merge %>% mutate(`Target/Issuer`=Parent.Company) %>% filter(!is.na(`CIQ Transaction ID`))
parentCompany_acquired_merge  = parentCompany_acquired_merge  %>% distinct()
parentCompany_acquired_merge = parentCompany_acquired_merge[,!grepl('\\.y', names(parentCompany_acquired_merge))]
names(parentCompany_acquired_merge) = gsub('\\.x$', '', names(parentCompany_acquired_merge))
parentCompany_acquired_merge  = parentCompany_acquired_merge  %>% group_by(`CIQ Transaction ID`, Company.Name, `All Transactions Announced Date`) %>% filter(row_number()==1) %>% ungroup()
parentCompany_acquired_merge = parentCompany_acquired_merge %>% filter(`Percent Sought (%)`>=50)


# compute size

# MZ commented out:
# acquired_size = sapply(parentCompany_acquired_merge$Transactions, function(x){
#   substr(x,  regexpr('Size ($mm): ', x, fixed=T) + 12, regexpr('Status', x)-3)
# })
# MZ replace with: (because Transactions column does not exist)
# acquired_size = sapply(parentCompany_acquired_merge$`All Transactions Announced Date`, function(x){
#   substr(x,  regexpr('Size ($mm): ', x, fixed=T) + 12, regexpr('Status', x)-3)
# })
acquired_size = sapply(parentCompany_acquired_merge$`Transaction Comments`, function(x){
  substr(x,  regexpr('Size ($mm): ', x, fixed=T) + 12, regexpr('Status', x)-3)
})

# MZ These columns dont exist
# parentCompany_acquired_merge$acquired_size = acquired_size
# parentCompany_acquired_merge = parentCompany_acquired_merge %>%
#   mutate(acquired_size = ifelse(is.na(acquired_size),`Total Transaction Value ($USDmm, Historical rate)` ,acquired_size)) %>%
#   mutate(acquired_size = ifelse(is.na(acquired_size),`Total Gross Transaction Value ($USDmm, Historical rate)` ,acquired_size))

#MZ removed 'Deal Resolution' from list
parentCompany_acquired_merge =parentCompany_acquired_merge %>% relocate(`CIQ Transaction ID`, `All Transactions Announced Date`,`Transaction Comments`, Company.Name, `Target/Issuer`, Parent.Company, `Buyers/Investors`, parent_name, Ultimate.Corporate.Parent) %>% filter(`Percent Sought (%)` >= 50)

# collapse multiple merger transactions of a subsidiary into the initial one that brought the company under the parent company
#   add columns with details on internal ownership transfers (i.e. a subsidiary acquired by another subsidiary with the same parent)

parentCompany_acquired_merge  = parentCompany_acquired_merge  %>% group_by(parent_id, Company.Name ) %>% arrange(`All Transactions Announced Date`) %>%
  mutate(internal_transfer_of_ownership = ifelse(row_number() > 1, 1,0)) %>% mutate(latest_transfer_date = max(as.Date(`All Transactions Announced Date`,format='%Y-%m-%d UTC'))) %>%
  mutate(number_of_internal_transfers = n()-1) %>%
  mutate(initial_acquired_date = min(as.Date(`All Transactions Announced Date`,format='%Y-%m-%d UTC'))) %>%
  mutate(latest_acquirer = ifelse(row_number()==n(), `Buyers/Investors`,'')) %>% mutate(latest_acquirer = paste0(latest_acquirer, collapse='')) %>%
  mutate(latest_acquisition_size = ifelse(row_number()==n(),acquired_size, NA_real_)) %>% mutate(latest_acquisition_size = max(latest_acquisition_size,na.rm=T)) %>%
  mutate(latest_acquisition_buyer_id = ifelse(row_number()==n(),`Excel Company ID [Buyers/Investors]`, '')) %>%
  mutate(latest_acquisition_target_id = ifelse(row_number()==n(),`Excel Company ID [Target/Issuer]`, '')) %>%
  mutate(latest_acquisition_transaction_comments = ifelse(row_number()==n(),`Transaction Comments`, '')) %>%
  mutate(latest_acquisition_transaction_comments = paste0(latest_acquisition_transaction_comments, collapse='')) %>%
  mutate(latest_acquisition_target_id = paste0(latest_acquisition_target_id, collapse='')) %>%
  mutate(latest_acquisition_buyer_id = paste0(latest_acquisition_buyer_id, collapse='')) %>%
  filter(row_number()==1) %>% ungroup()
# mZ comment out above, replace with: (seems like theres some blank values, and so calling max/min will warning and return -/+ Inf)
# parentCompany_acquired_merge = parentCompany_acquired_merge %>%
#   mutate(`All Transactions Announced Date` = as.character(`All Transactions Announced Date`)) %>%
#   mutate(`All Transactions Announced Date` = na_if(`All Transactions Announced Date`, "")) %>%
#   mutate(`All Transactions Announced Date` = as.Date(`All Transactions Announced Date`))
# 
# parentCompany_acquired_merge  = parentCompany_acquired_merge  %>%
#   group_by(parent_id, Company.Name) %>%
#   arrange(`All Transactions Announced Date`) %>%
#   mutate(
#     internal_transfer_of_ownership = ifelse(row_number() > 1, 1, 0),
#     latest_transfer_date = if_else(all(is.na(`All Transactions Announced Date`)), as.Date(NA), max(as.Date(`All Transactions Announced Date`, format='%Y-%m-%d UTC'), na.rm = TRUE)),
#     number_of_internal_transfers = n() - 1,
#     initial_acquired_date = if_else(all(is.na(`All Transactions Announced Date`)), as.Date(NA), min(as.Date(`All Transactions Announced Date`, format='%Y-%m-%d UTC'), na.rm = TRUE)),
#     latest_acquirer = ifelse(row_number() == n(), `Buyers/Investors`, ''),
#     latest_acquirer = paste0(latest_acquirer, collapse=''),
#     latest_acquisition_size = ifelse(row_number() == n(), acquired_size, NA_real_),
#     latest_acquisition_size = ifelse(all(is.na(latest_acquisition_size)), NA_real_, max(latest_acquisition_size, na.rm = TRUE)),
#     latest_acquisition_buyer_id = ifelse(row_number() == n(), `Excel Company ID [Buyers/Investors]`, ''),
#     latest_acquisition_target_id = ifelse(row_number() == n(), `Excel Company ID [Target/Issuer]`, ''),
#     latest_acquisition_transaction_comments = ifelse(row_number() == n(), `Transaction Comments`, ''),
#     latest_acquisition_transaction_comments = paste0(latest_acquisition_transaction_comments, collapse=''),
#     latest_acquisition_target_id = paste0(latest_acquisition_target_id, collapse=''),
#     latest_acquisition_buyer_id = paste0(latest_acquisition_buyer_id, collapse='')
#   ) %>%
#   filter(row_number() == 1) %>%
#   ungroup()

#MZ: removed 'Deal Resolution' from list 
parentCompany_acquired_merge =parentCompany_acquired_merge %>% relocate(merge_match_type,number_of_internal_transfers, internal_transfer_of_ownership,`CIQ Transaction ID`, `All Transactions Announced Date`,`Transaction Comments`, Company.Name, `Target/Issuer`, Parent.Company, `Buyers/Investors`, parent_name, Ultimate.Corporate.Parent) %>% filter(`Percent Sought (%)` >= 50) %>% arrange(parent_id, Ultimate.Corporate.Parent, Parent.Company,Company.Name, `All Transactions Announced Date`)


#noparentCompany_acquired_merge  = parentCompany_acquired_merge %>% mutate(`Target/Issuer`=Parent.Company) %>% filter(is.na(`CIQ Transaction ID`))

parentCompany_acquired_merge  = parentCompany_acquired_merge  %>% mutate(merge_match_type='III')


##############
# Combine the matched data with the corporate tree 
##############

# add names of parentCompany_acquired_merge to match subs

x = setdiff(names(parentCompany_acquired_merge), names(matched_subsidiaries_IandII))
# MZ commented: class_of_cols = parentCompany_acquired_merge %>% summarize_all(class)
## MZ changed to:
class_of_cols = parentCompany_acquired_merge %>% reframe(across(everything(), class)) %>% slice(1)
class_of_cols = class_of_cols[which(names(parentCompany_acquired_merge) %in% x)]
make_NA_from_class = function(z){
  # z is class
  y = NULL
  if(z=='character'){
    y = NA_character_
  }
  if(z=='numeric'){
    y = NA_real_
  }
  if(z=='Date'){
   #y = as.Date('garbage', format='%Y-%m-%d')  # generate NA in date class
    y = as.Date(NA_character_, format='%Y-%m-%d')  # mz changed
  }
  return(y)
}
for(i in 1:length(x)){
  tmp_NA = make_NA_from_class(class_of_cols[[1, i]]) #MZ added, ie. originally [i]
  matched_subsidiaries_IandII = matched_subsidiaries_IandII %>% mutate(!!rlang::sym(x[i]) := tmp_NA)
  #make_NA_from_class(class_of_cols[i]))  
}



cnames = intersect(names(matched_subsidiaries_IandII), names(parentCompany_acquired_merge))

matched_subsidiaries_IandII = matched_subsidiaries_IandII[,cnames]
parentCompany_acquired_merge = parentCompany_acquired_merge[,cnames]
merger_history_complete_corporate_tree_dataset_2020 = rbind(matched_subsidiaries_IandII, parentCompany_acquired_merge)

# remove duplicate observations (conditional on Company.Name, Merger transaction, any difference is from small )
# MZ commented out. acquired_size = 13, not 1 or 5236 (5236 = nrow(merger_history_complete_corporate_tree_dataset_2020))
# merger_history_complete_corporate_tree_dataset_2020 =merger_history_complete_corporate_tree_dataset_2020 %>% group_by(parent_id, Company.Name, `CIQ Transaction ID`) %>%
#   arrange(desc(acquired_size), desc(`All Transactions Announced Date`)) %>% filter(row_number()==1)



# chk = merger_history_complete_corporate_tree_dataset_2020 %>% group_by(parent_id, Company.Name) %>% mutate(num_subs = n()) %>% filter(num_subs > 1) %>% arrange(Company.Name, merge_match_type, desc(acquired_size), `All Transactions Announced Date`) %>% relocate(merge_match_type,number_of_internal_transfers, internal_transfer_of_ownership,`CIQ Transaction ID`, `All Transactions Announced Date`,`Deal Resolution`,`Transaction Comments`, Company.Name, `Target/Issuer`, Parent.Company, `Buyers/Investors`, parent_name, Ultimate.Corporate.Parent) %>% filter(`Percent Sought (%)` >= 50) %>% arrange(parent_id, Ultimate.Corporate.Parent, Parent.Company,Company.Name, `All Transactions Announced Date`)
### MZ commented this part out
# chk = merger_history_complete_corporate_tree_dataset_2020 %>% group_by(parent_id, Company.Name) %>% mutate(num_subs = n()) %>% filter(num_subs > 1) %>% arrange(Company.Name, merge_match_type, desc(acquired_size), `All Transactions Announced Date`) %>% relocate(merge_match_type,number_of_internal_transfers, internal_transfer_of_ownership,`CIQ Transaction ID`, `All Transactions Announced Date`,`Transaction Comments`, Company.Name, `Target/Issuer`, Parent.Company, `Buyers/Investors`, parent_name, Ultimate.Corporate.Parent) %>% filter(`Percent Sought (%)` >= 50) %>% arrange(parent_id, Ultimate.Corporate.Parent, Parent.Company,Company.Name, `All Transactions Announced Date`)
# num_dups = nrow(chk)
# num_distinct_subs_w_dups = length(unique(chk$Company.Name))
# 
# 
# if(nrow(chk)>0){
#   
#   print('Note, same subsidiary company matched with multiple M&A transactions. Selecting the more approrpiate one')
#   print(paste('number of non-unique obs = ',num_dups, ' from ', num_distinct_subs_w_dups, ' distinct companies' ))
#   
#   # removing the ones w
#   print('From inspection, duplicate matches come from IB and III, where III is the most appropriate match')
#   merger_history_complete_corporate_tree_dataset_2020 =merger_history_complete_corporate_tree_dataset_2020 %>% group_by(parent_id, Company.Name) %>%
#     mutate(dup_obs_per_companyName = n()-1) %>% filter(dup_obs_per_companyName ==0 | (dup_obs_per_companyName > 0 & merge_match_type=='III')) 
# }


# filter out any multiple mergers matched
#     giving precedence
## MZ took out desc(acquired_size) from arrange list
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  arrange(Company.Name, merge_match_type, `All Transactions Announced Date`)

print(paste('Number of matches of all types:', nrow(merger_history_complete_corporate_tree_dataset_2020), ' which accounts for fraction ', nrow(merger_history_complete_corporate_tree_dataset_2020)/nrow(complete_corporate_tree_dataset_2020)))

# Now add unmatched corporate tree

non_tree_names = setdiff(names(merger_history_complete_corporate_tree_dataset_2020), names(complete_corporate_tree_dataset_2020))
unmatched_corporate_tree = left_join(complete_corporate_tree_dataset_2020, merger_history_complete_corporate_tree_dataset_2020[,c('Company.Name', 'parent_id',non_tree_names)], by=c('Company.Name'='Company.Name', 'parent_id'='parent_id')) %>% filter(is.na(`CIQ Transaction ID`) | is.na(`Target/Issuer`))
unmatched_corporate_tree = unmatched_corporate_tree %>% mutate(merge_match_type='none')
merger_history_complete_corporate_tree_dataset_2020= rbind(merger_history_complete_corporate_tree_dataset_2020, unmatched_corporate_tree)


#noexact_parent_id_match_subsidiaries = noexact_parent_id_match_subsidiaries[,cnames]
#merger_history_complete_corporate_tree_dataset_2020 = rbind(matched_subsidiaries_IandII, parentCompany_acquired_merge,noparentCompany_acquired_merge)
# create acquisition date

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_ind = ifelse(!is.na(`CIQ Transaction ID`),1,0)) %>%
  mutate(acquired_date = substr(Transactions, 7, 17)) %>%
  mutate(acquired_date = as.Date(acquired_date, format='%b-%d-%Y'))
  
# defining merger ind as when CIQ Transaction ID is non missing

merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(merger_ind = acquired_ind)


merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_date = ifelse(is.na(acquired_date), as.Date(`M&A Closed Date`,  format='%Y-%m-%d UTC'),acquired_date)) %>%
  mutate(acquired_date = ifelse(is.na(acquired_date), as.Date(`All Transactions Announced Date`,  format='%Y-%m-%d UTC'),acquired_date)) 

acquired_size = sapply(merger_history_complete_corporate_tree_dataset_2020$Transactions, function(x){
  substr(x,  regexpr('Size ($mm): ', x, fixed=T) + 12, regexpr('Status', x)-3)
})

merger_history_complete_corporate_tree_dataset_2020$acquired_size = acquired_size

#MZ changed $USDmm to CADmm
merger_history_complete_corporate_tree_dataset_2020 = merger_history_complete_corporate_tree_dataset_2020 %>%
  mutate(acquired_size = ifelse(is.na(acquired_size),as.numeric(`Total Transaction Value (CADmm, Historical rate)`) ,as.numeric(acquired_size))) # %>%
  # mutate(acquired_size = ifelse(is.na(acquired_size), as.numeric(`Total Gross Transaction Value ($USDmm, Historical rate)` ),acquired_size))





###############
# summarize
###############


tab_merge_type = table(merger_history_complete_corporate_tree_dataset_2020$merge_match_type, useNA='ifany')
# setwd(paste1(output_dir,'tmp/'))
setwd(paste1(output_dir, '/Output/tmp/'))
stargazer(as.matrix(tab_merge_type), out=paste0(tables_dir,'summary_table_merger_complete_corporate_tree_dataset_2020.tex'),
          title='Type of matches', dep.var.caption ='', label='merger corporate tree matches')

summary(as.Date(merger_history_complete_corporate_tree_dataset_2020$acquired_date, origin='1970-01-01'))
summary(merger_history_complete_corporate_tree_dataset_2020$acquired_size)


summary(merger_history_complete_corporate_tree_dataset_2020$acquired_date)
chk = merger_history_complete_corporate_tree_dataset_2020 %>% mutate(has_date = ifelse(!is.na(acquired_date),1,0))
acq_has_date_table = table(chk$has_date, chk$acquired_ind)
acq_has_date_table = as.matrix(acq_has_date_table)
rownames(acq_has_date_table) = c('No acquisition date', 'Has acquisition date')
colnames(acq_has_date_table) = c('No id\'d merger transaction', 'id\'d merger transaction')
stargazer(acq_has_date_table, out=paste0(tables_dir, 'summary_table_has_date_vs_acquisition_ind.tex'),rownames = F,colnames=F,
          title='Breakdown of subsidiaries with identified acquisition date and identified via merger data', dep.var.caption ='', label='acquisitionVsdate')



save(merger_history_complete_corporate_tree_dataset_2020, file=corporate_tree_data_w_merger_transactions_file)


###############################################
# Use M&A data to get start date as subsidiary
###############################################

################
# Identify additional acquisition dates from the business descriptions of the corporate tree
################

# Extract from business description strings merger date like
# "As of Month DD, YYYY, X was acquired"; "X was formerly a subsidiary.. As a result of Y's acquisition by Z on Month DD, YYYY", etc.

source(paste(code_dir, 'process_CIQ_data/process_capiq_business_description_for_merger_dates.R',sep=''))


################
# Fix the parent_name / parent_id for corporate tree
#   e.g. for some firms the merged entities have different form of parent_name than the 
#     parent names share the ticker
#         NYSE:MMM vs NYSE MMM
################

source(paste(code_dir, 'process_CIQ_data/clean_parent_names_in_corporate_tree.R',sep=''))
#paste(output_dir, 'merger_history_complete_corporate_tree_dataset_2020.RData', sep='')

save(merger_history_complete_corporate_tree_dataset_2020, file=corporate_tree_data_w_merger_transactions_file)
