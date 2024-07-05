########################################
# Raw processing of Capital IQ corporate tree / investment files
########################################
# Restored Aug 4, 2023, from Feb 11, 2021 commit "updated sub_dir to new reference in process.."

setwd(subsidiary_data_dir)

# 1) loop through the corporate tree's of each parent company
#   i) read in first sheet of excel row 7 onwards
#   ii) create new column of parent company name / CIQ

# 2) Append the 

## In this file, changed Relationship.Type to "Relationship Type" for corporate trees and investment
## changed Company.Name to "parent_name" for investment 

raw_files = list.files()
raw_tree_files = raw_files[grepl('Structure Tree', raw_files)] # get corporate tree files
raw_invest_files = raw_files[grepl('Direct Investments', raw_files)] # get Investments
raw_product_files = raw_files[grepl('Product', raw_files)] # get product files

# MZ: switched it to use readxl, commented out all gdata usage
# require(readxl)
## find setting in gdata to handle special characters upon read 
require(gdata)
# library(gdata)
perl <- "C:/strawberry/perl/bin/perl.exe"
subs_data_list = vector('list', length(raw_tree_files))
for(i in 1:length(raw_tree_files)){
  #MZ: added encoding as latin1
  tmp = gdata::read.xls(raw_tree_files[i], perl=perl, sheet=1,skip = 4, encoding = "latin1")
  parent_name = tmp[1,1]
  parent_id =  tmp[1,2]
  tmp$parent_name = parent_name
  tmp$parent_id = parent_id
  subs_data_list[[i]] = tmp
}

corporate_tree_dataset_2020 = do.call(rbind, subs_data_list)
print(names(corporate_tree_dataset_2020))


# fix the handful of files / observations with corrupted spacing 
# MZ: changed Relationship.Type in nchar() to just 'Relationship Type'
prob_obs = corporate_tree_dataset_2020 %>% filter(nchar('Relationship Type')>nchar('Current Subsidiary/Operating Unit'))
print(unique(prob_obs$parent_name))
corporate_tree_dataset_2020 = corporate_tree_dataset_2020 %>%filter(nchar('Relationship Type')<=nchar('Current Subsidiary/Operating Unit'))

missing_obs = read_csv('ProctorGambleSupplementCorpTree.csv')
tmp_pad = matrix('', nrow(missing_obs), ncol=ncol(corporate_tree_dataset_2020) - ncol(missing_obs))
tmp_pad = as.data.frame(tmp_pad)

missing_obs = data.frame(missing_obs, tmp_pad)
names(missing_obs) = names(corporate_tree_dataset_2020) # overwrite missing_obs names with corportae tree 
corporate_tree_dataset_2020 = rbind(corporate_tree_dataset_2020, missing_obs)

corporate_tree_dataset_2020 = corporate_tree_dataset_2020 %>%filter(!grepl('*denotes proprietary relationship information', Company.Name))

corporate_tree_dataset_2020 = corporate_tree_dataset_2020 %>% arrange(Ultimate.Corporate.Parent)
save(corporate_tree_dataset_2020, file =corporate_tree_data_file) 
print('process_capitalIQ_corporate_tree_raw_data: finished corporate_tree_dataset_2020')

##############################
# Get merged entities from Direct Investment Tables
##############################


# Now append  merged corporate entities () to the corporate tree
#   i) extract company name from row 5 preceding `>`
#   ii) skip 18 rows, to read rest
#   iii) append relationship type == merged entity to corporate tree
#       - use last investment date as start of subsidiary status
#   iv) merge current operating subsidiary to corporate tree (add website and business description)
#       columns which are relevant: 

options(stringsAsFactors = F)
invest_data_list = vector('list', length(raw_invest_files))
for(i in 1:length(raw_invest_files)){
  tryCatch({
    tmp = gdata::read.xls(raw_invest_files[i], sheet=1,skip = 11,fileEncoding="latin1") # change skip = 11 to skip = 15
    
    # cat("File read successfully:", raw_invest_files[i], "\n")
    # col_count <- sapply(tmp, length)
    # unique_col_counts <- unique(col_count)
    # 
    # if (length(unique_col_counts) != 1) {
    #   inconsistent_rows <- which(col_count != unique_col_counts[1])
    #   stop(paste("Inconsistent number of columns in file:", raw_invest_files[i], "at rows:", paste(inconsistent_rows, collapse = ", ")))
    # }
    
    # read firm name from file name
    tmp_name =  substr(raw_invest_files[i], 1,regexpr('Investment Analysis', raw_invest_files[i]))
    tmp$parent_name = rep(tmp_name, nrow(tmp))
    invest_data_list[[i]] = tmp
  }, error = function(e) {
    cat("Error in file:", raw_invest_files[i], "\n")
    cat("Error message:", e$message, "\n")
  })
}

chk = lapply(invest_data_list, ncol)

indx = c(seq(1,23), ncol(invest_data_list[[1]]))
invest_history_dataset_2020 = invest_data_list[[1]]
invest_history_dataset_2020 = invest_history_dataset_2020[,indx]
print(str(invest_history_dataset_2020)) # mz added

names(invest_history_dataset_2020)[22]='Exit.Date'
names(invest_history_dataset_2020)[23]='Investor.Holding.Period..yrs.'
base_names = names(invest_history_dataset_2020)
print(str(invest_history_dataset_2020))

for(i in 2:length(raw_invest_files)){
  indx = c(seq(1,23), ncol(invest_data_list[[i]]))
  tmp = invest_data_list[[i]][,unique(indx)] # MZ: added unique() around indx
  names(tmp)[22]='Exit.Date'
  names(tmp)[23]='Investor.Holding.Period..yrs.'
  
  if(length(setdiff(base_names, names(tmp)))==0){
    
    invest_history_dataset_2020 = rbind(invest_history_dataset_2020, tmp)  
  } else {
    # re-arrange columns so that they line up with 
    tmp = invest_data_list[[i]][,indx]
    tmp_names = names(tmp)
    common_names = intersect(base_names,tmp_names)
    tmp_core = tmp[,common_names]
    
    # pad 
    residual_names = setdiff(base_names,common_names)
    if(length(residual_names)>0){
      tmp_pad = matrix(NA_character_, nrow=nrow(tmp_core), ncol=length(residual_names)) 
      tmp_new = data.frame(tmp_core, tmp_pad)
      k = ncol(tmp_new)
      names(tmp_new)[seq(k-length(residual_names)+1,k)] = residual_names
      tmp_new = tmp_new[,base_names]
    } else {
      tmp_new = tmp_core
    }
    
    invest_history_dataset_2020 = rbind(invest_history_dataset_2020, tmp_new)  
    
  }
  
}

# drop trailing rows in each excel file

invest_history_dataset_2020 = invest_history_dataset_2020 %>% filter(!grepl('* denotes that the relationship is proprietary', X)) # MZ - change Company.Name to X 

#save(invest_history_dataset_2020, file=paste(output_dir,'invest_history_dataset_2020.RData',sep=''))

###########
# fix the < 50 mis-read observations 
##########

prob_obs = invest_history_dataset_2020 %>% filter(nchar('Prior.Subsidiary.Operating.Unit')>nchar('Current Subsidiary/Operating Unit'))# MZ: changed Relationship.Type in nchar to Prior.Subsidiary.Operating.Unit 
print(unique(prob_obs$parent_name))

# drop these prob obs
invest_history_dataset_2020 = invest_history_dataset_2020 %>%  filter(nchar(Prior.Subsidiary.Operating.Unit)<=nchar('Current Subsidiary/Operating Unit')) # MZ: changed Relationship.Type in nchar to Prior.Subsidiary.Operating.Unit 

# load supplement
# MZ commented out, I don't have this csv
# missing_obs = read_csv('Novartis_supplementDirectInv.csv')
# if(ncol(missing_obs) > ncol(invest_history_dataset_2020)){
#   tmp_pad = matrix('', nrow=nrow(missing_obs), ncol= ncol(missing_obs) - ncol(invest_history_dataset_2020)) 
#   tmp_pad = as.data.frame(tmp_pad)
#   missing_obs = data.frame(missing_obs, tmp_pad)
# } else {
#   missing_obs = missing_obs[,-c(ncol(missing_obs),ncol(missing_obs)-1)]
# }

# names(missing_obs) = names(invest_history_dataset_2020) # overwrite missing_obs names with corportae tree (has identical columns, just possibly slight variation in labels)
# missing_obs = missing_obs[,names(invest_history_dataset_2020)] # select only the non-missing columns (= ones overlapping with invest_history)
# invest_history_dataset_2020 = rbind(invest_history_dataset_2020, missing_obs)


relationship_types = c('Current Investment', 'Current Subsidiary/Operating Unit', 'Merged Entity', 'Pending Acquisition/Investment')
prob_obs = invest_history_dataset_2020 %>% filter(!(Prior.Subsidiary.Operating.Unit %in% relationship_types)) # MZ: changed Relationship.Type in nchar to Prior.Subsidiary.Operating.Unit 

invest_history_dataset_2020 = invest_history_dataset_2020 %>% filter((Prior.Subsidiary.Operating.Unit %in% relationship_types)) # MZ: changed Relationship.Type in nchar to Prior.Subsidiary.Operating.Unit 
#save(invest_history_dataset_2020, file=paste(output_dir,'invest_history_dataset_2020.RData',sep=''))

#############
# now combine the investment history dataset with the current (2020) corporate tree
#   basically want to add the merged entitites into the corporate tree 
############

invest_history_dataset_2020 = invest_history_dataset_2020 %>% filter(Prior.Subsidiary.Operating.Unit=='Merged Entity')

blank_cols = grepl('X\\.[0-9]', names(corporate_tree_dataset_2020))
blank_cols = which(blank_cols)
residual_cols = setdiff(names(invest_history_dataset_2020), names(corporate_tree_dataset_2020))
# replace the X COLUMNS WITH THE INVESTMENT COLUMNS WHICH DON'T APPEAR IN THE CORPORATE TREE  
names(corporate_tree_dataset_2020)[blank_cols[1:length(residual_cols)]] = residual_cols

blank_cols = grepl('X\\.[0-9]', names(corporate_tree_dataset_2020))
blank_cols = which(blank_cols)
corporate_tree_dataset_2020 = corporate_tree_dataset_2020[,-blank_cols]
corporate_tree_dataset_2020 = corporate_tree_dataset_2020[,-which(names(corporate_tree_dataset_2020)=='X')]

#MZ added renaming the columns
invest_dataset_names <- c('Company Name', 'Relationship Type', 'Current Level Geography', 'Primary Industry',
                          'Last Investment Date', 'Pre-Money Valuation ($mm)', 'Post-Money Valuation',
                          'LTM Ttotal Rev. ($mm)', 'LFQ Total Assests ($mm)', 'LFQ Total Debt ($mm)', 'Period End Date',
                          'Website', 'Buisness Description', 'Investment Coverage', 'Total Investment ($mm)', 
                          'Expected Exit Date', 'Percent Owned %', 'Return on Investment', 'Controlling Interest', 'Investor Notes', 
                          'Transactions', 'Exit.Date', 'Investor.Holding.Period..yrs.', 'parent_name')
colnames(invest_history_dataset_2020) <- invest_dataset_names

common_columns <- intersect(names(invest_history_dataset_2020), names(corporate_tree_dataset_2020))
invest_history_dataset_2020 <- invest_history_dataset_2020[, common_columns, drop = FALSE]

# pad invest_history_dataset_2020
# MZ added if condition bc n_extra_cols is negative 
n_extra_cols = ncol(corporate_tree_dataset_2020) - ncol(invest_history_dataset_2020)+1
if (n_extra_cols > 0) {
  tmp_pad <- matrix('', nrow = nrow(invest_history_dataset_2020), ncol = n_extra_cols)
  residual_cols <- setdiff(names(corporate_tree_dataset_2020), names(invest_history_dataset_2020))
  tmp_pad <- as.data.frame(tmp_pad)
  names(tmp_pad) <- residual_cols
  invest_history_dataset_2020 <- data.frame(invest_history_dataset_2020, tmp_pad)
}


invest_history_dataset_2020 = invest_history_dataset_2020[,names(corporate_tree_dataset_2020)]
complete_corporate_tree_dataset_2020 = rbind(corporate_tree_dataset_2020,invest_history_dataset_2020)

# fix minor spacing issue for one remaining observation
# complete_corporate_tree_dataset_2020[which(grepl('Electrosila',complete_corporate_tree_dataset_2020$Relationship.Type)),which(names(complete_corporate_tree_dataset_2020)=='Stake.Type')] = 'Majority'
# complete_corporate_tree_dataset_2020[which(grepl('Electrosila',complete_corporate_tree_dataset_2020$Relationship.Type)),which(names(complete_corporate_tree_dataset_2020)=='Relationship.Type')] = 'Current Subsidiary/Operating Unit'

# for the parent companies with blank company name, but populated parent name, replace the company name with parent
complete_corporate_tree_dataset_2020 = complete_corporate_tree_dataset_2020 %>% 
  mutate(CIQ.Company.ID=iconv(CIQ.Company.ID, 'UTF-8', 'UTF-8', sub='')) %>%
  mutate(Company.Name = iconv(Company.Name,  'UTF-8', 'UTF-8', sub=''))

#complete_corporate_tree_dataset_2020 = complete_corporate_tree_dataset_2020 %>% 
#  mutate(nchar_id = nchar(CIQ.Company.ID, allowNA=T)) %>% filter(!is.na(nchar_id)) %>% select(-nchar_id)

complete_corporate_tree_dataset_2020 = complete_corporate_tree_dataset_2020 %>% 
  mutate(CIQ.Company.ID=ifelse(nchar(Company.Name)<3 & nchar(CIQ.Company.ID) < 3, parent_id, CIQ.Company.ID)) %>%
  mutate(Company.Name=ifelse(nchar(Company.Name)<3, parent_name,Company.Name ))

complete_corporate_tree_dataset_2020 = complete_corporate_tree_dataset_2020 %>% mutate(CIQ.Company.ID = ifelse(is.na(CIQ.Company.ID), '',CIQ.Company.ID))

save(complete_corporate_tree_dataset_2020, file=corporate_tree_data_file)
	# paste(output_dir, 'firm_data/complete_corporate_tree_dataset_2020.RData',sep=''))
