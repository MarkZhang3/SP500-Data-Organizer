########################################
# Raw processing of Capital IQ merger data
########################################
# Restored Aug 4, 2023 from Feb 17, 2021 commit "added back fuzzy disambig code into prepare.."


#######################################################
# Add Capital IQ merger data
########################################################
# try to determine when a firm became a subsidiary if it was acquired (ie if it ever was not owned by the current parent)
# if we want the capital IQ ids / CIKs of the acquired firms 
# pull up capital IQ merger data, try to merge based on buyer CID, and M&A closed date
#   main goal is to get the excel ID of the seller (target) so that I can track the firm 


#####################
# Load M&A data
#####################

print("starting merger data processing")

require(readxl)
setwd(merger_dir)
merger_files = list.files()
merger_files = merger_files[grepl('xls', merger_files)]
col_type_list_merger_by_date = c('date', 'text', 'text', 'text', 'numeric','text','text','text','numeric','numeric',
                                 'text','numeric','numeric','numeric','numeric','numeric','numeric','numeric','text',
                                 'text','text','numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'numeric',
                                 'numeric','text','text','text','text','text', 'numeric','numeric','date','text',
                                 'text','date','date','date','date','date','date','date','text','text',
                                 rep('numeric',16),rep('text',6),'numeric','numeric','numeric')


merger_data_list = vector('list', length(merger_files))
for(fn in merger_files){
  tmp <- read_excel(fn, skip=7, range='A8:BV10010', col_types=col_type_list_merger_by_date)  
  merger_data_list[[which(fn == merger_files)]] = tmp
}
chk = sapply(merger_data_list, ncol)
stopifnot(length(unique(chk))==1)

capiq_merger_dataset_2020 = do.call(rbind, merger_data_list)

# 
nms = names(capiq_merger_dataset_2020)

# remove the IQ in the identifiers


IDcols = nms[grepl('Excel Company ID', nms)]
capiq_merger_dataset_2020=capiq_merger_dataset_2020  %>%
  mutate_at(IDcols, function(x) gsub('IQ','',x))

capiq_merger_dataset_2020 = capiq_merger_dataset_2020 %>% distinct()

save(capiq_merger_dataset_2020, file=initial_collated_merger_data_file)
    # initial_collated_merger_data_file = paste(output_dir, 'capiq_merger_dataset_2020.RData',sep=''))

print(paste("merger data processing complete/saved in ", initial_collated_merger_data_file))

# MZ: changed 'M&A Announced Date' to 'M&A Announced Date (Including Bids and Letters of Intent)'
chk = capiq_merger_dataset_2020 %>% mutate(ann_year = lubridate::year(`M&A Announced Date (Including Bids and Letters of Intent)`)) %>%  
  group_by(ann_year) %>% mutate(cash_offered_ind = ifelse(`Consideration Offered`=='Cash',1,0)) %>%
  summarize(cash_pct = mean(`Cash % of Consideration (%)`,na.rm=T), cash_offered_share = sum(cash_offered_ind)/n())

##############
# Merger sellof data
#############

# download and combine the merger sellof data 
#   note the merger data here is for any transaction value > 1.5 million
#   the selloff data here is for any transaction value > 15 million
#     -I used size here to corroborate missing data from the use of Announced dates in the above search

print("starting merger selloff data")
setwd(mergerSellof_dir)
merger_files = list.files()
merger_files = merger_files[grepl('xls', merger_files)]

col_type_list = c('date', 'text', 'text', 'text', 'numeric','text','text','text','numeric','numeric','text','numeric','numeric','numeric','numeric','numeric','numeric','numeric','text','text','text','numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'numeric','numeric','text','text','text','text','text', 'numeric','numeric','text','text','date','date','date','date','date','date',
                  'text','text',rep('numeric',16),rep('text',7),'numeric','numeric','numeric','numeric','text','text')

require(svMisc)                  
merger_data_list = vector('list', length(merger_files))
for(fn in merger_files){
  #svMisc::progress(which(fn==merger_files), progress.bar=T)
  #Sys.sleep(.01)
  tmp <- readxl::read_excel(fn, skip=7, range='A8:BX10010', col_types=col_type_list)
  # date_cols = names(tmp)[grepl('Date', names(tmp))]
  # 
  # #tmp = as.data.frame(tmp)
  # tmp_fun = function(x){
  #   x = as.numeric(x)
  #   #x = lapply(x, function(y) as.Date(y, origin='1899-12-30'))
  #   #x = do.call(c, x)
  #   x = as.Date(x, origin='1899-12-30')
  # }
  # tmp = tmp %>% mutate(across(date_cols, tmp_fun))
  i = which(fn == merger_files)
  merger_data_list[[which(fn == merger_files)]] = tmp
  print(which(fn == merger_files))
  Sys.sleep(.01)
  flush.console()
  if(i %% 20 == 0){
    save(merger_data_list, file=paste(output_dir, 'tmp_merger_data_list.RData',sep=''))
  }
}
chk = sapply(merger_data_list, ncol)
stopifnot(length(unique(chk))==1)

capiq_mergerSellOff_dataset_2020 = do.call(rbind, merger_data_list)

capiq_mergerSellOff_dataset_2020 = capiq_mergerSellOff_dataset_2020 %>% distinct()
nms = names(capiq_mergerSellOff_dataset_2020)


# remove the IQ in the identifiers
IDcols = nms[grepl('Excel Company ID', nms)]
capiq_mergerSellOff_dataset_2020=capiq_mergerSellOff_dataset_2020  %>%
  mutate_at(IDcols, function(x) gsub('IQ','',x))

save(capiq_mergerSellOff_dataset_2020, file=CIQ_selloff_dataset_file)
       # CIQ_selloff_dataset_file = paste(output_dir, 'firm_data/capiq_mergerSellOff_dataset_2020.RData',sep='')

print(paste("selloff data processing complete/saved in ", CIQ_selloff_dataset_file))

# checking for gaps
#chk = capiq_mergerSellOff_dataset_2020 %>% filter(`Total Transaction Value ($USDmm, Historical rate)`> 45 & `Total Transaction Value ($USDmm, Historical rate)`< 60)
#hist(log(capiq_mergerSellOff_dataset_2020$`Total Transaction Value ($USDmm, Historical rate)`))

# combine the merger data here with the capiq_merger_dataset above

capiq_mergerSellOff_dataset_2020_mergersOnly = capiq_mergerSellOff_dataset_2020 %>% filter(`Transaction Types`=="Merger/Acquisition")
capiq_mergerSellOff_dataset_2020_mergersOnly = capiq_mergerSellOff_dataset_2020_mergersOnly %>% mutate(selloff_ind =1)
capiq_merger_dataset_2020 = capiq_merger_dataset_2020 %>% select(-`M&A Announced Date (Including Bids and Letters of Intent)`)
# MZ commented out: capiq_merger_dataset_2020 = capiq_merger_dataset_2020 %>% select(-`M&A Announced Date`)

# re-order the selloff data to be commensurate with the the original merger data downloaded
# MZ: Rename columns
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Total Transaction Value ($USDmm, Historical rate)"] <- "Total Transaction Value (CADmm, Historical rate)"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Spin-off/Split-off Announced Date (Including Letters of Intent)"] <- "M&A Announced Date (Including Bids and Letters of Intent)"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Spin-off/Split-off Definitive Agreement Date"] <- "M&A Definitive Agreement Date"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Spin-off/Split-off Closed Date"] <- "M&A Closed Date"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Spin-off/Split-off Cancelled Date"] <- "M&A Cancelled Date"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Spin-off/Split-off Effective Date"] <- "M&A Effective Date"
names(capiq_mergerSellOff_dataset_2020_mergersOnly)[names(capiq_mergerSellOff_dataset_2020_mergersOnly) == "Net Assumed Liabilities ($USDmm, Historical rate)"] <- "Net Assumed Liabilities (CADmm, Historical rate)"

# capiq_mergerSellOff_dataset_2020_mergersOnly = capiq_mergerSellOff_dataset_2020_mergersOnly[,names(capiq_merger_dataset_2020)]
# capiq_merger_dataset_2020 = capiq_merger_dataset_2020[,names(capiq_mergerSellOff_dataset_2020_mergersOnly)]
# full_capiq_merger_dataset_2020 = rbind(capiq_merger_dataset_2020,capiq_mergerSellOff_dataset_2020_mergersOnly)
#MZ change to: 
# Identify all unique columns from both dataframes
all_columns <- union(names(capiq_mergerSellOff_dataset_2020_mergersOnly), names(capiq_merger_dataset_2020))

# Add missing columns with NA values to capiq_mergerSellOff_dataset_2020_mergersOnly
missing_columns_1 <- setdiff(all_columns, names(capiq_mergerSellOff_dataset_2020_mergersOnly))
capiq_mergerSellOff_dataset_2020_mergersOnly[missing_columns_1] <- NA

# Add missing columns with NA values to capiq_merger_dataset_2020
missing_columns_2 <- setdiff(all_columns, names(capiq_merger_dataset_2020))
capiq_merger_dataset_2020[missing_columns_2] <- NA

# Ensure both dataframes have the same column order
capiq_mergerSellOff_dataset_2020_mergersOnly <- capiq_mergerSellOff_dataset_2020_mergersOnly[, all_columns]
capiq_merger_dataset_2020 <- capiq_merger_dataset_2020[, all_columns]

# Combine the dataframes using rbind
full_capiq_merger_dataset_2020 <- rbind(capiq_mergerSellOff_dataset_2020_mergersOnly, capiq_merger_dataset_2020)
#### end of change


full_capiq_merger_dataset_2020_w_cancelled_transactions= full_capiq_merger_dataset_2020
save(full_capiq_merger_dataset_2020_w_cancelled_transactions ,file=full_capiq_merger_dataset_2020_w_cancelled_transactions_file)
  #full_capiq_merger_dataset_2020_w_cancelled_transactions_file =  paste(output_dir, 'firm_data/full_capiq_merger_dataset_2020_w_cancelled_transactions.RData',sep='')
rm(full_capiq_merger_dataset_2020_w_cancelled_transactions)

full_capiq_merger_dataset_2020 = full_capiq_merger_dataset_2020 %>% filter(!`Transaction Status`=='Cancelled') %>% filter(!`Transaction Status`=='Announced')
full_capiq_merger_dataset_2020 = unique(full_capiq_merger_dataset_2020)
save(full_capiq_merger_dataset_2020 , file=CIQ_merger_dataset_file)
print(paste("combined merger / selloff data processing complete/saved in ", CIQ_merger_dataset_file))

  #CIQ_merger_dataset_file =paste(output_dir, 'firm_data/full_capiq_merger_dataset_2020.RData',sep='')
