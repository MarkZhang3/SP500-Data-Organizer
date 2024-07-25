# wrds_ciq_key <- read.csv("C:/Users/zwend/Documents/UofT Material/work study local/Validation/WRDS_CIQ_Identifiers_Master_Table.csv")
# save(wrds_ciq_key, file="C:/Users/zwend/Documents/UofT Material/work study local/Validation/wrds_ciq_keys.RData")

# wrds_ciq_key$start_date <- as.Date(wrds_ciq_key$startdate, format = "%Y-%m-%d")
# wrds_ciq_key$end_date <- as.Date(wrds_ciq_key$enddate, format = "%Y-%m-%d")
# 
# wrds_ciq_key <- wrds_ciq_key[wrds_ciq_key$symboltypename %in% c("SEC CIK number", "S&P GVKey"), ]
# save(wrds_ciq_key, file="C:/Users/zwend/Documents/UofT Material/work study local/Validation/wrds_ciq_keys_cleaned.RData")

load("~/UofT Material/work study local/processed_data/firm_data/subsidiary_dataset.RData")
# ^ this loads dataset called subsidiary_dataset. `CIQ.Company.ID` is one of its columns 

load("~/UofT Material/work study local/Validation/wrds_ciq_keys_cleaned.RData")
# ^ this loads a dataset called wrds_ciq_keys. this has columns 
# ```> names(wrds_ciq_key)
# [1] "companyid"             "companyname"           "symbolid"              "symboltypeid"         
# [5] "symboltypecat"         "symboltypename"        "symbolvalue"           "startdate"            
# [9] "enddate"               "activeflag"            "objectid"              "objecttypeid"         
# [13] "objecttypename"        "securityname"          "primaryflag"           "exchangeid"           
# [17] "exchangename"          "tradingitemstatusname" "primaryflag_trd"       "start_date"           
# [21] "end_date" ```


# Ensure companyid and CIQ.Company.ID are character to handle leading zeros
subsidiary_dataset$CIQ.Company.ID <- as.character(subsidiary_dataset$CIQ.Company.ID)
wrds_ciq_key$companyid <- as.character(wrds_ciq_key$companyid)

# inner join causes the companyid column to not follow 
wrds_ciq_key <- wrds_ciq_key %>%
  mutate(CIQ_Company_ID = companyid)

# Join the datasets
joined_dataset <- subsidiary_dataset %>%
  inner_join(wrds_ciq_key, by = c("CIQ.Company.ID" = "CIQ_Company_ID"))

# Rows in wrds_ciq_key that were unable to join
wrds_ciq_key_unmatched <- wrds_ciq_key %>%
  anti_join(subsidiary_dataset, by = c("CIQ_Company_ID" = "CIQ.Company.ID"))

# Rows in subsidiary_dataset that were unable to join
subsidiary_dataset_unmatched <- subsidiary_dataset %>%
  anti_join(wrds_ciq_key, by = c("CIQ.Company.ID" = "CIQ_Company_ID"))

# View the resulting datasets
# View(joined_dataset)
# View(wrds_ciq_key_unmatched)
# View(subsidiary_dataset_unmatched)

# print the number of rows in each dataset for verification
message("Intersection (successful joins): ", nrow(joined_dataset))
message("Unmatched rows in wrds_ciq_key: ", nrow(wrds_ciq_key_unmatched))
message("Unmatched rows in subsidiary_dataset: ", nrow(subsidiary_dataset_unmatched))

load("~/UofT Material/work study local/Validation/wrds.RData")
# ^ this loads a dataset called wrds. this has columns 
# ```> names(wrds)
# [1] "cik"             "gvkey"           "accession"       "parent_name"     "fdate"           "rdate"          
# [7] "secpdate"        "subsidiary_name" "clean_company"   "location"        "country_code"    "subdiv_code"    
# [13] "form"            "extype"          "ex21url"         "formurl"         "start_year"      "end_year"  ```

# Rename columns in wrds to prefix with "WRDS"
wrds <- wrds %>%
  rename_with(~ paste0("WRDS_", .), everything())

# Ensure WRDS_gvkey and WRDS_cik are character to handle leading zeros
wrds$WRDS_gvkey <- as.character(wrds$WRDS_gvkey)
wrds$WRDS_cik <- as.character(wrds$WRDS_cik)

# Separate joined_dataset based on symboltypename
joined_gvkey <- joined_dataset %>%
  filter(symboltypecat == "gvkey") %>%
  inner_join(wrds, by = c("symbolvalue" = "WRDS_gvkey"))
# joined_gvkey <- joined_dataset %>%
#   filter(symboltypecat == "gvkey") %>%
#   left_join(wrds, by = c("symbolvalue" = "WRDS_gvkey"))

joined_cik <- joined_dataset %>%
  filter(symboltypecat == "cik") %>%
  inner_join(wrds, by = c("symbolvalue" = "WRDS_cik"))
# joined_cik <- joined_dataset %>%
#   filter(symboltypecat == "cik") %>%
#   left_join(wrds, by = c("symbolvalue" = "WRDS_cik"))

# Combine the two parts for successful joins
final_joined_dataset <- bind_rows(joined_gvkey, joined_cik)

# Rows in joined_dataset that were unable to join wrds
joined_dataset_unmatched_gvkey <- joined_dataset %>%
  filter(symboltypecat == "gvkey") %>%
  anti_join(wrds, by = c("symbolvalue" = "WRDS_gvkey"))

joined_dataset_unmatched_cik <- joined_dataset %>%
  filter(symboltypecat == "cik") %>%
  anti_join(wrds, by = c("symbolvalue" = "WRDS_cik"))

# Combine the unmatched parts
joined_dataset_unmatched <- bind_rows(joined_dataset_unmatched_gvkey, joined_dataset_unmatched_cik)

# Rows in wrds that were unable to join joined_dataset
wrds_unmatched_gvkey <- wrds %>%
  anti_join(joined_dataset %>% filter(symboltypecat == "gvkey"), by = c("WRDS_gvkey" = "symbolvalue"))

wrds_unmatched_cik <- wrds %>%
  anti_join(joined_dataset %>% filter(symboltypecat == "cik"), by = c("WRDS_cik" = "symbolvalue"))

# Combine the unmatched parts for wrds
wrds_unmatched <- bind_rows(wrds_unmatched_gvkey, wrds_unmatched_cik)



## Uncomment to write to CSVs 
wd <- 'C:/Users/zwend/Documents/UofT Material/work study local/Validation/'
# write.csv(final_joined_dataset, paste0(wd, "wrds_ciq_intersection.csv"), row.names = FALSE)
# write.csv(joined_dataset_unmatched, paste0(wd, "ciq_unmatched.csv"), row.names = FALSE)
# write.csv(wrds_unmatched, paste0(wd, "wrds_unmatched.csv"), row.names = FALSE)


# get unique count of subsidiary_dataset$Company.Name vs wrds$subsdiary_name group by cik,gvkey,parent_name
# get fuzzy string match for each Company.Name to subsidiary_name and keep only the best match from subsidiary_name for each Company.Name - tabluation of scores above 80% string metric,   
# https://cran.r-project.org/web/packages/fedmatch/vignettes/Fuzzy-matching.html
# https://cran.r-project.org/web/packages/stringdist/vignettes/RJournal_6_111-122-2014.pdf
# metrics: Jarccard, Jarro-winkler, “Jw”, soundex and levinschtein



final_joined_dataset_rearranged <- final_joined_dataset %>%
  select(Company.Name, Parent.Company, parent_name, WRDS_parent_name, WRDS_subsidiary_name, everything())

write.csv(final_joined_dataset_rearranged, paste0(wd, "wrds_ciq_intersection_rearranged.csv"), row.names=FALSE)


