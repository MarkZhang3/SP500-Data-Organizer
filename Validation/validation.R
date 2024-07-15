# Load required libraries
library(dplyr)
library(readr)

home = "C:/Users/zwend/Documents/UofT Material/work study local"

# Read the files
wrds_ciq_key_file <- paste0(home, "Validation/WRDS_CIQ_Identifiers_Master_Table.csv")
wrds_key <- read_csv(wrds_ciq_key_file)

# ciq_data_file <- paste0(home, "processed_data/merger_history_complete_corporate_tree_dataset.csv")
# ciq_data <- read_csv(ciq_data_file)
load(corporate_tree_data_w_merger_transactions_file)
ciq_data <- 

wrds_file <- './wrds.csv'
wrds <- read_csv(wrds_file)

# Step 1: Merging wrds_key into ciq_data
df <- ciq_data %>%
  left_join(wrds_key, by = c("CIQ Company ID" = "companyid")) %>%
  mutate(has_match = !is.na(symboltypename)) %>%
  mutate(across(c(symboltypename, symbolvalue), ~ifelse(is.na(.), NA_character_, .)))

# Keep track of values in wrds_key that are NOT in ciq_data
unmatched_wrds_key <- wrds_key %>%
  anti_join(ciq_data, by = c("companyid" = "CIQ Company ID"))

# Step 2: Merging wrds into new dataset
unmatched_subsidiaries <- list()

df <- df %>%
  rowwise() %>%
  mutate(
    wrds_match = case_when(
      symboltypename == "S&P GVKey" ~ {
        match <- wrds %>% 
          filter(gvkey == symbolvalue, subsidiary_name == `Company Name`) %>%
          slice(1)
        if(nrow(match) == 0) {
          unmatched_subsidiaries <<- c(unmatched_subsidiaries, `Company Name`)
          NA
        } else {
          list(match)
        }
      },
      symboltypename == "SEC CIK number" ~ {
        match <- wrds %>% 
          filter(cik == symbolvalue, subsidiary_name == `Company Name`) %>%
          slice(1)
        if(nrow(match) == 0) {
          unmatched_subsidiaries <<- c(unmatched_subsidiaries, `Company Name`)
          NA
        } else {
          list(match)
        }
      },
      TRUE ~ NA
    )
  ) %>%
  ungroup()

# Convert the list column to regular columns
df <- df %>%
  unnest(wrds_match, keep_empty = TRUE)

# Print summary
cat("Number of unmatched wrds_key entries:", nrow(unmatched_wrds_key), "\n")
cat("Number of unmatched subsidiaries:", length(unmatched_subsidiaries), "\n")