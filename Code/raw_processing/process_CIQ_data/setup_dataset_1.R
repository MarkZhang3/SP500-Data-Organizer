library(dplyr)
library(lubridate)

load("~/UofT Material/work study local/processed_data/firm_data/subsidiary_dataset.RData")

# Add start_year column
subsidiary_dataset <- subsidiary_dataset %>%
  mutate(start_year = year(consolidate_acquisition_date))

# Initialize end_date column as NA of type Date
subsidiary_dataset <- subsidiary_dataset %>%
  mutate(end_date = as.Date(NA))

# Function to update end_date
# lag - access all previous values in the column - ok? 
subsidiary_dataset <- subsidiary_dataset %>%
  group_by(Company.Name) %>%
  arrange(`All Transactions Announced Date`) %>%
  mutate(
    end_date = lag(`All Transactions Announced Date`, default = NA, order_by = `All Transactions Announced Date`)
  ) %>%
  ungroup()

# Update end_date with filtering for different Parent.Company
subsidiary_dataset <- subsidiary_dataset %>%
  group_by(Company.Name) %>%
  arrange(`All Transactions Announced Date`) %>%
  mutate(
    end_date = ifelse(
      Parent.Company != lag(Parent.Company, default = NA),
      lag(`All Transactions Announced Date`, default = NA),
      NA
    )
  ) %>%
  ungroup()

# Reorganize columns
subsidiary_dataset <- subsidiary_dataset %>%
  select(Company.Name, company_name, Parent.Company, parent_name, start_year, consolidate_acquisition_date, end_date, everything())

# View the updated dataset
View(subsidiary_dataset)

key_columns <- c("Company.Name", "company_name", "Parent.Company", "parent_name", "start_year", "consolidate_acquisition_date", "end_date")

if (nrow(unique(subsidiary_dataset[, key_columns])) == nrow(unique(subsidiary_dataset))) {
  message("All rows are uniquely identified by the key columns.")
} else {
  non_unique_rows <- subsidiary_dataset %>%
    group_by(across(all_of(key_columns))) %>%
    filter(n() > 1) %>%
    ungroup()
  View(non_unique_rows)
}
