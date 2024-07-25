library(dplyr)
library(lubridate)

wrds_raw <- read.csv("C:/Users/zwend/Documents/UofT Material/work study local/Validation/all_wrds.csv")
# ^ change this to wrds_test.csv for testing. You should look for observation with 
# coname = INTERNATIONAL GAME TECHNOLOGY; subsidiary_name = IGT North America, I inserted a repeat obs with only 1 date change
# which should be removed by this script

# Rename columns
wrds_raw <- wrds_raw %>% rename(parent_name = coname)

# Convert date columns to Date type if they are not already
wrds_raw <- wrds_raw %>%
  mutate(across(c(fdate, rdate, secpdate), as.Date))

# Aggregate to find start_year and end_year
# group by the subsidiary_name, parent_name, then look for min/max of years 
aggregated_wrds <- wrds_raw %>%
  group_by(subsidiary_name, parent_name) %>%
  summarise(
    start_year = year(min(c(fdate, rdate, secpdate), na.rm = TRUE)),
    end_year = year(max(c(fdate, rdate, secpdate), na.rm = TRUE)),
    .groups = 'drop'
  )

# Merge back to find the rows with start_year and end_year dates
merged_wrds <- wrds_raw %>%
  left_join(aggregated_wrds, by = c("subsidiary_name", "parent_name"))

# Filter rows to keep only those corresponding to start_year and end_year
result_wrds <- merged_wrds %>%
  filter(
    year(fdate) == start_year | year(rdate) == start_year | year(secpdate) == start_year |
      year(fdate) == end_year | year(rdate) == end_year | year(secpdate) == end_year
  )

wrds <- result_wrds %>% distinct(subsidiary_name, parent_name, cik, gvkey, .keep_all = TRUE)
save(wrds, file="C:/Users/zwend/Documents/UofT Material/work study local/Validation/wrds.RData")

# Write the result to a CSV file (if needed)
# write.csv(final_df, "path_to_output_file.csv", row.names = FALSE)

# Display the final result
# View(result_df)