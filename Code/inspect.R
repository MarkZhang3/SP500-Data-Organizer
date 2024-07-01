# Define the path to the RData files
data_dir <- "C:/Users/zwend/Documents/UofT Material/work study local/processed_data/firm_data"

# Function to load an .RData file and list its contents
load_and_list <- function(file_path) {
  load(file_path)
  print(paste("Objects in", file_path, ":"))
  print(ls())
}

# Load the capiq_merger_dataset_June2024.RData file
load_and_list(file.path(data_dir, "capiq_merger_dataset_June2024.RData"))

print("Structure of capiq_merger_dataset:")
str(capiq_merger_dataset_2020)
summary(capiq_merger_dataset_2020)

# Inspect the loaded objects (replace 'actual_object_name' with the correct name after checking the output)
# str(actual_object_name)
# summary(actual_object_name)

# Load the complete_corporate_tree_dataset_2020.RData file
load_and_list(file.path(data_dir, "complete_corporate_tree_dataset_2020.RData"))


print("Structure of complete_corporate_tree_dataset:")
str(corporate_tree_dataset_2020)
summary(corporate_tree_dataset_2020)


