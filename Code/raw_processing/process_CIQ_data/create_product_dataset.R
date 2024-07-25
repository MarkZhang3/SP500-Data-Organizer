library(dplyr)
library(gdata)
library(readxl)

files_dir <- "C:/Users/zwend/Documents/UofT Material/work study local/Data/CIQ/corporate_trees_investments_and_products"

raw_files = list.files(path=files_dir)
raw_product_files = raw_files[grepl('Product', raw_files)]

## remove 1 obs from the list, Sonoco Products Company (has products in the title)
raw_product_files = raw_product_files[!grepl('Sonoco Products Company', raw_product_files)]

product_data_list = vector('list', length(raw_product_files))



get_start_row <- function(file = paste0(files_dir, '/', raw_product_files[1]), col1 = "Product Name", col2 = "Company") {
  data <- read.xls(file, header = FALSE, nrows = 30) 
  start_row <- which(data[, 1] == col1 & data[, 2] == col2)[1]
  return(start_row)
}

expected_columns <- c("Product Name", "Company", "Entity", "Source", "Product Description")

for (i in 1:length(raw_product_files)) {
  tryCatch({
    # tmp = gdata::read.xls(paste0(files_dir, '/', raw_product_files[i]), sheet=1, skip = 5, encoding = "UTF-8", check.names=TRUE,stringsAsFactors=FALSE)
    # ^ runs into some string handling errors
    
    tmp = readxl::read_excel(paste0(files_dir, '/', raw_product_files[i]), sheet = 1, skip = 12)
    
    # Check for missing columns and add them with NA values
    missing_columns <- setdiff(expected_columns, names(tmp))
    for (col in missing_columns) {
      tmp[[col]] <- NA
    }
    
    # Ensure columns are in the expected order
    tmp <- tmp[expected_columns]
    
    product_data_list[[i]] = tmp
  }, error = function(e) {
    cat("Error in file:", raw_product_files[i],i, "\n")
    cat("Error message:", e$message, "\n")
    break
  })
}

## notable changes
# in Adeia Inc, changed mu to u
# in ams OSRAM, changed mu to u
# in Analog Devices, changed mu to u

product_data = do.call(rbind, product_data_list)
View(product_data)

save(product_data, file="C:/Users/zwend/Documents/UofT Material/work study local/processed_data/firm_data/products_dataset.RData")

