import os
import pandas as pd 
import numpy as np
import re
import sys


def is_valid_filename(filename):
    # Define the regular expression pattern
    pattern = r"^\d{2}-\d{2}-\d{4} to \d{2}-\d{2}-\d{4}$"
    
    # Match the pattern with the filename
    match = re.match(pattern, filename)
    
    return bool(match)


# from util.py
def fetch_data(source_dir):
    """
    """
    tree, investments, products = None, None, None
    for filename in os.listdir(source_dir):
        file_path = os.path.join(source_dir, filename)
        if not (filename.endswith(".xls")):
            continue
        if ('Corporate Structure Tree' in filename):
            tree = pd.read_excel(file_path, engine='xlrd')
        if ('Direct Investment' in filename):
            investments = pd.read_excel(file_path, engine='xlrd', skiprows=1)
        if ('Products' in filename):
            products = pd.read_excel(file_path, engine='xlrd')
    return tree, investments, products


def get_ma_data(source_dir):
    for filename in os.listdir(source_dir):
        file_path = os.path.join(source_dir, filename)
        if '6-6' in filename and '.xlsx' in filename:
            return pd.read_excel(file_path, engine='openpyxl', skiprows=0)


# from util.py
def find_ciq_parent_id(row, corporate_tree):
    parent_name = row['Parent Company Name']
    if parent_name in corporate_tree['Company Name'].values:
        return corporate_tree.loc[corporate_tree['Company Name'] == parent_name, 'CIQ Company ID'].values[0]
    else:
        return None
    
# functions for building dataframe for 1 company 

def initial_build(tree, investments):
    """Builds an inital dataset given the corp tree and investments
    Assumes tree and investments are both pandas dataframes 
    The columns will be:
        Company name, CIQ company id, Parent company name, CIQ parent company id
    """
    # For company name and CIQ company id
    merged_data = tree.merge(investments, on=['Company Name'], how='left')
    merged_data['CIQ Company ID'] = merged_data['CIQ Company ID']


    # For parent company name, CIQ parent company id 
    # Extract the Parent Company Name directly from the corporate tree
    merged_data['Parent Company Name'] = merged_data['Parent Company']


    # Apply the function to each row in the merged data
    merged_data['CIQ Parent Company ID'] = merged_data.apply(find_ciq_parent_id, axis=1, corporate_tree=tree)


    # add the start and end dates
    # I can also change this so that '-' is left as '-'
    # right now, errors='coerce' will replace '-' with NaN 
    merged_data['Start Year'] = pd.to_datetime(investments['Last Investment Date'], errors='coerce').dt.year
    merged_data['End Year'] = pd.to_datetime(investments['Period End Date'], errors='coerce', format='%b-%d-%Y').dt.year 

    merged_data['Start Date'] = investments['Last Investment Date']
    merged_data['End Date'] = investments['Period End Date']

    merged_data['Exit Type'] = tree['Relationship Type']

    return merged_data


# add ma data for 1 company 
# will need to call this M times, M = number of companies 
def add_ma(merged_df, ma_data):
    """Given the existing dataframe for a company, add the columns that come from the M & A Dataset
    These columns are: 
        Exit Type, start M & A Transaction, End M & A transaction 

    This approach is slower, need to traverse entire M & A datasets to search for company + parent 
    roughly O(NM)
    """


# add ma data for all companies
# should only need to call once, but need all companies dataframes to be finished 
def add_ma(ma_data, all_merged_df: dict):
    """Given ALL existing data frames for ALL companies as a dictionary, 
    go through the ma_data, ma_data is already sorted most recent transaction date -> least recent

    for every transaction date:
        find the target (company) and buyer (parent) 
        get all_merged_df[buyer]:
            if "start M & A transaction ID" is empty, add it
            add to "end M & A transaction ID" (regardless of if statement)
    
    roughly O(N), N = number of entries in M & A dataset 
    """

    ## temporarily pad the columns from the M & A dataset
    for df in all_merged_df.values():
        # print(df)
        df['Start M&A Transaction ID'] = '-'
        df['End M&A Transaction ID'] = '-'
        df['Status'] = '-'

    ## Currently still need to modify if there is more than 1 buyer 

    for index, row in ma_data.iterrows():
        target = row['Target/Issuer']
        buyer = row['Buyers/Investors']
        transaction_id = row['CIQ Transaction ID']
        status = row['Transaction Status']
        
        # to check if I got the right transaction, I'll check if the target is an investment of the buyer
        if buyer in all_merged_df and target in all_merged_df[buyer]['Company Name']:
            buyer_df = all_merged_df[buyer]
            target_row_index = buyer_df[buyer_df['Company Name'] == target].index
            
            if not target_row_index.empty:
                target_row_index = target_row_index[0]
                
                if pd.isna(buyer_df.at[target_row_index, 'Start M&A Transaction ID']):
                    buyer_df.at[target_row_index, 'Start M&A Transaction ID'] = transaction_id
                buyer_df.at[target_row_index, 'End M&A Transaction ID'] = transaction_id
                buyer_df.at[target_row_index, 'Status'] = status

            # Update the dictionary with the modified DataFrame
            all_merged_df[buyer] = buyer_df
    
    return all_merged_df 


def select_rows(merged_df):
    dataset_1_columns = [
        'Company Name', 'CIQ Company ID', 'Parent Company Name', 'CIQ Parent Company ID',
        'Start Year', 'End Year', 'Start Date', 'End Date', 'Status', 'Exit Type',
        'Start M&A Transaction ID', 'End M&A Transaction ID'
    ]
    return merged_df [dataset_1_columns]


# put this in util.py
def export(all_merged_df):
    combined_df = pd.concat(all_merged_df.values(), ignore_index=True)

    df = select_rows(combined_df)
    # Export to Excel
    # df.to_excel('combined_dataframe.xlsx', index=False)

    # Export to CSV
    df.to_csv('combined_dataframe.csv', index=False)

    # Convert dataframe to markdown format
    # markdown = df.to_markdown(index=False)

    # # Write to .Rmd file
    # with open('Dataset_1.Rmd', 'w') as file:
    #     file.write('---\ntitle: "Dataset 1"\noutput: html_document\n---\n\n')
    #     file.write(markdown)



if __name__ == '__main__':
    source_dir = 'C:\\Users\\zwend\\Documents\\UofT Material\\work study local\\tmp'
    tree, investments, products = fetch_data(source_dir)
    # print(investments['Last Investment Date'])
    # sys.exit()
    
    df = initial_build(tree, investments)
    # print(df.head())

    df = {
        'A O Smith': df
    }

    ma_data = get_ma_data(source_dir)
    # print(ma_data.head())

    df_dict = add_ma(ma_data, df)
    
    df = export(df_dict)

    