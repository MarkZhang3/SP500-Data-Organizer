import pandas as pd
import os
from dotenv import load_dotenv

load_dotenv()
home_dir = os.getenv('HOME_DIR')

wrds_ciq_key_file = '/WRDS_CIQ_Identifiers_Master_Table.csv'

# print(home_dir + wrds_ciq_key)
wrds_ciq_key = pd.read_csv(home_dir + wrds_ciq_key_file, nrows=100)
# print(keys_df['symboltypename'].unique())

firm_data = os.getenv('FIRM_DATA_DIR')
merger_history = '/merger_history.csv'
ciq_data = pd.read_csv(firm_data + merger_history, nrows=100)

wrds_file = '/all_wrds.csv'
wrds = pd.read_csv(home_dir + wrds_file, nrows=100)

# Step 1: Merging wrds_key into ciq_data
df = pd.merge(ciq_data, wrds_ciq_key, how='left', left_on='CIQ Company ID', right_on='companyid', indicator=True)

# Add columns 'symboltypename' and 'symbolvalue' from wrds_key or pad with NaN
df['symboltypename'] = df.apply(lambda row: row['symboltypename'] if row['_merge'] == 'both' else None, axis=1)
df['symbolvalue'] = df.apply(lambda row: row['symbolvalue'] if row['_merge'] == 'both' else None, axis=1)

# List of unmatched companyids from wrds_key
unmatched_companyids = wrds_ciq_key[~wrds_ciq_key['companyid'].isin(ciq_data['CIQ Company ID'])]['companyid'].tolist()

# Step 2: Merging wrds into the new dataset
matched_rows = []
for index, row in df.iterrows():
    if row['symboltypename'] == 'S&P GVKey':
        match = wrds[(wrds['gvkey'] == row['symbolvalue']) & (wrds['subsidiary_name'] == row['Company Name'])]
    elif row['symboltypename'] == 'SEC CIK number':
        match = wrds[(wrds['cik'] == row['symbolvalue']) & (wrds['subsidiary_name'] == row['Company Name'])]
    else:
        match = None

    if match is None or match.empty:
        matched_rows.append(row['Company Name'])

# List of rows in df where subsidiary_name in wrds does not match 'Company Name'
unmatched_names = matched_rows

# Output results
print("Unmatched companyids from wrds_key: ", unmatched_companyids)
print("Rows where subsidiary_name in wrds does not match 'Company Name': ", unmatched_names)





# company id = ciq id 
# sym type name => which identifier (gvkey, )
# symbol value => code corresponding to symbol type name 
# map symbol value to cik in wrds

# merge keys with corp tree based on ciq 
# then merge merged with wrds 





