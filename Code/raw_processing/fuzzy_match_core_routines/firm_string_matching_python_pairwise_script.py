# CODE FOR FUZZY STRING MATCHING TWO DATASETS OF COMPANY NAMES
#	IMPLEMENTS TF-IDF string matching procedure discussed by
#		https://bergvca.github.io/2017/10/14/super-fast-string-matching.html
#		https://github.com/Bergvca/string_grouper

# reads in two input files:
#	company_names_to_match_series_A.csv"
#	company_names_to_match_series_B.csv"
#
#	each of these files must have a column of column_name
# edit this one to use the same function as the firm_string_matching_python_script.py but update the package (line 30) to use the right
# function from the tfidf_matcher package and update the treshold update .65

import tfidf_matcher as sg 
import pandas as pd
import numpy as np
import os

current_directory = os.getcwd() #####################
projpath = os.path.dirname(os.path.dirname(current_directory)) #####################
filepath = os.path.join(projpath, "Output", "Temp/") #####################

#company_names_file_A = "/Users/mrempel/Dropbox/CultivatingMarketPower/data/output/company_names_to_match_series_A.csv"
#company_names_file_B = "/Users/mrempel/Dropbox/CultivatingMarketPower/data/output/company_names_to_match_series_B.csv"
company_names_file_A = "company_names_to_match_series_A.csv"
company_names_file_B = "company_names_to_match_series_B.csv"

##################### all the following code is newly added
# companies_A = pd.read_csv(os.path.join(filepath,company_names_file_A)) 
# companies_B = pd.read_csv(os.path.join(filepath, company_names_file_B), encoding='ISO-8859-1') 

def read_and_process_dataframe(filepath, filename):
    return (pd.read_csv(os.path.join(filepath, filename), encoding='ISO-8859-1')
            .query('company_name.notnull()')
            .drop_duplicates(subset=['company_name']))

companies_A = read_and_process_dataframe(filepath, company_names_file_A)
companies_A = companies_A.reset_index(drop=True)
companies_A = companies_A.reset_index(drop=False)

companies_B = read_and_process_dataframe(filepath, company_names_file_B)
companies_B = companies_B.reset_index(drop=True)
companies_B = companies_B.reset_index(drop=False)
# create a subset of companies_B
temp_companies_B = companies_B[['company_name', 'company_ID']]
temp_companies_B = temp_companies_B.reset_index(drop=False)

# use the tfidf_matcher package to perform the matching
matches = sg.matcher(companies_A['company_name'].astype(str), temp_companies_B['company_name'].astype(str), k_matches=5, ngram_length=3)
matches['Original Index'] = matches.index # here original index is the index for companies_A
# Note: Lookup 1 Index, Lookup 2 Index, Lookup 3 Index, Lookup 4 Index, Lookup 5 Index, all relates to indices of companies_B
# Merge to get 'company_ID'
matched_results = pd.merge(matches, temp_companies_B, left_on='Lookup 1 Index', right_on='index')
matched_results = matched_results.drop(['company_name', 'index'], axis=1) # drop "company_name" and "index" columns 
matched_results = matched_results.rename(columns={'company_ID': 'Lookup 1 ID'}) # renae "company_ID" to "Lookup 1 ID"

matched_results = pd.merge(matched_results, temp_companies_B, left_on='Lookup 2 Index', right_on='index')
matched_results = matched_results.drop(['company_name', 'index'], axis=1)
matched_results = matched_results.rename(columns={'company_ID': 'Lookup 2 ID'})

matched_results = pd.merge(matched_results, temp_companies_B, left_on='Lookup 3 Index', right_on='index')
matched_results = matched_results.drop(['company_name', 'index'], axis=1)
matched_results = matched_results.rename(columns={'company_ID': 'Lookup 3 ID'})

matched_results = pd.merge(matched_results, temp_companies_B, left_on='Lookup 4 Index', right_on='index')
matched_results = matched_results.drop(['company_name', 'index'], axis=1)
matched_results = matched_results.rename(columns={'company_ID': 'Lookup 4 ID'})

matched_results = pd.merge(matched_results, temp_companies_B, left_on='Lookup 5 Index', right_on='index')
matched_results = matched_results.drop(['company_name', 'index'], axis=1)
matched_results = matched_results.rename(columns={'company_ID': 'Lookup 5 ID'})

matched_results = matched_results[matched_results['Lookup 1 Confidence']>=.65] 

# now reshape the data from wide to long 
long_matches = pd.melt(matched_results, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 1', 'Lookup 2', 'Lookup 3', 'Lookup 4', 'Lookup 5'], var_name='lookup', value_name='match')
long_matches_confidence = pd.melt(matched_results, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 1 Confidence', 'Lookup 2 Confidence', 'Lookup 3 Confidence', 'Lookup 4 Confidence', 'Lookup 5 Confidence'], var_name='lookup', value_name='confidence')
long_matches_index = pd.melt(matched_results, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 1 Index', 'Lookup 2 Index', 'Lookup 3 Index', 'Lookup 4 Index', 'Lookup 5 Index'], var_name='lookup', value_name='index')
long_matches_id = pd.melt(matched_results, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 1 ID', 'Lookup 2 ID', 'Lookup 3 ID', 'Lookup 4 ID', 'Lookup 5 ID'], var_name='lookup', value_name='company_ID')

# clean lookup column to just be the number (k match)
long_matches.lookup = long_matches.lookup.str.extract('(\d+)').astype(int)
long_matches_confidence.lookup = long_matches_confidence.lookup.str.extract('(\d+)').astype(int)
long_matches_index.lookup = long_matches_index.lookup.str.extract('(\d+)').astype(int)
long_matches_id.lookup = long_matches_id.lookup.str.extract('(\d+)').astype(int)

# merge the three long dataframes together
long_matches = long_matches.merge(long_matches_confidence, on=['Original Name', 'Original Index', 'lookup'], how='left')
long_matches = long_matches.merge(long_matches_index, on=['Original Name','Original Index', 'lookup'], how='left')
long_matches = long_matches.merge(long_matches_id, on=['Original Name','Original Index', 'lookup'], how='left')

# now drop any matches with confidence below 0.65 
long_matches = long_matches[long_matches['confidence']>=.65]

# now rename the columns to match the output of the string_grouper package
long_matches = long_matches.rename(columns={'Original Name': 'left_company_name', 'Original Index': 'left_index', 'match': 'right_company_name', 'confidence': 'similarity', 'Index': 'right_index'})
# long_matches.columns.tolist() # check the column names


# output into current working directory + tmp folder
long_matches.to_csv(os.path.join(filepath, 'company_names_matched_pairwise.csv'), index=False)
# Save the DataFrame to the file in the specified directory