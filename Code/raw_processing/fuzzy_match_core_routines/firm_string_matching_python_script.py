# CODE FOR FUZZY STRING MATCHING TWO DATASETS OF COMPANY NAMES
#	IMPLEMENTS TF-IDF based string matching procedure provided by
#       https://pypi.org/project/tfidf-matcher/ (April 6 2023 version)
#       which is based on and improves upon the string_grouper package
#           see discussion here:
#	        	https://bergvca.github.io/2017/10/14/super-fast-string-matching.html
#	        	https://github.com/Bergvca/string_grouper

# reads in one input file:
#	each of these files must have a column of column_name

import tfidf_matcher as sg 
import pandas as pd
import numpy as np
import os

# from string_grouper import match_strings, match_most_similar, group_similar_strings, StringGrouper

# set the temporary diretory to locate the company_names_to_match_series_all.csv file
current_directory = os.getcwd() #####################
projpath = os.path.dirname(os.path.dirname(current_directory)) #####################
filepath = os.path.join(projpath, "Output", "Temp/") #####################
#company_names_file = "/Users/mrempel/Dropbox/CultivatingMarketPower/processed_data/tmp/company_names_to_match_series_all.csv"
#company_names_file = "tmp/company_names_to_match_series_all.csv"

company_names_file = "company_names_to_match_series_all.csv"
#
# chunksize = 10000
# companies_iterator = pd.read_csv(company_names_file,iterator=True)
#                                  # chunksize = chunksize)
# for i in range(0, len(companies_iterator), chunksize):
#     companies_chunk = companies_iterator.get_chunk(chunksize)
#     matches = match_strings(companies_chunk['company_name'].astype(str),min_similarity=.65 )
#     if i == 0:
#         matches.to_csv('company_names_matched.csv', index=False)
#     elif:
#         matches.to_csv('company_names_matched.csv', mode='a', header=False, index=False)

# companies = pd.read_csv(company_names_file) 
companies = pd.read_csv(os.path.join(filepath,company_names_file)) #####################

companies= companies[companies['company_name'].notnull()]

companies= companies.drop_duplicates(subset=['company_name'])

matches = sg.matcher(companies['company_name'].astype(str),companies['company_name'].astype(str), k_matches=5, ngram_length=3)
                     # min_similarity=.65 )
# matches.columns.tolist() # check the column names

# add row index as column to matches dataframe "Original Index"

matches['Original Index'] = matches.index

# drop lookup 1 matches since these will be the trivial matches 

matches = matches.drop(["Lookup 1", "Lookup 1 Confidence",  "Lookup 1 Index"], axis=1)

# amongst lookup 2 matches, drop the matches where the lookup 2 confidence is less than 0.65

matches = matches[matches['Lookup 2 Confidence']>=.65] # play around with the cutoff #####################

# now reshape the data from wide to long 
#   mapping "Lookup n", Lookup n Confidence", "Lookup n Index" to k_match = n, match name, match confidence, match index 

long_matches = pd.melt(matches, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 2', 'Lookup 3', 'Lookup 4', 'Lookup 5'], var_name='lookup', value_name='match')

long_matches_confidence = pd.melt(matches, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 2 Confidence', 'Lookup 3 Confidence', 'Lookup 4 Confidence', 'Lookup 5 Confidence'], var_name='lookup', value_name='confidence')

long_matches_index = pd.melt(matches, id_vars=['Original Name', 'Original Index'], value_vars=['Lookup 2 Index', 'Lookup 3 Index', 'Lookup 4 Index', 'Lookup 5 Index'], var_name='lookup', value_name='index')

# clean lookup column to just be the number (k match)

long_matches.lookup = long_matches.lookup.str.extract('(\d+)').astype(int)

long_matches_confidence.lookup = long_matches_confidence.lookup.str.extract('(\d+)').astype(int)

long_matches_index.lookup = long_matches_index.lookup.str.extract('(\d+)').astype(int)

# merge the three long dataframes together

long_matches = long_matches.merge(long_matches_confidence, on=['Original Name', 'Original Index', 'lookup'], how='left')

long_matches = long_matches.merge(long_matches_index, on=['Original Name','Original Index', 'lookup'], how='left')

# now drop any matches with confidence below 0.65 

long_matches = long_matches[long_matches['confidence']>=.65]

# now rename the columns to match the output of the string_grouper package

long_matches = long_matches.rename(columns={'Original Name': 'left_company_name', 'Original Index': 'left_index', 'match': 'right_company_name', 'confidence': 'similarity', 'Index': 'right_index'})

# matches = sg.match_strings(companies['company_name'].astype(str),min_similarity=.65 )

# long_matches.to_csv('company_names_matched.csv', index=False)
# output into current working directory + tmp folder

long_matches.to_csv(os.path.join(filepath, 'company_names_matched.csv'), index=False) #####################
# Save the DataFrame to the file in the specified directory #####################
