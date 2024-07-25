import pandas as pd
import os

def join_csv_files(directory):
    # List to store individual DataFrames
    dataframes = []

    # Iterate over files in the given directory
    for filename in os.listdir(directory):
        if filename.endswith(".csv"):
            filepath = os.path.join(directory, filename)
            df = pd.read_csv(filepath)
            dataframes.append(df)

    # Concatenate all DataFrames
    combined_df = pd.concat(dataframes, ignore_index=True)
    
    return combined_df

def clean_wrds(wrds):
    df = wrds 

    # Rename 'coname' to 'parent_name'
    df = df.rename(columns={'coname': 'parent_name'})

    # Convert date columns to datetime
    df['fdate'] = pd.to_datetime(df['fdate'])
    df['rdate'] = pd.to_datetime(df['rdate'])
    df['secpdate'] = pd.to_datetime(df['secpdate'])

    # Find the minimum and maximum years for each subsidiary_name, parent_name pair
    grouped = df.groupby(['subsidiary_name', 'parent_name'])

    start_year = grouped[['fdate', 'rdate', 'secpdate']].min().min(axis=1).dt.year.rename('start_year')
    end_year = grouped[['fdate', 'rdate', 'secpdate']].max().max(axis=1).dt.year.rename('end_year')

    # Function to filter rows for start_year and end_year
    def filter_rows(group, start_year, end_year):
        start_year_value = start_year.loc[(group.name[0], group.name[1])]
        end_year_value = end_year.loc[(group.name[0], group.name[1])]
        min_date = group[['fdate', 'rdate', 'secpdate']].min(axis=1)
        max_date = group[['fdate', 'rdate', 'secpdate']].max(axis=1)
        return group[(min_date.dt.year == start_year_value) | (max_date.dt.year == end_year_value)]

    # Apply the filter function
    result = grouped.apply(lambda x: filter_rows(x, start_year, end_year), include_groups=False)

    # Reset index and remove unnecessary columns
    result = result.reset_index(drop=True)
    result = result.join(start_year, on=['subsidiary_name', 'parent_name'])
    result = result.join(end_year, on=['subsidiary_name', 'parent_name'])

    # Reorder columns
    columns_order = ['subsidiary_name', 'parent_name', 'start_year', 'end_year'] + [col for col in df.columns if col not in ['subsidiary_name', 'parent_name', 'fdate', 'rdate', 'secpdate']]
    result = result[columns_order]

    wrds.to_csv(output_path + '/WRDS_aggregated.csv', index=False)



if __name__ == '__main__':
    from dotenv import load_dotenv

    load_dotenv()
    directory_path = os.getenv('WRDS_CSV_DIR')
    output_path = os.getenv('HOME_DIR')

    # combined_df = join_csv_files(directory_path)

    # # save to csv file again
    # combined_df.to_csv(output_path + "/all_wrds.csv", index=False)

    wrds_df = pd.read_csv(output_path + '/all_wrds.csv', nrows=5000)
    clean_wrds(wrds_df)
