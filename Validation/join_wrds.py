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

if __name__ == '__main__':
    from dotenv import load_dotenv

    load_dotenv()
    directory_path = os.getenv('WRDS_CSV_DIR')
    output_path = os.getenv('HOME_DIR')
    combined_df = join_csv_files(directory_path)

    # save to csv file again
    combined_df.to_csv(output_path + "/all_wrds.csv", index=False)
