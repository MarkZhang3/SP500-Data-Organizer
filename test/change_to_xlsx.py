import os
import pandas as pd

def resave_excel_files(source_dir, target_dir):
    for filename in os.listdir(source_dir):
        if (filename.endswith(".xls")):
            file_path = os.path.join(source_dir, filename)
            try:
                # Read the Excel file
                df = pd.read_excel(file_path, engine='openpyxl' if filename.endswith(".xlsx") else 'xlrd')
                
                # Save the Excel file in the same format
                new_file_path = os.path.join(target_dir, filename.replace('.xls', '.xlsx'))
                df.to_excel(new_file_path, index=False, engine='openpyxl')
                print(f"Resaved: {filename} as {new_file_path}")
            except Exception as e:
                print(f"Failed to resave: {filename}, Error: {e}")

source_dir = 'C:\\Users\\zwend\\Documents\\UofT Material\\work study local\\tmp'
resave_excel_files(source_dir, source_dir)