# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 07:52:46 2024

@author: Thomas Ball
"""

import pandas as pd
import os
import urllib
from tqdm import tqdm

OVERWRITE = True
SITE_URL = "https://www.brake.co.uk/"
CLARK_PATH = os.path.join("dat", "clark_data")

if not os.path.isdir(CLARK_PATH):
    import requests
    import zipfile
    import io
    clark_dat_url = "https://ora.ox.ac.uk/objects/uuid:4ad0b594-3e81-4e61-aefc-5d869c799a87/files/sst74cr71b"    
    response = requests.get(clark_dat_url)
    response.raise_for_status()
    with zipfile.ZipFile(io.BytesIO(response.content)) as zip_file:
        zip_file.extractall(CLARK_PATH)
    

cache_data_dir = os.path.join("dat", "site_data")
cache_data_name = f"{urllib.parse.urlparse(SITE_URL).netloc}_site_data.csv"
cache_data_path = os.path.join(cache_data_dir, cache_data_name)

cats_df = pd.read_csv(os.path.join(CLARK_PATH, "Clark_et_al_2022_PNAS_SM", 
                                   "foodDB_dat", "categories anonymised.csv"), 
                      encoding = "latin-1")
prods_df = pd.read_csv(os.path.join(CLARK_PATH, "Clark_et_al_2022_PNAS_SM",
                        "foodDB_dat", "products anonymised.csv"), 
                       encoding = "latin-1", low_memory=False)

df = pd.read_csv(cache_data_path, index_col = 0)

cdf = pd.DataFrame(columns = cats_df.columns.to_list() + ["brakes_id"])
pdf = pd.DataFrame(columns = prods_df.columns)

pdf_cols = {'Energy (kcal)' : "energy_per_100",
            #'Energy (KJ)' : None,
            'Protein (g)' : "protein_per_100", 
            'Carbohydrates (g)' : "carbohydrate_per_100",
            'Of which sugars (g)' : "sugar_per_100",
            'Fat (g)' : "fat_per_100",
            'Of which saturates (g)' : "saturates_per_100", 
            'Fibre (g)' : "fibre_per_100",
            
            "Ingredients_string" : "ingredients_text"
            }


for r, (idx, row) in tqdm(enumerate(df.iterrows()), total=len(df), desc="Formatting.."):
    
    if not pd.isnull(row.Nutrition_string):
        
        row_id = len(pdf)
        
        shop_location = {"product_id":      row.iloc[1] + cats_df.product_id.max(), # to make sure nothing overlaps with existing food-db data
                        "name" :            row["Product name_string"],
                        "main_category" :   row.iloc[7],
                        "department" :      row.iloc[6],
                        "aisle" :           row.iloc[5],
                        "shelf" :           row.iloc[4],
                        "brakes_id":        row.iloc[1]}
        
        cdf.loc[row_id] = pd.Series(shop_location)
        
        pdf.loc[row_id, "product_id"] = row.iloc[1] + cats_df.product_id.max()
        pdf.loc[row_id, "product_name"] = row["Product name_string"]
        pdf.loc[row_id, "url"] = row.URL
        
        
        for item in row.index:
            if item in pdf_cols.keys():
                pdf.loc[row_id, pdf_cols[item]] = row[item]
        
        
        