# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 07:52:46 2024

@author: Thomas Ball
"""

import pandas as pd
import os
import urllib
from tqdm import tqdm
import tldextract

OVERWRITE = True
SITE_URL = "https://www.brake.co.uk/"
CLARK_PATH = os.path.join("dat", "clark_data")
sitename = tldextract.extract(SITE_URL).domain

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
cache_data_name = f"{sitename}_site_data.csv"
cache_data_path = os.path.join(cache_data_dir, cache_data_name)

cats_df = pd.read_csv(os.path.join(CLARK_PATH, "Clark_et_al_2022_PNAS_SM", 
                                   "foodDB_dat", "categories anonymised.csv"), 
                      encoding = "latin-1")
prods_df = pd.read_csv(os.path.join(CLARK_PATH, "Clark_et_al_2022_PNAS_SM",
                        "foodDB_dat", "products anonymised.csv"), 
                       encoding = "latin-1", low_memory=False)

df = pd.read_csv(cache_data_path, index_col = 0)

cdf = pd.DataFrame(columns = cats_df.columns.to_list() + ["shop_uid"])
pdf = pd.DataFrame(columns = prods_df.columns)

pdf_cols = {'Energy (kcal)' : "energy_per_100",
            #'Energy (KJ)' : None,
            'Protein (g)' : "protein_per_100", 
            'Carbohydrates (g)' : "carbohydrate_per_100",
            'Of which sugars (g)' : "sugar_per_100",
            'Fat (g)' : "fat_per_100",
            'Of which saturates (g)' : "saturates_per_100", 
            'Fibre (g)' : "fibre_per_100",
            'Salt (g)' : "salt_per_100",
            "Ingredients_string" : "ingredients_text",
            'Pack size_string' : "pack_size_string",
            " items_in_pack" : "items_in_pack",
            "item_mass_g" : "item_mass_g",
            }

for r, (idx, row) in tqdm(enumerate(df.iterrows()), total=len(df), desc="Formatting.."):
    if not pd.isnull(row.Nutrition_string):
        row_id = len(pdf)
        
        shop_location = {"product_id":      str(row.iloc[1]) + tldextract.extract(SITE_URL).domain, # to make sure nothing overlaps with existing food-db data
                        "name" :            row["Product name_string"],
                        "main_category" :   row.iloc[7],
                        "department" :      row.iloc[6],
                        "aisle" :           row.iloc[5],
                        "shelf" :           row.iloc[4],
                        "shop_uid":        row.iloc[1]}
        
        cdf.loc[row_id] = pd.Series(shop_location)
        
        pdf.loc[row_id, "product_id"] = str(row.iloc[1]) + tldextract.extract(SITE_URL).domain # this may break things down the line..
        pdf.loc[row_id, "product_name"] = row["Product name_string"]
        pdf.loc[row_id, "url"] = row.URL
        pdf.loc[row_id, "ingredients_text"] = row.Ingredients_string
        pdf.loc[row_id, "Retailer"] = tldextract.extract(SITE_URL).domain
        pdf.loc[row_id, "shop_uid"] = row.iloc[1]
        
        for item in row.index:
            if item in pdf_cols.keys():
                pdf.loc[row_id, pdf_cols[item]] = row[item]
        

products_out_path = os.path.join(cache_data_dir, f"products_{sitename}.csv")
pdf.to_csv(products_out_path, index=False)
categories_out_path = os.path.join(cache_data_dir, f"categories_{sitename}.csv")
cdf.to_csv(categories_out_path, index=False)

# prods_df_concat = pd.concat([pdf, prods_df])
# cats_df_concat = pd.concat([cdf, cats_df])

# prods_df_concat.to_csv(os.path.join("clark_mod", "products_concat.csv"))
# cats_df_concat.to_csv(os.path.join("clark_mod", "categories_concat.csv"))
