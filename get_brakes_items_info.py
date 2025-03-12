# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 16:13:02 2024

@author: Thomas Ball
"""

import _site_scrape_tools as sst
import pandas as pd
import numpy as np
# import urllib
import os
# import bs4
# import requests
from tqdm import tqdm
import tldextract

OVERWRITE = True
SITE_URL = "https://www.brake.co.uk/sitemap.xml"

HEADER_STRINGS = {
            "Ingredients" : sst.fetch_ingredients,
            "Nutrition" : sst.fetch_nutrition,
            "Pack size" : sst.fetch_packsize,
            "Product name" : sst.fetch_name,
            }

REJECT_LIST = ["catering-supplies"]
cache_data_dir = os.path.join("dat", "site_data")
cache_data_name = f"{tldextract.extract(SITE_URL).domain}_site_data.csv"
cache_data_path = os.path.join(cache_data_dir, cache_data_name)

# create dirs if they don't exist
if not os.path.isdir(cache_data_dir):
    os.makedirs(cache_data_dir)

df = sst._get_url_df(SITE_URL, cache_data_path, overwrite = OVERWRITE)

for idx, row in tqdm(df.iterrows(), total=len(df), desc=f"Scraping via {SITE_URL}"):
    
    if not pd.isnull(row.iloc[6]):
        
        for i, (fstring, fetch_func) in enumerate(HEADER_STRINGS.items()):
            
            if f"{fstring}_string" not in df.columns or np.isnan(df.loc[idx, f"{fstring}_string"]) or OVERWRITE:
                
                page_url = row.URL
                for r in REJECT_LIST:
                    if r in page_url:
                        df.loc[idx, "RFLAG"] = "non_food"
                else:
                    soup, status_code = sst._get_soup(page_url)
                    df.loc[idx, "status_code"] = status_code 
                    if status_code != 404:
                        fstring_text = fetch_func(soup)
                        df.loc[idx, f"{fstring}_string"] = fstring_text
                
df.to_csv(cache_data_path)

for idx, row in tqdm(df.iterrows(), total=len(df), desc="Processing info"):

    if "Nutrition_string" in row and isinstance(row.Nutrition_string, str):
        nutrition_text = row.Nutrition_string
        nutrition_text_cdf = sst.clean_nutrition_str(nutrition_text)
        
        df.loc[idx, nutrition_text_cdf.Element +\
               " ("+nutrition_text_cdf.Unit+")"
               ] = nutrition_text_cdf.Value.astype(float).values
    
    if "Pack size_string" in row and isinstance(row["Pack size_string"], str):
        ps_text = row["Pack size_string"]
        df.loc[idx, ["item_mass_g"," items_in_pack", "CHECKFLAG"]] = sst.clean_pack_size(ps_text)
        
df.to_csv(cache_data_path)

