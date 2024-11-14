# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 16:13:02 2024

@author: Thomas Ball
"""

import _site_scrape_tools as sst
import pandas as pd
import numpy as np
import urllib
import os
import bs4
import requests
from tqdm import tqdm

OVERWRITE = False
SITE_URL = "https://www.brake.co.uk/"
HEADER_STRINGS = {
            "Ingredients" : lambda x: x.find_next("p").text,
            "Nutrition" : lambda x: x.find_next().find_next().text,
            }

cache_data_dir = os.path.join("cache_dat", "site_data")
cache_data_name = f"{urllib.parse.urlparse(SITE_URL).netloc}_site_data.csv"
cache_data_path = os.path.join(cache_data_dir, cache_data_name)

# create dirs if they don't exist
if not os.path.isdir(cache_data_dir):
    os.makedirs(cache_data_dir)

df = sst._get_url_df(SITE_URL, cache_data_path)

for i, (fstring, fetch_func) in enumerate(HEADER_STRINGS.items()):
    
    if df[f"{fstring}_string"].isnull().all() or OVERWRITE:
        
        for idx, row in tqdm(df.iterrows(), total=len(df), desc=f"Looking for '{fstring}'"):
            
            if not pd.isnull(row.iloc[6]):
                
                if not fstring in row.index:
                    
                    page_url = row.URL
                    fstring_text = sst._get_fstring_p(page_url, fstring, fetch_func)
                    df.loc[idx, f"{fstring}_string"] = fstring_text

    df.to_csv(cache_data_path)
    
for idx, row in tqdm(df.iterrows(), total=len(df), desc=f"Processing nutrition info"):
    
    if isinstance(row.Nutrition_string, str):
        nutrition_text = row.Nutrition_string
        nutrition_text_cdf = sst.clean_nutrition_str(nutrition_text)
        
        df.loc[idx, nutrition_text_cdf.Element +" ("+nutrition_text_cdf.Unit+")"] = nutrition_text_cdf.Value.astype(float).values