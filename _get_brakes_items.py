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

url = "https://www.brake.co.uk/"
tdata_path = os.path.join("cache_dat", "site_data")
tfile_name = f"{urllib.parse.urlparse(url).netloc}_site_data.csv"
tfile_path = os.path.join(tdata_path, tfile_name)

fstrings = {
    "Ingredients" : lambda x: x.find_next("p").text,
    "Nutrition" : lambda x: x.find_next().find_next().text}

df = sst._get_url_df(url)

for idx, row in tqdm(df.iterrows(), total=len(df), desc="Processing pages"):
    
    if not pd.isnull(row.iloc[6]):
        
        for i, (fstring, fetch_func) in enumerate(fstrings.items()):
            
            if not fstring in row.index:
                
                page_url = row.URL
                fstring_text = sst._get_fstring_p(page_url, fstring, fetch_func)
                df.loc[idx, f"{fstring}_string"] = fstring_text

df.to_csv(tfile_path)
