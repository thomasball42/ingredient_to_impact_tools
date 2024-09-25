# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 16:13:02 2024

@author: Thomas Ball
"""

import _site_scrape
import pandas as pd
import urllib
import os

url = "https://www.brake.co.uk/"

def _get_url_df(url):
    tdata_path = os.path.join("data", "cached_data")
    
    tfile_name = f"{urllib.parse.urlparse(url).netloc}_linkdat.csv"
    tfile_path = os.path.join(tdata_path, tfile_name)
    # check for existing link data
    if os.path.isfile(tfile_path):
        df = pd.read_csv(tfile_path, index_col = 0)
    else:
        # create dirs if they don't exist
        if not os.path.isdir(tdata_path):
            os.makedirs(tdata_path)
        url_list = _site_scrape.list_site_pages(url)
        url_listx = [url for url in url_list if urllib.parse.urlparse(url).netloc in url]
        paths = [urllib.parse.urlsplit(url).path.strip('/') for url in url_list]
        final_paths = set(paths)
        for path in paths:
            segments = path.split('/')
            for i in range(1, len(segments)):
                parent_path = '/'.join(segments[:i])
                if parent_path in final_paths:
                    final_paths.remove(parent_path)
        df_rows = []
        for path in final_paths:
            row = {"URL": urllib.parse.urlunsplit((
                urllib.parse.urlsplit(url).scheme,
                urllib.parse.urlsplit(url).netloc,
                path,
                '',
                ''
            ))}
            path_segments = path.split("/")
            path_segments.reverse()
            for i, item in enumerate(path_segments):
                row[i] = item
            df_rows.append(row)
        df = pd.DataFrame(df_rows)
        df.to_csv(tfile_path)
    return df