# -*- coding: utf-8 -*-
"""
Created on Tue May 13 17:10:38 2025

@author: Thomas Ball
"""

import requests
import xml.etree.ElementTree as ET
import os
import urllib
import pandas as pd
from fake_useragent import UserAgent
import bs4
import tqdm
import time
import random
import numpy as np

import _0_1_connection_functions as cf

def _get_url_df_brakes(sitemap_url, cache_data_path, overwrite: bool, skip_get_urls: bool = False):
    def fetch_product_pages_brakes(sitemap_url):
        def fetch_sitemap_urls(sitemap_url):
            response = requests.get(sitemap_url)
            response.raise_for_status()
            root = ET.fromstring(response.content)
            urls = [element.text for element in root.iter() if element.tag.endswith("loc")]
            return urls
        sitemap_urls = fetch_sitemap_urls(sitemap_url)
        product_urls = fetch_sitemap_urls([url for url in sitemap_urls if "Product" in url][0])
        return [url for url in product_urls if ".jpg" not in url]
    # check for existing link data
    if os.path.isfile(cache_data_path) and not overwrite:
        df = pd.read_csv(cache_data_path, index_col = 0)
    else:
        url_list = fetch_product_pages_brakes(sitemap_url)
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
                urllib.parse.urlsplit(sitemap_url).scheme,
                urllib.parse.urlsplit(sitemap_url).netloc,
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
        df.to_csv(cache_data_path)
    return df
      
def _get_url_df_tesco(sitemap_url, cache_data_path, overwrite: bool = False,
                      resume: bool = True):
    def fetch_sitemap_urls_tesco(sitemap_url, cache_data_path, 
                                 resume=True, batch_size=100, 
                                 delay=2.0, max_retries=3,
                                 use_proxy=False):
        headers, proxies = cf.setup_connection(use_proxy, rotate_user_agent=True)

        if resume and os.path.exists(cache_data_path):
            existing_df = pd.read_csv(cache_data_path, index_col=0)
            processed_urls = set(existing_df['URL'])
            print(f"Resuming with {len(existing_df)} existing URLs")
        else:
            existing_df = pd.DataFrame(columns=['URL'])
            processed_urls = set()
        retry_count = 0
        while retry_count < max_retries:
            try:
                # Rotate user agent for each attempt
                headers['User-Agent'] = UserAgent().random
                
                print(f"Attempt {retry_count + 1} of {max_retries}")
                print(f"Fetching sitemap with User-Agent: {headers['User-Agent']}")
                
                response = requests.get(
                    sitemap_url,
                    headers=headers,
                    timeout=30,
                    proxies=proxies,
                    stream=True
                )
                if response.status_code == 403:
                    print("Received 403 Forbidden - trying different approach")
                    raise ConnectionError("Blocked by server")
                
                response.raise_for_status()
                soup = bs4.BeautifulSoup(response.content, 'lxml-xml')
                url_tags = soup.find_all('loc')
                new_urls_df = pd.DataFrame(columns=['URL'])
                with tqdm.tqdm(total=len(url_tags), desc="Extracting URLs") as pbar:
                    for loc in url_tags:
                        try:
                            url = loc.text.strip()
                            if url not in processed_urls:
                                new_urls_df.loc[len(new_urls_df)] = [url]
                                processed_urls.add(url)
                                
                                # Batch processing with random delay
                                if len(new_urls_df) >= batch_size:
                                    combined_df = pd.concat([existing_df, new_urls_df])
                                    combined_df.to_csv(cache_data_path)
                                    existing_df = combined_df
                                    new_urls_df = pd.DataFrame(columns=['URL'])
                                    time.sleep(delay + random.uniform(0, 1.5))  # Randomized delay
                                    pbar.set_postfix({'Saved': len(existing_df)})    
                            pbar.update(1)   
                        except Exception as e:
                            print(f"\nError processing URL: {e}")
                            continue
                if not new_urls_df.empty:
                    combined_df = pd.concat([existing_df, new_urls_df])
                    combined_df.to_csv(cache_data_path)
                    existing_df = combined_df
                return existing_df
                
            except requests.exceptions.RequestException as e:
                retry_count += 1
                if retry_count < max_retries:
                    wait_time = min(2 ** retry_count, 30)  # Exponential backoff
                    print(f"\nRequest failed (attempt {retry_count}): {e}")
                    print(f"Waiting {wait_time} seconds before retry...")
                    time.sleep(wait_time)
                else:
                    print("\nMax retries reached. Failed to fetch sitemap.")
                    if 'new_urls_df' in locals() and not new_urls_df.empty:
                        combined_df = pd.concat([existing_df, new_urls_df])
                        combined_df.to_csv(cache_data_path)
                    raise
                    
    if not os.path.isfile(cache_data_path) or overwrite:
        fetch_sitemap_urls_tesco(sitemap_url, cache_data_path, resume = resume, 
                                batch_size = 100, delay = 1.1)
    else:
        try:
            df = pd.read_csv(cache_data_path, index_col = 0)
        except FileNotFoundError:
            print(f"File not found: {cache_data_path}. Fetching sitemap again.")
            df = fetch_sitemap_urls_tesco(sitemap_url, cache_data_path, resume = resume, 
                                          batch_size = 100, delay = 1.1)
    if "0" not in df.columns:
        df = df.reset_index(drop = True)
        with tqdm.tqdm(total=len(df)) as pbar:        
            for idx, row in df.iterrows():
                tcode = urllib.parse.urlsplit(row.URL).path.split("/")[-1]
                df.loc[idx, [0, 1, 2, 3, 4, 5, 6]] = [tcode] + [np.nan] * 6
                pbar.update(1)
        df.to_csv(cache_data_path)
    return df