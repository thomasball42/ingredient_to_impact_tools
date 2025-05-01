# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 10:45:46 2024

Functions to enable the scraping of product information from grocery retail
and catering supplier websites. Currently works for brake.co.uk..

Also makes no attempt to bypass any scraping preventation.

@author: Thomas Ball
"""

import requests
import bs4
import urllib.parse
import os
import pandas as pd
import re 
import xml.etree.ElementTree as ET
import numpy as np
import tqdm
import time
import random
from fake_useragent import UserAgent

def fetch_ingredients_brakes(soup,):
    ret = None
    elements = soup.find_all(string=lambda x: "Ingredients:" in x)
    for element in elements:
        if element:
            ret = element.find_parent().text
    return ret

def fetch_nutrition_brakes(soup,):
    fetch_func = lambda x: x.find_next().find_next().text
    elements = soup.find_all(string=lambda string: string and "Nutrition" in string)
    ret = None
    if len(elements) >= 1:
        for element in elements:
            etext = None
            parent = element.find_parent()
            if parent:
                etext = fetch_func(parent)
                ret = etext
    return ret

def fetch_packsize_brakes(soup, ):
    element = soup.find('div', class_='product-size__item product-size__item--size-pack')
    ret = None
    if element:
        ret = element.get_text(strip=True)
    return ret

def fetch_name_brakes(soup,):
    element = soup.find('h1', class_="product-details__name no-content")
    ret = None    
    if element:
        ret = element.get_text(strip=True)
    return ret

def fetch_sitemap_urls(sitemap_url):
    response = requests.get(sitemap_url)
    response.raise_for_status()
    root = ET.fromstring(response.content)
    urls = [element.text for element in root.iter() if element.tag.endswith("loc")]
    return urls
    
def fetch_product_pages(sitemap_url):
    sitemap_urls = fetch_sitemap_urls(sitemap_url)
    product_urls = fetch_sitemap_urls([url for url in sitemap_urls if "Product" in url][0])
    return [url for url in product_urls if ".jpg" not in url]

def _get_url_df_brakes(sitemap_url, cache_data_path, overwrite: bool):
    """Currently this is specific to Brake.co.uk, might need to be updated for
    other sites."""
    
    # check for existing link data
    if os.path.isfile(cache_data_path) and not overwrite:
        df = pd.read_csv(cache_data_path, index_col = 0)
    else:
        url_list = fetch_product_pages(sitemap_url)
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
    
def _get_soup(page_url: str):
    reqs = requests.get(page_url)
    status_code = reqs.status_code
    if status_code != 404:
        soup = bs4.BeautifulSoup(reqs.text, 'html.parser')
    else: soup = None    
    return soup, status_code

def fetch_sitemap_urls_tesco(sitemap_url, cache_data_path, 
                           resume=True, batch_size=100, 
                           delay=2.0, max_retries=3,
                           use_proxy=False):
    # user agent generator
    ua = UserAgent()
    # headers
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Accept-Language': 'en-US,en;q=0.5',
        'Accept-Encoding': 'gzip, deflate, br',
        'Connection': 'keep-alive',
        'Referer': 'https://www.tesco.com/',
        'DNT': '1',
    }
    # Proxy configuration (if enabled)
    proxies = {
        'http': 'http://your_proxy:port',
        'https': 'http://your_proxy:port'
    } if use_proxy else None
    
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
            headers['User-Agent'] = ua.random
            
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
    
def _get_url_df_tesco(sitemap_url, cache_data_path, overwrite: bool):
    
    # check for existing link data
    # if os.path.isfile(cache_data_path) and not overwrite:
    #     df = pd.read_csv(cache_data_path, index_col = 0)
    # else:
    fetch_sitemap_urls_tesco(sitemap_url, cache_data_path, resume = True, 
                             batch_size = 100, delay = 1.1)
    df = pd.read_csv(cache_data_path, index_col = 0)
    if "0" not in df.columns:
        print(df.columns)
        df = df.reset_index(drop = True)
        with tqdm.tqdm(total=len(df)) as pbar:        
            for idx, row in df.iterrows():
                tcode = urllib.parse.urlsplit(df.URL[1]).path.split("/")[-1]
                df[[0, 1, 2, 3, 4, 5, 6]] = [tcode, None, None, None, None, None, None]
                pbar.update(1)
        df.to_csv(cache_data_path)
    return df


    
    