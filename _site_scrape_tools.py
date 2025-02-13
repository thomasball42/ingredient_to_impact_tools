# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 10:45:46 2024

This script recursivesly looks for pages from a given URL. Using this one a large
site (i.e. something like YouTube) will inevitably lead to a crash)

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

def _get_url_df(url, cache_data_path, overwrite: bool):
    # check for existing link data
    if os.path.isfile(cache_data_path) and not overwrite:
        df = pd.read_csv(cache_data_path, index_col = 0)
    else:
        # url_list = list_site_pages(url)
        url_list = fetch_product_pages(url)
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
        df.to_csv(cache_data_path)
    return df
    
def fetch_ingredients(soup,):
    ret = None
    elements = soup.find_all(string=lambda x: "Ingredients:" in x)
    for element in elements:
        if element:
            ret = element.find_parent().text
    return ret

def fetch_nutrition(soup,):
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

def fetch_packsize(soup, ):
    element = soup.find('div', class_='product-size__item product-size__item--size-pack')
    ret = None
    if element:
        ret = element.get_text(strip=True)
    return ret

def fetch_name(soup,):
    element = soup.find('h1', class_="product-details__name no-content")
    ret = None    
    if element:
        ret = element.get_text(strip=True)
    return ret

def _get_soup(page_url: str):
    reqs = requests.get(page_url)
    status_code = reqs.status_code
    if status_code != 404:
        soup = bs4.BeautifulSoup(reqs.text, 'html.parser')
    else: soup = None    
    
    return soup, status_code

def clean_nutrition_str(text: str):
    """Returns a pandas.DataFrame
    """
    pattern = r'(\w+(?: \w+)*) \((\w+)\)\s+\n+\s+([\d.]+)'
    matches = re.findall(pattern, text)
    return pd.DataFrame(matches, columns = ["Element", "Unit", "Value"])
    
def clean_pack_size(text: str, **kwargs):
    mass_convert = {"g": 1,"kg" : 0.001,"ltr": 0.001,"L" : 0.001,
                    "l" : 0.001,"lb": 0.00220462,"cl" : 0.1,"ml" : 1,
                    "gne" : 1,
                    }
    range_mean = lambda rstr: np.mean([float(v) for v in rstr.split("-")])
    def MASS_EXTRACTOR(pack_size_str):
        match = re.search(r'(?P<quantity>\d+)\s*x\s*(?P<item_size>\d+(?:\.\d+)?(?:-\d+(?:\.\d+)?)?)(?P<unit>[a-zA-Z]+)', pack_size_str)
        item_mass_g, items_in_pack, flag = None, None, np.nan
        if match:
            items_in_pack = int(match.group("quantity"))
            item_size_str = match.group("item_size")
            unit = match.group("unit")
            if "-" in item_size_str:
                item_mass = range_mean(item_size_str)
            else:
                item_mass = float(item_size_str)
            if unit in mass_convert.keys():
                item_mass_g = item_mass * mass_convert["g"]/mass_convert[unit] 
            else:
                flag = "no_unit"      
        return item_mass_g, items_in_pack, flag

    item_mass_g, items_in_pack, flag = MASS_EXTRACTOR(text)
    t = text.strip("Pack size: ") 
    parenthesis_content = None
    # if "(" in t and ")" in t:
    #     parenthesis_content = re.search(r"\((.*?)\)", t)
    #     parenthesis_content = parenthesis_content.group(1) if parenthesis_content else None
    #     if "/" in parenthesis_content:
    #         pc = parenthesis_content.split("/")
    #         per_u = range_mean(pc[0])
    #         per_g = per_u * (1/(mass_convert["g"]/mass_convert[pc[1]]))
    #     elif "makes" in parenthesis_content or "drained" in parenthesis_content:
    #         yield_match = re.search(r'\((?P<usable_size>\d+(?:\.\d+)?)(?P<usable_unit>[a-zA-Z]+) (?:drained wt|makes)\)', parenthesis_content)
    #         pass
    return item_mass_g, items_in_pack, flag
    
def list_site_pages(url):
    """ **recursion**without**proper**safety**features** :)** 
    This isn't used in the current pipeline, Tom was an idiot when he wrote 
    this, jury is still out on that.
    """
    def get_urls(url, url_list, max_depth=20, depth=0):
        if depth > max_depth:
            return
        reqs = requests.get(url)
        soup = bs4.BeautifulSoup(reqs.text, 'html.parser')
        tlist = soup.find_all("a")
        for link in tlist:
            linkurl = link.get("href")
            if linkurl is not None:
                fullurl = urllib.parse.urljoin(url, linkurl)
                if fullurl not in url_list:
                    print(fullurl)
                    url_list.append(fullurl)
                    # This checks that we haven't wandered onto a different site
                    if urllib.parse.urlparse(fullurl).netloc == urllib.parse.urlparse(url).netloc:
                        get_urls(fullurl, url_list, max_depth, depth + 1)
    url_list = []
    get_urls(url, url_list)
    return url_list

def extract_ingredient_details(df, column_name):
    def parse_ingredients(ingredient_text):
        if pd.isna(ingredient_text):
            return []
        pattern = r'([^(),%]+)(?:\s*(\d+\.?\d*)%?)?'
        matches = re.findall(pattern, ingredient_text)
        return [(match[0].strip(), float(match[1]) if match[1] else None) for match in matches]
    
    df['parsed_ingredients'] = df[column_name].apply(parse_ingredients)
    exploded_df = df.explode('parsed_ingredients')
    exploded_df[['ingredient', 'percentage']] = pd.DataFrame(
        exploded_df['parsed_ingredients'].tolist(), index=exploded_df.index
    )
    exploded_df.drop(columns=['parsed_ingredients'], inplace=True)
    return exploded_df


if __name__ == "__main__":
    
    soup, sc = _get_soup("https://www.brake.co.uk/bakery/sweet-bakery/doughnuts-eclairs/ring-doughnuts/country-choice-sugared-ring-doughnut/p/67004")
    element = soup.find('h1', class_="product-details__name no-content")
    
    ret = None    
    if element:
        ret = element.get_text(strip=True)
    
    
    pass
        
        
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    