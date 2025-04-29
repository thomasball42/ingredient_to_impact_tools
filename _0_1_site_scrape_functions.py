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

def clean_nutrition_str(text: str):
    """Returns a pandas.DataFrame
    """
    pattern = r'(\w+(?: \w+)*) \((\w+)\)\s+\n+\s+([\d.]+)'
    matches = re.findall(pattern, text)
    return pd.DataFrame(matches, columns = ["Element", "Unit", "Value"])
        
def clean_pack_size(text: str, prod_name: str, **kwargs):
    """This is a mess - trying to catch all the exceptions but it's not easy"""
    text = text.lower()
    mass_convert = {"g": 1,"kg" : 0.001,"ltr": 0.001,"L" : 0.001,
                    "l" : 0.001,"lb": 0.00220462,"cl" : 0.1,"ml" : 1,
                    "gne" : 1, "Kg" : 0.001, "lt" : 0.001,
                    "gall" : (1/4546.09)
                    }
    range_mean = lambda rstr: np.mean([float(v) for v in rstr.replace(" ", "").split("-")])
    def look_for_mass_units(instr: str):
        mass_units = [r'\b(\d+\.?\d*)\s*(mg|g|kg|lb|oz|l|ml)\b',]  # Match numbers followed by mass units
        matches = []
        for pattern in mass_units:
            matches.extend(re.findall(pattern, instr, re.IGNORECASE))
        return matches          
    def MASS_EXTRACTOR_1(pack_size_str): #lol
        match = re.search(r'(?P<quantity>\d+)\s*x\s*(?P<item_size>\d+(?:\.\d+)?(?:-\d+(?:\.\d+)?)?)(?P<unit>[a-zA-Z]+)', pack_size_str)
        item_mass_g, items_in_pack, portions_in_pack, flag = None, None, None, np.nan
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
            elif unit.lower() in ["ptn", "ptns", "tabs", "pk", "pots", "pcs", "pc", "pkt"]:
                flag = "ME1_PORTIONUNIT"
            else:
                flag = "FAIL"
        else:
            flag = "FAIL"      
        return item_mass_g, items_in_pack, portions_in_pack, flag
    def MASS_EXTRACTOR_2(pack_size_str, prod_name):
        """This one looks for mass in the product name.
        This only actually catches like 3 products... """
        item_mass_g, items_in_pack, portions_in_pack, flag = None, None, None, np.nan
        match = re.search(r'(?P<quantity>\d+)\s*x\s*(?P<item_size>\d+(?:\.\d+)?(?:-\d+(?:\.\d+)?)?)(?P<unit>[a-zA-Z]+)', pack_size_str)
        if match:
            items_in_pack = int(match.group("quantity"))
            item_size_str = match.group("item_size")
            unit = match.group("unit")
            matches = look_for_mass_units(prod_name)
            if matches:
                item_mass_g = float(matches[0][0]) * mass_convert["g"]/mass_convert[matches[0][1]]
                portions_in_pack = float(items_in_pack) * float(item_size_str)
            else:
                flag = "NO_MASS1"
                portions_in_pack = float(items_in_pack) * float(item_size_str)
        else:
            flag = "FAIL"
        return item_mass_g, items_in_pack, portions_in_pack, flag
    def MASS_EXTRACTOR_3(pack_size_str, prod_name):
        item_mass_g, items_in_pack, portions_in_pack, flag = None, None, None, np.nan
        flag = "TESTING"
        match = re.search(r'(?P<quantity>\d+)\s*x\s*(?P<item_size>\d+(?:\.\d+)?(?:-\d+(?:\.\d+)?)?)(?P<unit>[a-zA-Z]+)', pack_size_str)
        if not match:     
            nums = [n.strip(" ") for n in pack_size_str.strip("pack size: ").split("x")]    
            n = 1
            portion = False
            for num in nums:
                try: 
                    num = float(num)
                    n = n * num
                except ValueError:
                    if "ptn" in num:
                        portion = True
                        n = n * float(num.lower().split(" ")[0])
                    else:
                        try:
                            n = n * float(num.lower().strip(" qwertyuiopasdfghjklzxcvbnm"))
                        except ValueError:
                            flag = "FAIL"
                if flag != "FAIL":
                    if portion:
                        portions_in_pack = n
                        flag = "NO_MASS2"
                    else:
                        items_in_pack = n
                        flag = "NO_MASS2"
        else:
            flag = "UNIT_FAIL"
        return item_mass_g, items_in_pack, portions_in_pack, flag
    item_mass_g, items_in_pack, portions_in_pack, flag = MASS_EXTRACTOR_1(text)
    if flag == "ME1_PORTIONUNIT":
        item_mass_g, items_in_pack, portions_in_pack, flag = MASS_EXTRACTOR_2(text, prod_name)
    if flag == "FAIL":
        item_mass_g, items_in_pack, portions_in_pack, flag = MASS_EXTRACTOR_3(text, prod_name)
    return item_mass_g, items_in_pack, portions_in_pack, flag

# def extract_ingredient_details(df, column_name):
#     def parse_ingredients(ingredient_text):
#         if pd.isna(ingredient_text):
#             return []
#         pattern = r'([^(),%]+)(?:\s*(\d+\.?\d*)%?)?'
#         matches = re.findall(pattern, ingredient_text)
#         return [(match[0].strip(), float(match[1]) if match[1] else None) for match in matches]
    
#     df['parsed_ingredients'] = df[column_name].apply(parse_ingredients)
#     exploded_df = df.explode('parsed_ingredients')
#     exploded_df[['ingredient', 'percentage']] = pd.DataFrame(
#         exploded_df['parsed_ingredients'].tolist(), index=exploded_df.index
#     )
#     exploded_df.drop(columns=['parsed_ingredients'], inplace=True)
#     return exploded_df

    
def _get_url_df(sitemap_url, cache_data_path, overwrite: bool):
    """Currently this is specific to Brake.co.uk, might need to be updated for
    other sites."""
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


    
        
            