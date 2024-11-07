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

def list_site_pages(url):
    """ **recursion**without**proper**safety**features** :)** 
    !!BE CAREFUL USING THIS ON BIG WEBSITES!!
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

def _get_url_df(url):
    tdata_path = os.path.join("cache_dat", "site_data")
    tfile_name = f"{urllib.parse.urlparse(url).netloc}_site_data.csv"
    tfile_path = os.path.join(tdata_path, tfile_name)
    # check for existing link data
    if os.path.isfile(tfile_path):
        df = pd.read_csv(tfile_path, index_col = 0)
    else:
        # create dirs if they don't exist
        if not os.path.isdir(tdata_path):
            os.makedirs(tdata_path)
        url_list = list_site_pages(url)
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

def _get_fstring_p(page_url, fstring, fetch_func):
    """This is janky, if there are (eg) two ingredients lists on a page this
    will only return one of them. Works for Brakes though"""
    reqs = requests.get(page_url)
    soup = bs4.BeautifulSoup(reqs.text, 'html.parser')
    elements = soup.find_all(string=lambda string: string and fstring in string)
    ret = None
    if len(elements) >= 1:
        for element in elements:
            etext = None
            parent = element.find_parent()
            if parent:
                etext = fetch_func(parent)
                # if fstring in etext or fstring.upper() in etext or fstring.lower() in etext:
                ret = etext
    return ret


if __name__ == "__main__":
    
    text = '\n\n\n       \n          \n        \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100g\n            \n            \n        \n\n\n\nSuitable for Vegetarians\n\n\n\n\nEnergy (kcal)\n\n          \n          395\n          \n        \n\n\nEnergy (KJ)\n\n          \n          1662\n          \n        \n\n\nProtein (g)\n\n          \n          3.7\n          \n        \n\n\nCarbohydrates (g)\n\n          \n          65.4\n          \n        \n\n\nOf which sugars (g)\n\n          \n          36.7\n          \n        \n\n\nFat (g)\n\n          \n          13.5\n          \n        \n\n\nOf which saturates (g)\n\n          \n          8\n          \n        \n\n\nFibre (g)\n\n          \n          1.4\n          \n        \n\n\nSalt (g)\n\n          \n          0.2\n          \n        \n\n\nVegetarian Alternatives\n\n\n\n\n\n'
    # def _clean_nutrition_str()
    # def clean_nutrition_str(text):
        # Clean up the text by removing excessive newlines and spaces
    text_list = text.split()
    
    def tfloat(obj):
        try:
            float(obj)
            return True
        except ValueError:
            return False
        
    for i, t in enumerate(text_list):
        
        # print(i, t)
        
        if tfloat(t):
            
            val = float(t)
            unit = text_list[i-1]
            name_inf = ""
            ii = 0
            print(val, unit)
            while not tfloat(text_list[i-2-ii]):
                name_inf = text_list[i-2-ii] + " " + name_inf
                ii += 1
            print(name_inf.strip())
        
        
    # # Define a list of nutrients and values
    # nutrients = [
    #     'Energy (kcal)', 'Energy (KJ)', 'Protein (g)', 'Carbohydrates (g)', 'Of which sugars (g)',
    #     'Fat (g)', 'Of which saturates (g)', 'Fibre (g)', 'Salt (g)'
    # ]
    
    # # Initialize an empty dictionary
    # nutrient_dict = {}

    # # Iterate through each nutrient and extract its value from the text
    # for nutrient in nutrients:
    #     # Find the nutrient in the cleaned text
    #     start_idx = clean_text.find(nutrient)
        
    #     if start_idx != -1:
    #         # Move to the position after the nutrient name
    #         start_idx += len(nutrient)
            
    #         # Extract the value (numbers are after the nutrient)
    #         value = ''
    #         for char in clean_text[start_idx:]:
    #             if char.isdigit() or char == '.' or char == ',':
    #                 value += char
    #             elif value:  # Stop when we've collected the value
    #                 break
            
    #         # Add the nutrient and its value to the dictionary
    #         nutrient_dict[nutrient] = value
    
    # return nutrient_dict
