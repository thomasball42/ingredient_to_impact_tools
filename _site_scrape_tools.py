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

def _get_url_df(url, cache_data_path):
    # check for existing link data
    if os.path.isfile(cache_data_path):
        df = pd.read_csv(cache_data_path, index_col = 0)
    else:
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
        df.to_csv(cache_data_path)
    return df

def _get_fstring_p(page_url: str, fstring: str, fetch_func):
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

def clean_nutrition_str(text: str):
    """Returns a pandas.DataFrame
    """

# texts = ['\n\n\n       \n          \n        \n          \n        \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100g\n            \n            \n        \n\n\n\nSuitable for Vegetarians\n\n\n\n\nSuitable for Vegans\n\n\n\n\nEnergy (kcal)\n\n          \n          417\n          \n        \n\n\nEnergy (KJ)\n\n          \n          1733\n          \n        \n\n\nProtein (g)\n\n          \n          3.4\n          \n        \n\n\nCarbohydrates (g)\n\n          \n          35\n          \n        \n\n\nOf which sugars (g)\n\n          \n          22\n          \n        \n\n\nFat (g)\n\n          \n          29\n          \n        \n\n\nOf which saturates (g)\n\n          \n          14\n          \n        \n\n\nFibre (g)\n\n          \n          2.6\n          \n        \n\n\nSalt (g)\n\n          \n          0.31\n          \n        \n\n\n\n',
#  '\n\n\n       \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100g\n            \n            \n        \n\n\n\nEnergy (kcal)\n\n          \n          194\n          \n        \n\n\nEnergy (KJ)\n\n          \n          812\n          \n        \n\n\nProtein (g)\n\n          \n          13\n          \n        \n\n\nCarbohydrates (g)\n\n          \n          20\n          \n        \n\n\nOf which sugars (g)\n\n          \n          1.6\n          \n        \n\n\nFat (g)\n\n          \n          6.9\n          \n        \n\n\nOf which saturates (g)\n\n          \n          0.7\n          \n        \n\n\nFibre (g)\n\n          \n          0.5\n          \n        \n\n\nSalt (g)\n\n          \n          1.4\n          \n        \n\n\nHalal\n\n\n\n\n\n',
#  '\n\n\n       \n          \n        \n          \n        \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100ml\n            \n            \n        \n\n\n\nSuitable for Vegetarians\n\n\n\n\nSuitable for Vegans\n\n\n\n\n\n',
#  '\n\n\n       \n          \n        \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100g\n            \n            \n        \n\n\n\nSuitable for Vegetarians\n\n\n\n\nEnergy (kcal)\n\n          \n          181\n          \n        \n\n\nEnergy (KJ)\n\n          \n          758\n          \n        \n\n\nProtein (g)\n\n          \n          2.9\n          \n        \n\n\nCarbohydrates (g)\n\n          \n          19\n          \n        \n\n\nOf which sugars (g)\n\n          \n          4.1\n          \n        \n\n\nFat (g)\n\n          \n          10\n          \n        \n\n\nOf which saturates (g)\n\n          \n          4\n          \n        \n\n\nFibre (g)\n\n          \n          1.7\n          \n        \n\n\nSalt (g)\n\n          \n          0.64\n          \n        \n\n\nVegetarian Alternatives\n\n\n\n\n\n',
#  '\n\n\n       \n          \n        \n          \n                \n                     \n                \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n          \n        \n        \n            \n                per 100g\n            \n            \n        \n\n\n\nSuitable for Vegetarians\n\n\n\n\nEnergy (kcal)\n\n          \n          267\n          \n        \n\n\nEnergy (KJ)\n\n          \n          1118\n          \n        \n\n\nProtein (g)\n\n          \n          5\n          \n        \n\n\nCarbohydrates (g)\n\n          \n          31\n          \n        \n\n\nOf which sugars (g)\n\n          \n          20\n          \n        \n\n\nFat (g)\n\n          \n          13.7\n          \n        \n\n\nOf which saturates (g)\n\n          \n          6.9\n          \n        \n\n\nFibre (g)\n\n          \n          0.3\n          \n        \n\n\nSalt (g)\n\n          \n          0.28\n          \n        \n\n\nGluten Free Alternatives\n\n\n\n\n\n']



# for text in texts:
    sr = pd.Series()
    
    pattern = r'(\w+(?: \w+)*) \((\w+)\)\s+\n+\s+([\d.]+)'
    matches = re.findall(pattern, text)
    
    return pd.DataFrame(matches, columns = ["Element", "Unit", "Value"])
