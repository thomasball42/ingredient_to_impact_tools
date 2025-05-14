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

import numpy as np
import tqdm
import time
import random

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
    
