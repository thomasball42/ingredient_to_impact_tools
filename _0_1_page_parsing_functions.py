# -*- coding: utf-8 -*-
"""
Created on Tue Apr 29 16:51:12 2025

@author: tom
"""

import re
import numpy as np
import pandas as pd
import requests
from fake_useragent import UserAgent

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

def fetch_with_user_agent_rotation(url):
    """
    Fetches a URL using a rotating user-agent.
    """
    ua = UserAgent()
    headers = {"User-Agent": ua.random}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response
    except requests.exceptions.RequestException as e:
        print(f"Error fetching URL with user-agent rotation: {e}")
        return None