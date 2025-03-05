# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 08:10:23 2024

@author: Thomas Ball
"""

import os
import pymupdf as pm
import re
import pandas as pd 

def extract_recipes_strs(pdf_path):
    doc = pm.Document(pdf_path)
    page_count = doc.page_count
    metadata = doc.metadata
    recipes_list = []
    dtext = None
    for page in doc.pages():   
        text = page.get_text()
        if "Ingredients" in text:
            if dtext:
                recipes_list.append(dtext)
            dtext = text
        else:
            dtext = dtext + text
    if dtext:
        recipes_list.append(dtext)
    return recipes_list

