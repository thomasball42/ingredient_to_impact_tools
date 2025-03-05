# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 09:17:05 2025

@author: Thomas Ball
"""

import pandas as pd
import _pdf_tools
import os 
import re 

data_dir = os.path.join("dat", "bwch_data")
pdf_path = os.path.join(data_dir, "RecipeCardRepor Mandala.pdf")

recipes_list = _pdf_tools.extract_recipes_strs(pdf_path)

rdf = pd.DataFrame(columns = ["recipe_name", "servings", "ingredients"])
for rec in recipes_list:
    rdf_idx = len(rdf)
    rname = rec.split("\n")[0]
    rdf.loc[rdf_idx, "recipe_name"] = rname
    smatch = re.search(r"(\d+\.?\d*)\s*Serving?(?=.*\bIngredients\b)", rec, re.DOTALL)
    n_servs = smatch.group(1) if smatch else ""
    rdf.loc[rdf_idx, "servings"] = float(n_servs) if n_servs else None
    imatch = re.search(r"Ingredients\n(.*?)\n(?:\*|Preparation)", rec, re.DOTALL)
    ingredients_str = imatch.group(1).strip() if imatch else "" 
    ingredients = [x.strip() for x in ingredients_str.replace("\u2022", "").split("\n")]
    rdf.at[rdf_idx, "ingredients"] = ingredients
rdf = rdf.explode("ingredients")