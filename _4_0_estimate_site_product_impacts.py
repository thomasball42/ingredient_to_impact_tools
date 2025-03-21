# -*- coding: utf-8 -*-
"""
Created on Fri Mar  7 10:41:47 2025
@author: tom
"""

import pandas as pd
import numpy as np
import os
import tldextract
import tqdm

import _0_4_impact_funcs

SITE_URL = "https://www.brake.co.uk/sitemap.xml"
dat_path = "dat"

site = tldextract.extract(SITE_URL).domain

# where are the files?
site_ingredients_data_dir = os.path.abspath(os.path.join(dat_path, "clark_mod_outputs", site))
impact_data_path = os.path.abspath(os.path.join(dat_path, "food_commodity_impacts_UK.csv"))
commodity_crosswalk_path = os.path.abspath(os.path.join(dat_path, "commodity_crosswalk.csv"))
food_cat_group_path = os.path.abspath(os.path.join(dat_path, "food_cat_groups.csv"))
product_data_path = os.path.abspath(os.path.join(dat_path, "site_data", f"products_{site}.csv"))

impact_df = pd.read_csv(impact_data_path, index_col = 0)
impact_df = impact_df.reset_index(names= ["Item"] + impact_df.columns.to_list())
crosswalk = pd.read_csv(commodity_crosswalk_path, index_col = 0)
food_cat_group = pd.read_csv(food_cat_group_path, index_col = 0)
product_data_df = pd.read_csv(product_data_path, index_col = 0)

# Read in site files from clark output
f = []
for path, subdirs, files in os.walk(site_ingredients_data_dir):
        for name in files:
            f.append(os.path.join(path, name))
df = pd.DataFrame()
for file in [_ for _ in f if "EMBED" in _]:
    try:
        df = pd.concat([df, pd.read_csv(file)])
    except UnicodeDecodeError:
        df = pd.concat([df, pd.read_csv(file, encoding = "latin-1")])
        
#Minor processing
df["percent"] = df.percent.apply(_0_4_impact_funcs.clean_perc)
df.loc[df.Department.isna(), "Department"] = "Unknown"
impact_list = impact_df.columns.to_list()[1:]

# Create dfs for output
composition_impacts_long = pd.DataFrame()
composition_impacts_totals = pd.DataFrame()

# Loop through unique products
for item_id in tqdm.tqdm(df.id.unique(), total = len(df.id.unique())): 
    
    product_df = df[df.id == item_id]
    product_df.loc[:, "Food_Category"] = product_df.Food_Category.copy().fillna("other ingredients")
    product_df = product_df.drop(columns = ["variable", "value", "Food_Category_sub", 
                              "Food_Category_sub_sub", 
                              "value_not_embedded", 
                              "total_composition"])
    product_df_gsum = product_df.groupby(product_df.columns.drop("percent").to_list()).percent.sum().reset_index()
    missing_perc = product_df_gsum.loc[[0], product_df_gsum.nunique() == 1].copy()
    missing_perc.loc[:, ["Food_Category", "percent"]] = ["unknown", 0 if (100 - product_df_gsum.percent.sum()) < 0 else 100 - product_df_gsum.percent.sum()]
    product_df_gsum = pd.concat([product_df_gsum, missing_perc])
    product_df_gsum = product_df_gsum.reindex(columns=product_df_gsum.columns.tolist() + impact_list)
    product_df_gsum["group_v7"] = ""
    
    for idx, row in product_df_gsum.iterrows():
        fc = _0_4_impact_funcs.name_mismatches(row.Food_Category) 
        pc = row.percent
        if fc in crosswalk.LCA_name.to_list():
            cw = crosswalk[crosswalk.LCA_name == fc]
            impact_name = cw.Item.squeeze()
            if isinstance(impact_name, str):
                impacts = impact_df.loc[impact_df.Item == impact_name].squeeze()[1:]
                
                if len(impacts) > 0:
                    product_df_gsum.loc[product_df_gsum.Food_Category==fc, 
                                        [x+"_ingred" for x in impact_list]] = impacts.values
                    product_df_gsum.loc[product_df_gsum.Food_Category==fc, 
                                        [x+"_prod" for x in impact_list]] = (impacts *(pc / 100)).values
                    gname = food_cat_group[food_cat_group.Food_category == fc].group_name_v7.squeeze()
                    product_df_gsum.loc[product_df_gsum.Food_Category==fc, "group_v7"] = gname

            else:
                impacts = impact_df.loc[impact_df.Item.isin(impact_name)] 
                if len(impacts) > 0:
                    for col in impacts.iloc[:, 1:]:    
                        vals = impacts[col]
                        wm = _0_4_impact_funcs.weighted_quantile(vals, 0.5) 
                        product_df_gsum.loc[idx, col+"_ingred"] = wm
                        product_df_gsum.loc[idx, col+"_prod"] = wm * (pc / 100)
                gname = food_cat_group[food_cat_group.Food_category == fc].group_name_v7.squeeze()
                if isinstance(gname, str):
                    product_df_gsum.loc[idx, "group_v7"] = gname
                else:
                    pass
        else:
            product_df_gsum.loc[product_df_gsum.Food_Category==fc, 
                                [x+"_ingred" for x in impact_list]] = [np.nan for _ in impact_list]
            product_df_gsum.loc[product_df_gsum.Food_Category==fc, 
                                [x+"_prod" for x in impact_list]] = [np.nan for _ in impact_list]
    
    composition_impacts_long = pd.concat([composition_impacts_long, product_df_gsum])

    # Calcualte totals and pack based impacts
    long_comp = composition_impacts_long[composition_impacts_long.id == item_id] # redundant structuring but minor efficiency gains aren't worth it.
    comp = long_comp.loc[:, ~long_comp.columns.str.contains("_ingred")]
    prod_sums = comp.reset_index(drop=True).loc[[0], comp.nunique() == 1].copy()
    product_data = product_data_df.loc[item_id]
    items_in_pack = product_data.items_in_pack
    pack_mass_kg = product_data.item_mass_g * items_in_pack / 1000
    portions_in_pack = product_data.portions_in_pack
    prod_sums[["items_in_pack", "portions_in_pack", "pack_mass_kg"]] = items_in_pack, portions_in_pack, pack_mass_kg
    composition_impacts_long.loc[composition_impacts_long.id == item_id, ["items_in_pack", "portions_in_pack", "pack_mass_kg"]] = items_in_pack, portions_in_pack, pack_mass_kg                    

    for i, impact in enumerate(impact_list):
        impact_val = np.nansum(comp[impact + "_prod"])
        if impact_val == 0:
            impact_val = np.nan
        #update totals
        prod_sums[impact + "_prod"] = impact_val
        prod_sums[impact.replace("_per_kg", "_per_pack")] = impact_val * pack_mass_kg
        #update long dat
        composition_impacts_long.loc[composition_impacts_long.id == item_id, impact.replace("_per_kg", "_per_pack")] = comp[impact + "_prod"] * pack_mass_kg
    composition_impacts_totals = pd.concat([composition_impacts_totals, prod_sums])
composition_impacts_totals = composition_impacts_totals.drop(columns="percent") # really not sure where this 'percent' column comes from - it's not in prod_sums...

if not os.path.isdir("outputs"):
    os.makedirs("outputs", exist_ok=True)
    
composition_impacts_long.to_csv(os.path.join("outputs", f"{site}_composition_impacts_long.csv"))
composition_impacts_totals.to_csv(os.path.join("outputs", f"{site}_composition_impacts_totals.csv"))
