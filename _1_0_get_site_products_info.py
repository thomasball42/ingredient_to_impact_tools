import _0_1_site_scrape_functions as ssf
import _0_1_page_parsing_functions as ppf
import _0_1_connection_functions as cf

import pandas as pd
import os
from tqdm import tqdm
import tldextract
import requests

OVERWRITE = True

SITE_CONFIGS = {
    # "brake": {
    #     "sitemap_url": "https://www.brake.co.uk/sitemap.xml",
    #     "reject_list": ["catering-supplies"],
    #     "get_url_df_func": ssf._get_url_df_brakes,
    #     "connect_method": "default",  # No special connection method needed
    # },
    "tesco": {
        "sitemap_url": "https://www.tesco.com/groceries/sitemap/UK.en.pdp.sitemap.xml",
        "reject_list": [],
        "get_url_df_func": ssf._get_url_df_tesco,
        "connect_method": "user_agent_rotation",  # Use user-agent rotation for Tesco
        "prod_info_funcs": {
            # "Ingredients": ssf.fetch_ingredients_tesco,
            # "Nutrition": ssf.fetch_nutrition_tesco,
            # "Pack size": ssf.fetch_packsize_tesco,
            # "Product name": ssf.fetch_name_tesco,
            # "Dept info": ssf.get_dept_tesco,
        },
    },
}

for site, config in SITE_CONFIGS.items():
    SITEMAP_URL = config["sitemap_url"]
    CONNECT_METHOD = config.get("connect_method", "default")  # Default to "default" if not specified
    
    # Fetch the sitemap using the specified connection method
    response = cf.fetch_page(SITEMAP_URL, connect_method=CONNECT_METHOD)
    if response is None:
        print(f"Failed to fetch sitemap for {site}")
        continue
    
    # Process the sitemap content
    sitemap_content = response.text  # Example: process the sitemap content as needed
    
    cache_data_dir = os.path.join("dat", "site_data")
    site_name = tldextract.extract(SITEMAP_URL).domain
    cache_data_name = f"{site_name}_site_data.csv"
    cache_data_path = os.path.join(cache_data_dir, cache_data_name)
    
    os.makedirs(cache_data_dir, exist_ok=True)
    
    GET_URL_DF_FUNC = config["get_url_df_func"]
    df = GET_URL_DF_FUNC(SITEMAP_URL, cache_data_path, overwrite=OVERWRITE)
   
    REJECT_LIST = config["reject_list"]
    PROD_INFO_FUNCS = config["prod_info_funcs"]
    
    for idx, row in tqdm(df.iterrows(), total=len(df), desc=f"Scraping via {SITEMAP_URL}"):
        if pd.notnull(row.get("status_code")):
            continue
        
        if not pd.isnull(row.iloc[6]):
            page_url = row.URL
            if any(r in page_url for r in REJECT_LIST):
                df.loc[idx, "RFLAG"] = "non_food"
            else:
                soup, status_code = ssf._get_soup(page_url)
                df.loc[idx, "status_code"] = status_code
                if status_code != 404:
                    for fstring, fetch_func in PROD_INFO_FUNCS.items():
                        df.loc[idx, f"{fstring}_string"] = fetch_func(soup)
                        
        if idx % 10 == 0:
            pass
            # df.to_csv(cache_data_path)
            
    # df.to_csv(cache_data_path)
    
    # Processing info
    for idx, row in tqdm(df.iterrows(), total=len(df), desc="Processing info"):
        if "Nutrition_string" in row and isinstance(row.Nutrition_string, str):
            nutrition_text_cdf = sipf.clean_nutrition_str(row.Nutrition_string)
            df.loc[idx, nutrition_text_cdf.Element + " (" + nutrition_text_cdf.Unit + ")"] = \
                nutrition_text_cdf.Value.astype(float).values
    
        if "Pack size_string" in row and isinstance(row["Pack size_string"], str):
            ps_text, prod_name = row["Pack size_string"], row["Product name_string"]
            df.loc[idx, ["item_mass_g", "items_in_pack", "portions_in_pack", "PACKFLAG"]] = sipf.clean_pack_size(ps_text, prod_name)
    
    # df.to_csv(cache_data_path)
