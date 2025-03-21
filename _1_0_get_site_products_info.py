import _0_1_site_scrape_tools as sst
import pandas as pd
import os
from tqdm import tqdm
import tldextract

OVERWRITE = False
SITE_URL = "https://www.brake.co.uk/sitemap.xml"

SITE_CONFIGS = {
    "brake": {
        "reject_list": ["catering-supplies"],
        "header_strings": {
            "Ingredients": sst.fetch_ingredients_brakes,
            "Nutrition": sst.fetch_nutrition_brakes,
            "Pack size": sst.fetch_packsize_brakes,
            "Product name": sst.fetch_name_brakes,
        },
    },
    # "example": {  # Placeholder for another site
    #     "reject_list": ["exclude-category"],
    #     "header_strings": {
    #         "Ingredients": sst.fetch_ingredients_example,
    #         "Nutrition": sst.fetch_nutrition_example,
    #         "Pack size": sst.fetch_packsize_example,
    #         "Product name": sst.fetch_name_example,
    #     },
    # },
}

cache_data_dir = os.path.join("dat", "site_data")
site_name = tldextract.extract(SITE_URL).domain
cache_data_name = f"{site_name}_site_data.csv"
cache_data_path = os.path.join(cache_data_dir, cache_data_name)

if site_name in SITE_CONFIGS:
    config = SITE_CONFIGS[site_name]
    REJECT_LIST = config["reject_list"]
    HEADER_STRINGS = config["header_strings"]
else:
    raise ValueError(f"No configuration found for site: {site_name}")


os.makedirs(cache_data_dir, exist_ok=True)

df = sst._get_url_df(SITE_URL, cache_data_path, overwrite=OVERWRITE)

for idx, row in tqdm(df.iterrows(), total=len(df), desc=f"Scraping via {SITE_URL}"):
    if pd.notnull(row.get("status_code")):
        continue
    
    if not pd.isnull(row.iloc[6]):
        page_url = row.URL
        if any(r in page_url for r in REJECT_LIST):
            df.loc[idx, "RFLAG"] = "non_food"
        else:
            soup, status_code = sst._get_soup(page_url)
            df.loc[idx, "status_code"] = status_code
            if status_code != 404:
                for fstring, fetch_func in HEADER_STRINGS.items():
                    df.loc[idx, f"{fstring}_string"] = fetch_func(soup)
                    
    if idx % 10 == 0:
        df.to_csv(cache_data_path)
        
df.to_csv(cache_data_path)

# Processing info
for idx, row in tqdm(df.iterrows(), total=len(df), desc="Processing info"):
    if "Nutrition_string" in row and isinstance(row.Nutrition_string, str):
        nutrition_text_cdf = sst.clean_nutrition_str(row.Nutrition_string)
        df.loc[idx, nutrition_text_cdf.Element + " (" + nutrition_text_cdf.Unit + ")"] = \
            nutrition_text_cdf.Value.astype(float).values

    if "Pack size_string" in row and isinstance(row["Pack size_string"], str):
        ps_text, prod_name = row["Pack size_string"], row["Product name_string"]
        df.loc[idx, ["item_mass_g", "items_in_pack", "portions_in_pack", "PACKFLAG"]] = sst.clean_pack_size(ps_text, prod_name)

df.to_csv(cache_data_path)
