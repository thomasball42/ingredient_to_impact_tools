# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 09:31:59 2025

@author: Thomas Ball
"""

import os
import rpy2.robjects as ro
# import urllib
import tldextract

SITE_URL = "https://www.brake.co.uk/sitemap.xml"
dat_path = "dat"


site = tldextract.extract(SITE_URL).domain

# Create args
args = {"arg1" : os.path.abspath(os.getcwd()),
        "arg2" : os.path.abspath(os.path.join(dat_path, "clark_mod_outputs", site)),
        "arg3" : os.path.abspath(os.path.join(dat_path, "site_data", f"products_{site}.csv")),
        "arg4" : os.path.abspath(os.path.join(dat_path, "site_data", f"categories_{site}.csv")),
        
        # constant
        "arg5" : os.path.abspath(os.path.join(dat_path, "clark_mod", "0.0_Functions_Estimating_Composition_22January2022.R")),
        "arg6" : os.path.abspath(os.path.join(dat_path, "clark_data", "Clark_et_al_2022_PNAS_SM", "Data Inputs"))

        }

if not os.path.isdir(os.path.join(dat_path, "clark_mod_outputs", site)): 
    os.makedirs(os.path.join(dat_path, "clark_mod_outputs", site), exist_ok=True)
   
packages = ["plyr", "dplyr", "readr", "stringr", "stringi", "reshape2", "dismo"]
for package in packages:
    ro.r(f'if (!requireNamespace("{package}", quietly = TRUE)) install.packages("{package}", repos="http://cran.r-project.org")')

estimating_comps_script_path = os.path.abspath(os.path.join(
    "dat", "clark_mod", "1.0_Estimating_Composition_21January2022.R"))

# create args for script
for arg, arg_val in args.items():
    ro.r.assign(arg, arg_val)

if os.path.isfile(estimating_comps_script_path):
    ro.r.source(estimating_comps_script_path)
else:
    print(f"Can't find {estimating_comps_script_path}")
