# -*- coding: utf-8 -*-
"""
Created on Mon Feb 17 09:31:59 2025

@author: Thomas Ball
"""

import os
import rpy2.robjects as ro

os.environ["PYTHONIOENCODING"] = "latin-1"

packages = ["plyr", "dplyr", "readr", "stringr", "stringi", "reshape2", "dismo"]
for package in packages:
    ro.r(f'if (!requireNamespace("{package}", quietly = TRUE)) install.packages("{package}", repos="http://cran.r-project.org")')


clark_mod_path = os.path.join("clark_mod")

est_comp1_path = os.path.abspath(os.path.join(clark_mod_path, "1.0_Estimating_Composition_21January2022.R"))
if os.path.isfile(est_comp1_path):
    ro.r.source(est_comp1_path)
else:
    print(f"Can't find {est_comp1_path}")

# est_comp1_path = os.path.abspath(os.path.join(clark_mod_path, "1.0_Estimating_Composition_21January2022.R"))
# if os.path.isfile(est_comp1_path):
#     ro.r.source(est_comp1_path)
# else:
#     print(f"Can't find {est_comp1_path}")