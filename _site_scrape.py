# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 10:45:46 2024

This script recursivesly looks for pages from a given URL. Using this one a large
site (i.e. something like YouTube) will inevitably lead to a crash)

@author: Thomas Ball
"""

import requests
import bs4
import urllib.parse

def list_site_pages(url):
    """ **recursion**without**proper**safety**features**:)** 
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

