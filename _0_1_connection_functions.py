import requests
import bs4
from fake_useragent import UserAgent
import time
import random

def setup_connection(use_proxy=False, rotate_user_agent=True):
    ua = UserAgent()
    headers = {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Accept-Language': 'en-US,en;q=0.5',
        'Accept-Encoding': 'gzip, deflate, br',
        'Connection': 'keep-alive',
        'Referer': 'https://www.tesco.com/',
        'DNT': '1',
        'User-Agent': ua.random if rotate_user_agent else 'Mozilla/5.0'  # Default fallback
    }
    proxies = {
        # 'http': 'http://your_proxy:port',
        # 'https': 'http://your_proxy:port',
        'http': "http://82.102.10.253:80",
        'https': "https://82.102.10.253:80",
        
    } if use_proxy else None
    return headers, proxies


def get_soup(url, use_proxy=False, max_retries=3, delay=2.0):
    headers, proxies = setup_connection(use_proxy=use_proxy, rotate_user_agent=True)
    retry_count = 0
    while retry_count < max_retries:
        try:
            # Rotate UA for each attempt
            headers['User-Agent'] = UserAgent().random
            
            response = requests.get(
                url,
                headers=headers,
                proxies=proxies,
                timeout=30,
                stream=True
            )
            
            # Special handling for 403
            if response.status_code == 403:
                raise ConnectionError("403 Forbidden - Possible blocking")  
            response.raise_for_status()
            return (
                bs4.BeautifulSoup(response.content, 'html.parser'),
                response.status_code,
                None
            )
            
        except requests.exceptions.RequestException as e:
            retry_count += 1
            last_status = getattr(e.response, 'status_code', None)
            last_error = e
            
            if retry_count < max_retries:
                wait_time = min(delay * (2 ** retry_count), 30)  # Exponential backoff
                time.sleep(wait_time + random.uniform(0, 1.5))  # Jitter
                
    return None, last_status, str(last_error)