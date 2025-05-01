import requests
from fake_useragent import UserAgent

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

def fetch_page(url, connect_method="default"):
    """
    Fetches a page using the specified connection method.
    """
    if connect_method == "user_agent_rotation":
        return fetch_with_user_agent_rotation(url)
    elif connect_method == "default":
        try:
            response = requests.get(url)
            response.raise_for_status()
            return response
        except requests.exceptions.RequestException as e:
            print(f"Error fetching URL with default method: {e}")
            return None
    else:
        raise ValueError(f"Unknown connection method: {connect_method}")