# pip install google-play-scraper

from google_play_scraper import app
from google_play_scraper import Sort, reviews
result, continuation_token = reviews('com.cslplasma.cslplasmadonorapplication',lang='en', count=2000)

import pandas as pd
df = pd.DataFrame(result)
df.to_excel('c:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/Python Codes/scraping_playstore_CLSPlasma.xlsx', header=True, index=False)