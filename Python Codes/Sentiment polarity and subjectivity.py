#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

"""

pip install textblob

import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline  
import nltk
nltk.download('stopwords')
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer, PorterStemmer
from wordcloud import WordCloud, STOPWORDS
from textblob import TextBlob

amazon_reviews = pd.read_excel("~/reviews_DC_June_2022.xlsx")
amazon_reviews.shape
amazon_reviews.columns
#columns = ['id','name','keys','manufacturer','reviews.dateAdded', 'reviews.date','reviews.didPurchase','reviews.userCity', 'reviews.userProvince', 'reviews.dateSeen', 'reviews.doRecommend','asins','reviews.id', 'reviews.numHelpful', 'reviews.sourceURLs', 'reviews.title']
#df = pd.DataFrame(playstore_reviews.drop(columns,axis=1,inplace=False))
df = pd.DataFrame(amazon_reviews)
#df['score'].value_counts().plot(kind='bar')
## Change the reviews type to string
df['comments'] = df['comments'].astype(str)
#df['Review.Title'] = df['Review.Title'].astype(str)

## Before lowercasing 
df['comments'][2]
## Lowercase all reviews
df['comments'] = df['comments'].apply(lambda x: " ".join(x.lower() for x in x.split()))
df['comments'][2] ## to see the difference
## remove punctuation
#df['content'] = df['content'].str.replace('[^ws]','')
#df['content'][2]
stop = stopwords.words('english')
df['comments'] = df['comments'].apply(lambda x: " ".join(x for x in x.split() if x not in stop))
df['comments'][2]
st = PorterStemmer()
df['comments'] = df['comments'].apply(lambda x: " ".join([st.stem(word) for word in x.split()]))
df['comments'][2]
## Define a function which can be applied to calculate the score for the whole dataset

def senti(x):return TextBlob(x).sentiment  
df['senti_score'] = df['comments'].apply(senti)
#df['senti_score_title'] = df['Review.Title'].apply(senti)
df['polarity'] = df['comments'].apply(lambda df: TextBlob(df).polarity)
df['subjectivity'] = df['comments'].apply(lambda df: TextBlob(df).subjectivity)
#df['polarity_title'] = df['Review.Title'].apply(lambda df: TextBlob(df).polarity)
#df['subjectivity_title'] = df['Review.Title'].apply(lambda df: TextBlob(df).subjectivity)
df.senti_score.head()
df.head()
df.shape

df.to_excel('/Users/poojasengupta/Documents/Research/Tourism/Data/Washington DC/DC_sentiment.xlsx', header = True, index=False) 
