# https://scikit-learn.org/stable/modules/feature_extraction.html

import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.feature_extraction.text import TfidfVectorizer

data = pd.read_csv('job_skill_short.csv')
data = data.drop(['Company','Unnamed: 0'], axis = 1)
category = data['Category'].unique()
location = data['Location'].unique()
title = data['Title'].unique()
pqlist = list(data['Preferred.Qualification'])
mqlist = list(data['Minimum.Qualification'])

for i in range(len(pqlist)):
    pqlist[i] = pqlist[i].replace('\n',' ')
    mqlist[i] = mqlist[i].replace('\n',' ')

vectorizer = CountVectorizer()
mq = vectorizer.fit_transform(mqlist)
mqcount = mq.toarray()
transformer = TfidfTransformer(smooth_idf=False)
tfidf_mq = transformer.fit_transform(mqcount)
mq_featarr = tfidf_mq.toarray()

