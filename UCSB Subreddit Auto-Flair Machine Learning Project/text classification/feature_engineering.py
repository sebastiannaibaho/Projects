import pickle
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.model_selection import train_test_split
from pathlib import Path
import collections

Path("/Pickles").mkdir(parents=True, exist_ok=True)

df = pd.read_json('Data/Processed/UCSB_manually_added.json')
df = df.sort_values(by='Category')

# let's try subsetting the dataframe with only posts with flairs already
df_sorted = df[df['Category'] != 'None']
df_sorted = df_sorted[df_sorted['Category'] != 'Image']

print(len(df_sorted))
#df_sorted = df_sorted.iloc[[i for i in range(0, len(df_sorted)) if i % 10 == 0]]
#print(len(df_sorted))

# Label coding
category_codes = {}
categories = list(set(df_sorted['Category']))
[category_codes.update({categories[i]: i}) for i in range(0, len(categories))]

# add category codes
df_sorted['Category_Code'] = df_sorted['Category']
df_sorted = df_sorted.replace({'Category_Code': category_codes})

# get rid of categories with less than 3 posts
#deleted = collections.Counter(df_sorted['Category_Code'])
#df_sorted = df_sorted[[deleted[i] >= 3 for i in df_sorted['Category_Code']]]

# convert array to string
df_sorted['Content'] = [" ".join(i) for i in df_sorted['Content']]

# Train-test split
X_train, X_test, y_train, y_test = train_test_split(df_sorted['Content'],
                                                    df_sorted['Category_Code'],
                                                    test_size=0.01,  # low because of small sample size (for now)
                                                    random_state=8)


# Text representation (TF-IDF Vectors)
# Parameter election
ngram_range = (1, 2)
min_df = .01
max_df = .9
max_features = 300
tfidf = TfidfVectorizer(encoding='utf-8',
                        ngram_range=ngram_range,
                        stop_words=None,
                        lowercase=False,
                        max_df=max_df,
                        min_df=min_df,
                        max_features=max_features,
                        norm='l2',
                        sublinear_tf=True)

features_train = tfidf.fit_transform(X_train).toarray()
labels_train = y_train

features_test = tfidf.transform(X_test).toarray()
labels_test = y_test

from sklearn.feature_selection import chi2
import numpy as np


def get_unigrams(topic):
    for Product, category_id in sorted(category_codes.items()):
        if topic == Product:
            features_chi2 = chi2(features_train, labels_train == category_id)
            indices = np.argsort(features_chi2[0])
            sort_chi2 = sorted(features_chi2[0].tolist())
            feature_names = np.array(tfidf.get_feature_names())[indices]
            nfeature_names = feature_names.tolist()
            list_bi = []
            for v in nfeature_names:
                if len(v.split(' ')) == 2:
                    list_bi.append(nfeature_names.index(v))
            j = 0
            for i in list_bi:
                sort_chi2.pop(i - j)
                j += 1
            unigram_keys = {}
            unigrams = [v for v in feature_names if len(v.split(' ')) == 1]
            for k in range(0, len(sort_chi2)):
                unigram_keys[unigrams[k]] = sort_chi2[k]
    return unigram_keys


# Save files for later
# X_train
with open('Models/Pickles/X_train.pickle', 'wb+') as output:
    pickle.dump(X_train, output)

# X_test
with open('Models/Pickles/X_test.pickle', 'wb+') as output:
    pickle.dump(X_test, output)

# y_train
with open('Models/Pickles/y_train.pickle', 'wb+') as output:
    pickle.dump(y_train, output)

# y_test
with open('Models/Pickles/y_test.pickle', 'wb+') as output:
    pickle.dump(y_test, output)

# df
with open('Models/Pickles/df.pickle', 'wb+') as output:
    pickle.dump(df_sorted, output)

# features_train
with open('Models/Pickles/features_train.pickle', 'wb+') as output:
    pickle.dump(features_train, output)

# labels_train
with open('Models/Pickles/labels_train.pickle', 'wb+') as output:
    pickle.dump(labels_train, output)

# features_test
with open('Models/Pickles/features_test.pickle', 'wb+') as output:
    pickle.dump(features_test, output)

# labels_test
with open('Models/Pickles/labels_test.pickle', 'wb+') as output:
    pickle.dump(labels_test, output)

# TF-IDF object
with open('Models/Pickles/tfidf.pickle', 'wb+') as output:
    pickle.dump(tfidf, output)
