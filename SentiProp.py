import os 
os.chdir("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
import numpy as np
from sklearn.preprocessing import normalize
import matplotlib.pyplot as plt
import pandas as pd 
from RandomWalk import random_walk

## Objective: Use Hamilton method to induce dictionary

## Bad words
bad_words = pd.read_csv("Data/rootBadMc.csv")
bad_words = np.reshape(bad_words.values, -1)

## Good Words
good_words = pd.read_csv("Data/rootGoodMc.csv")
good_words = np.reshape(good_words.values, -1)

## Read embeddings
w_embed = pd.read_csv("Embed.csv")
words = w_embed.name
embeddings = w_embed.drop(["name"], axis=1).values
normalize(embeddings, copy=False)

## Sentiprop
polarities = random_walk(words, embeddings, good_words, bad_words, beta=0.99, nn=25,
                         arccos=False)

polarities_df = pd.DataFrame(polarities.items())
polarities_df.columns = ["words", "polarity"]
polarities_df.sort_values("polarity",inplace = True)

## Plot
plt.hist(polarities.values())

## To Csv
polarities_df.to_csv("data/SentiPropPolaritiesMc.csv", index = False)
