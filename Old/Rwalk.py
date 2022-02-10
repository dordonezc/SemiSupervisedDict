import networkx as nx
import os 
import pandas as pd
import numpy as np
import re
os.chdir("C:/Users/dordo/Dropbox/Capstone Project/Data/Others")

## Load Embeddings
data = pd.read_csv("Embed.csv")
embed = np.array(data.drop(["name"], axis = 1).T)

## Define auxiliary function to return window nns and assign weight to edges
def get_neighbors(ind, window = 5):
    aux = np.matmul(embed[:,ind], embed)/50
    nns = np.argsort(aux)[-2:(-2-window):-1]
    vals = np.arccos(-aux[nns])
    return nns, vals

## Iterate function above
all_nns, all_dist = [], []
for index in range(data.shape[0]):   
    res = get_neighbors(index)
    all_nns.append(res[0])
    all_dist.append(res[1])

## Edge list
edges = [(i, elem) for i, item in enumerate(all_nns) for elem in item]
dist = np.reshape(all_dist, (len(edges),))
edges_w = [(*elem, weight) for elem, weight in zip(edges, dist)]

##-------------------------------------------------------------------------##
## Define graph
G = nx.Graph()
G.add_nodes_from(list(range(len(all_nns))))

## Add edges
G.add_weighted_edges_from(edges_w)

## Delete
del edges, dist, all_nns, all_dist
 
##-------------------------------------------------------------------------## 
## Find positions of seed words
def easy_search(x):
    pattern = re.compile(x)
    return [(index, elem) for index, elem in enumerate(data.name) if pattern.match(elem) != None]

def pool_search(x):
    good_elems = []
    for elem in x:
        good_elems.extend(easy_search(elem))
    return good_elems

## Good Seeds
good_seed = ["succes", "excel", "^profit", "benefic", "improv", 
               "positiv", "^gain"]

good_elems = pool_search(good_seed)

## Bad Seeds
bad_seed = ["^los", "volati", "wron", "damag", "^bad", "litiga",
  "fail", "negat", "downg", "lock[^c]", "down[swt].",
  "^[^ld].+down$"]
bad_elems = pool_search(bad_seed)

##--------------------------------------------------------------------------##
## Random Walking: Select seed 
#nx.dijkstra_path(G, 609, 5)

## Calculations for Markov Chain

## Adjacency matrix
A = nx.adj_matrix(G)
A = np.array(A.todense(), dtype = np.float64)

# Degree Matrix
Di = np.diag(1/np.sum(A, axis=0))

# Transition Matrix 
Tr = np.dot(Di,A)

# Random Walking
def go_walk(start, wl = 10):
    ## Starting point (seed)
    visited = list()
    p = start
    for k in range(wl):
        # Probabilities of moving
        p_aux = Tr[:,p]
        care =  np.where(p_aux > 0)[0]
        care_ps = np.reshape(p_aux[care]/(np.sum(p_aux[care])),(-1))
        p = np.random.choice(care, p = care_ps)
        visited.append(p)
    return visited

def exercise(x, times = 500, wl = 50):
    res = []
    for i in range(times):
        res.extend(go_walk(x, wl = wl))
    aux = np.bincount(res, minlength = data.shape[0])
    res =  np.array([0 if index == x else val for index, val in enumerate(aux)])
    return res

node = 2787
out = exercise(node)
data.name[np.argsort(out)]

np.bincount(res)
##--------------------------------------------------------------------------##
