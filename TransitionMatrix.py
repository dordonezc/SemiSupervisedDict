import numpy as np 
from scipy import sparse

## Transition Matrix
def similarity_matrix(embeddings, arccos=False, similarity_power=1, nn=25, **kwargs):
        """
        Constructs a similarity matrix from embeddings.
        nn argument controls the degree.
        """
        def make_knn(vec, nn=nn):
            vec[vec < vec[np.argsort(vec)[-nn]]] = 0
            return vec
        
        L = embeddings.dot(embeddings.T)
        if sparse.issparse(L):
            L = L.todense()
        if arccos:
            L = np.arccos(np.clip(-L, -1, 1))/np.pi
        else:
            L += 1
        np.fill_diagonal(L, 0)
        L = np.apply_along_axis(make_knn, 1, L)
        return L ** similarity_power

def transition_matrix(embeddings, **kwargs):
    """
    Calls similarity matrix. Builds a probabilistic transition matrix
    from word embeddings.
    """
    L = similarity_matrix(embeddings, **kwargs)
    #Dinv = np.diag([1. / L[i].sum() for i in range(L.shape[0])])
    #L = L.dot(Dinv)
    ## Symmetric?
    Dinv = np.diag([1. / np.sqrt(L[i].sum()) if L[i].sum() > 0 else 0 for i in range(L.shape[0])])
    L = Dinv.dot(L).dot(Dinv)
    return L