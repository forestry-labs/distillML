import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../Python_package'))
print(sys.path)
from sklearn.datasets import load_iris
from forestry import forestry

import numpy as np
import pandas as pd

import pytest

data = load_iris()
df = pd.DataFrame(data['data'], columns=data['feature_names'])
df['target'] = data['target']
X = df.loc[:, df.columns != 'target']
y = df['target']

forest1 = forestry(ntree=1, maxDepth = 2, seed=1,verbose=True)
forest1.fit(X, y)
p1 = forest1.predict(X)

forest2 = forestry(ntree=1, maxDepth = 2, seed=1,verbose=True)
forest2.fit(X, y)
p2 = forest2.predict(X)
