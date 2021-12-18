"""
A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

"""

from ..datasets import DefaultQuerys
import numpy as np

def gen_sample():
    e = np.random.rand(61) * 1e-3
    n = np.random.rand(61) * 1e-3
    u = np.random.rand(61) * 1e-3
    return (e,n,u)

raw = DefaultQuerys.fake(gen_sample,1000,100)

raw = fop.map(raw,ftr.label_batch([1.,0.]))
data = fop.map(raw,ftr.scale())

