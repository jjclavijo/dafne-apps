"""
Funcione para convertir un dataset a tensorflow
"""

from .dataspec import adapter as tfAdapter
from ..base_simple import FunBuffer
from tensorflow.data import Dataset
from tensorflow import stack

PRACTICAMENTE_IFTO = 1e10

def dafneFbToTf(p: FunBuffer) -> Dataset:

    #p = neg10.parts[0] + pos10.parts[0]

    adp = tfAdapter(p._get_schema())

    p.options.batch_size = PRACTICAMENTE_IFTO

    for ix,i in enumerate(p):
        print(ix)
        data = adp.ToBatchTensors(i)

    transformer = lambda d: (stack((d['norte'],d['este'],d['altura']),axis=-1),d['etiqueta'])

    dset = Dataset.from_tensor_slices(data).map(transformer)#.shuffle(int(1e7))

    return dset

