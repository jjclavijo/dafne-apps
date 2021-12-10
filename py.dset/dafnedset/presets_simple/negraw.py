"""
A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

"""

from .. import datasets_simple as defaultQuerys
from .. import fun_ops as fop

from .. import fun_transformations as ftr

raw = fop.read_db(defaultQuerys.NOCASES)

raw = fop.map(raw,ftr.label_batch([0.,1.]))

data = fop.map(raw,ftr.scale())

