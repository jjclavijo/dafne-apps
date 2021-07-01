"""
A pre-set for positive samples

- Query samples
- split in Train, Test, Val, Show

"""

from .. import datasets as defaultQuerys
from .. import fun_ops as fop

from .. import fun_transformations as ftr

#raw = DefaultQuerys.positive().label(label=[1.,0.]).scale()
raw = fop.read_db(defaultQuerys.CASES)

raw = fop.map(raw,ftr.label_batch([1.,0.]))

parts = fop.split(raw,[0.7,0.15,0.145,0.005]) # Train, Test, Val, show

data = parts[0]

#data = source.feed(batch_size=source.length,max_length=source.length,force=True)
