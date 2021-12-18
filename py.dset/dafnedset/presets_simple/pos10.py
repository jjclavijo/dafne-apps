"""
A pre-set for positive samples

- Query samples
- split in Train, Test, Val, Show

"""

from .. import datasets_simple as defaultQuerys
from .. import fun_ops as fop

from .. import fun_transformations as ftr

raw = fop.read_db(defaultQuerys.CASES)

raw = fop.map(raw,ftr.label_batch([1.,0.]))
raw = fop.map(raw,ftr.scale())

# Tremendo side efect!
raw.options.batch_size = 200

parts = fop.split(raw,[0.7,0.15,0.145,0.005]) # Train, Test, Val, show

data = parts[0]

