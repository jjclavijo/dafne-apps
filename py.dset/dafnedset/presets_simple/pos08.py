"""
A pre-set for positive samples, labeled 0.8,0.2

This showld probably be gone.

- Take pos10 samples
- Relabel

"""

from .pos10 import parts

from .. import fun_ops as fop

from .. import fun_transformations as ftr

parts = parts * 1

for i in range(len(parts)):
    parts[i] = fop.map(parts[i],ftr.label_batch([0.8,0.2]))

data = parts[0]
