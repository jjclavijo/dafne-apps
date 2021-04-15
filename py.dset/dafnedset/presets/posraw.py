"""
A pre-set for positive samples

- Query samples
- split in Train, Test, Val, Show

"""

from ..datasets import DefaultQuerys

source = DefaultQuerys.positive().label(label=[1.,0.]).scale()

data = source.feed(batch_size=source.length,max_length=source.length,force=True)
