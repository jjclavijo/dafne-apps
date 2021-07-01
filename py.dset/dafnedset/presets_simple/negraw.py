"""
A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

"""

from ..datasets import DefaultQuerys

source = DefaultQuerys.negative().label(label=[0.,1.]).scale()

data = source.feed(batch_size=source.length,max_length=source.length,force=True)
