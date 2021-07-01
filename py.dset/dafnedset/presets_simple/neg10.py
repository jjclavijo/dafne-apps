"""
A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

"""

from ..datasets import DefaultQuerys
from ..transformations import BatchProcessing

raw = DefaultQuerys.negative().label(label=[0.,1.]).scale()

parts = raw.split([0.7,0.15,0.145,0.005]).cache() # Train, Test, Val, show

# nr of nan    0     1     2     3     4     5     6     7     8     9
nan_dist = [5726,  524,  185,  117,   86,   71,   40,   44,   22,   12]
drop = BatchProcessing.dropper(distribution=nan_dist)

source = parts[0].preprocess(mix=drop)

data = source.feed(batch_size=source.length,max_length=source.length,force=True)
