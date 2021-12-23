"""
A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

"""

from .. import datasets_simple as defaultQuerys
from .. import fun_ops as fop

from .. import fun_transformations as ftr

raw = fop.read_db(defaultQuerys.NO_CASES)

raw = fop.map(raw,ftr.label_batch([0.,1.]))
raw = fop.map(raw,ftr.scale())

# Tremendo side efect!
raw.options.batch_size = 200

parts = fop.split(raw,[0.7,0.15,0.145,0.005]) # Train, Test, Val, show

# nr of nan    0     1     2     3     4     5     6     7     8     9
nan_dist = [5726,  524,  185,  117,   86,   71,   40,   44,   22,   12]
drop = ftr.drop_epochs(distribution=nan_dist)

data = fop.map(parts[0],drop)

data = fop.map( data,ftr.scale() )
