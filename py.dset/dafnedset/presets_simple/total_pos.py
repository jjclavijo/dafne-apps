"""
A pre-set for augmented positive Samples

Operates only on the training set.

- Take positive data
- Create 8x length set of data with mixed channels, relabeled.
- Return mixed data along original data. (labeled 0.8,0.2)

"""

from functools import partial

#from .. import transformations as pp
from .. import fun_ops as fop
from .. import fun_transformations as ftr

# import only training Sets
from .pos10 import data as pos10
from .pos08 import data as pos08


#See ch_mixer documentation for relabeling process
pos_mix8 = fop.map(pos10*8,
            ftr.channel_mix_w_relabel(times=1,
                p_true_pos=0.9,p_true_pos_ch=0.9))

data = pos08 + pos_mix8
