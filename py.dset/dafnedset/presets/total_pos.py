"""
A pre-set for augmented positive Samples

Operates only on the training set.

- Take positive data
- Create 8x length set of data with mixed channels, relabeled.
- Return mixed data along original data. (labeled 0.8,0.2)

"""

from functools import partial

from .. import transformations as pp

# import only training Sets
from .pos10 import data as pos10
from .pos08 import data as pos08

#See ch_mixer documentation for relabeling process
ch_mixer = pp.BatchProcessing.ch_mixer(times=1,p_true_pos=0.9,p_true_pos_ch=0.9)

pos_mix8 = pos10.feed(repeat=True,max_length=pos10.length*8,
                     batch_size=pos10.length)\
               .preprocess(mix=[ch_mixer])

pos08_div8 = pos08.feed(repeat=True,max_length= 8 * ( pos08.length // 8 ),
                        batch_size=(pos08.length // 8) )

data = pos08_div8 + pos_mix8
