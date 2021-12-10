"""

A pre-set for fully augmented dataset

- Takes positive and negative samples
- Creates 4x positive and negative sets
- For each set of ( all_positive, all_negative, all )
  - mix channels and relabel.
  - rotate east-north axis.

- Return all mixed data along unmodified data (positives 0.8,0.2).

"""


from .pos10 import data as pos10
from .neg10 import data as neg10
from .pos08 import data as pos08


from .. import fun_ops as fop
from .. import fun_transformations as ftr

# Create 4x sets
pos10x = pos10 * 4
neg10x = neg10 * 4


# Initializate Pre-processors
ch_mixer = ftr.channel_mix_w_relabel(times=1,p_true_pos=0.9,p_true_pos_ch=0.9)
rotator = ftr.random_rot_dir()

d1 = fop.map(fop.map(pos10x,ch_mixer),rotator)
d2 = fop.map(fop.map(neg10x,ch_mixer),rotator)
d3 = fop.map(fop.map(pos10x+neg10x,ch_mixer),rotator)

mixed = d1 + d2 + d3 + pos08 + neg10

# Combine data

