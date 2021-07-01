"""

A pre-set for augmented positive data relabeled as negative.

This is for usage in deceiver training, which targets an all-negative response
of the classifier.

"""

from .total_pos import data as total_pos

data = total_pos.label(label=[0.,1.])
