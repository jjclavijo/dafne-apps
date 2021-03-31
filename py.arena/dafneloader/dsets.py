#Local imports
#from . import data_providers as dp
#from .get_dataset import make_cached_dataset
from ._constants import CACHE_DIR, DATA_TYPES, DATA_SHAPES

#Global imports
import os
import tensorflow as tf

# The minimum possible dummy generator,
# Will never be iterated, data will allways be cached.

def dummy_gen(): yield None

def dummy_ds():
    return tf.data.Dataset.from_generator(dummy_gen,DATA_TYPES,DATA_SHAPES)

# Hardcoded Dataset Names.

train_dataset = dummy_ds()\
                .cache(os.path.join(CACHE_DIR,'train_ds'))\
                .shuffle(200000)

test_dataset = dummy_ds()\
                .cache(os.path.join(CACHE_DIR,'test_ds'))\
                .shuffle(100000)

train_dataset_e = dummy_ds()\
                .cache(os.path.join(CACHE_DIR,'train_e_ds'))\
                .shuffle(100000)

test_dataset_e = dummy_ds()\
                .cache(os.path.join(CACHE_DIR,'test_e_ds'))\
                .shuffle(100000)

tde = train_dataset_e.repeat()

tede = test_dataset_e

dtede = test_dataset

#To Show
show = dummy_ds()\
                .cache(os.path.join(CACHE_DIR,'show'))
