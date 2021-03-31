import os
import tensorflow as tf

ENV_DEFAULTS= { 'DAFNE_HOME':'/home/javier/dafne'}

DAFNE_HOME = os.environ.get('DAFNE_HOME',ENV_DEFAULTS.get('DAFNE_HOME'))

CACHE_DIR = os.path.join(DAFNE_HOME,'cache/')


FEATURE_SHAPE = tf.TensorShape([61,3])
LABEL_SHAPE = tf.TensorShape([2])

DATA_TYPES = (tf.float32,tf.float32)
DATA_SHAPES = (FEATURE_SHAPE, LABEL_SHAPE)
