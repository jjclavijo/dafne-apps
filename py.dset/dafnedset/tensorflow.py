"""
Funciones específicas para convertir un dataset a tensorflow
"""
#Local imports
#from . import data_providers as dp
#from ._constants import CACHE_DIR,SI_SOCKET_FILE,SI_SOCKDIR

#Global imports
import os
import tensorflow as tf
import socket
import pyarrow as pa
import numpy as np

import logging

log = logging.getLogger(__name__)

"""
Arrow <-> Tensorflow Conversions

TODO: This certainly can be cleaner
"""

def toDataset(reader, # some generator of arrow objects
                        data_shape=tf.TensorShape([61,3]),
                        label_shape=tf.TensorShape([2])
                        ):
    """
    from an iterator of pyArrow RecordBatchs (reader)

    Returns a tensorflow dataset
    """

    get_gen = lambda: unfold_labeled_batches(reader)
    dset = tf.data.Dataset.from_generator(get_gen,
                                          (tf.float32,tf.float32),
                                          (data_shape, label_shape)
                                          )

    return dset

COLUMNAS = ['este','norte','altura']
LABELS = 'etiqueta'

def unfold_labeled_batches(reader):
    """
    turn pyArrow RecordBatch iterator into individual data,label python tuples
    """
    try:
      columns=None
      labels=None
      for batch in reader:
        if columns is None:
          columns = [batch.schema.get_field_index(i)
                     for i in COLUMNAS]
          labels = batch.schema.get_field_index(LABELS)

        data = batch.to_pandas()
        data_l = data.iloc[:,labels]
        data_d = data.iloc[:,columns]

        data_d = data_d.values

        for row,l in zip(data_d,data_l):
          row = np.stack(row)
          yield row.T,l#(np.nan_to_num(row.T,nan=-99.0),l)

    # If reader is already exhausted, keep StopIterating
    except pa.ArrowIOError:
        pass

# Unused, maybe in the future we want to generate some unlabeled dataset.

def unfold_unlabeled_batches(reader):
    """
    turn RecordBatch iterator into individual data tuples
    """
    try:
      columns=None
      for batch in reader:
        if columns is None:
          columns = [batch.schema.get_field_index(i)
                     for i in COLUMNAS]

        data = batch.to_pandas()
        data_d = data.iloc[:,columns]

        data_d = data_d.values

        for row in data_d:
          row = np.stack(row)
          yield row.T#np.nan_to_num(row.T,nan=-99.0)

    # If reader is already exhausted, keep StopIterating
    except pa.ArrowIOError:
        pass
