#Local imports
#from . import data_providers as dp
from ._constants import CACHE_DIR,SI_SOCKET_FILE,SI_SOCKDIR

#Global imports
import os
import tensorflow as tf
import socket
import pyarrow as pa
import numpy as np

import logging

log = logging.getLogger(__name__)

NAMES_DIRS_MAP = {
    'total':('train_ds',200000),
    'test_tot':('test_ds',100000),
    'total_pos_n':('train_e_ds',100000),
    'test_pos_n':('test_e_ds',100000),
    'show_tot':('show',100000)}

def build_cache(named_dirs_map=NAMES_DIRS_MAP):
    """
    Build the dataset cache from a dictionary of names and save paths
    """

    # named_dirs_map is
    # A dictionary of datasets:(filename,shuffle_large)
    # TODO: Shuffling is not needed, why were we shuffling?

    cachable = []

    for d,(n,l) in names_dirs_map.items():
        plain = get_dataset(name=d, cached='True')

        dataset = batchGenToDataset(plain)\
                    .cache(os.path.join(CACHE_DIR,n))
                    #.shuffle(l)

        cachable.append(dataset)

    for n,i in enumerate(cachable):
        log.info('Caching dataset {} of {}'.format(n,len(cachable)))
        for j in i:
            pass

    return None

"""
Interaction with dafne-dset

dafne-dset has a URL-like API trough a linux SOCKET.
"""

def get_dataset(**kwargs):
    """
    Get dataset from dafne-dset server, given Name.
    """

    log.info('Starting Conecction')

    sock = socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)
    sock.connect(SI_SOCKET_FILE)
    readpt = sock.makefile('rb')

    request = 'cmd?serve_dataset&{}'.\
               format('&'.join(\
                      ['{}={}'.format(i,j)
                       for i,j in kwargs.items()]))

    log.info('Sending Request: {}'.format(request))

    sock.send(request.encode('utf-8'))

    archivo = sock.makefile('rb')
    reader = pa.RecordBatchStreamReader(archivo)

    for batch in reader:
        yield batch

"""
Arrow <-> Tensorflow Conversions

TODO: This certainly can be cleaner
"""

def batchGenToDataset(reader, # some generator of arrow objects
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
