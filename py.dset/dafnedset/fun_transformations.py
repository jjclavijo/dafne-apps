"""
transformations.py:

Implementation of classes

BatchProcessing: to group every data-modifying method
PreProcessor: A Feeder capable of apply transformations batch_by_batch
and
"""

# Core imports
import logging
from itertools import chain

# External imports
import pyarrow as pa

import numpy as np
import pandas as pd

# Internal imports
#from .base import FeededBuffer,Repeater
#from .extensions import Container,PromisedInt
from functools import partial

log = logging.getLogger(__name__)

### Decorator
from typing import Callable, TypeVar, List, Union, NewType, Any

#Ts = TypeVar("Ts")
RecordBatch = pa.lib.RecordBatch

#TReturn = TypeVar("TReturn")
#def RecordBatch(rb) -> RecordBatch:
#    return rb

def apply_pd_args(
        f: "Callable[[pd.DataFrame,...], pd.DataFrame]"
      ) -> "Callable[[...], Callable[[RecordBatch], RecordBatch]]":
    def inner(*args: ...) -> Callable[[RecordBatch], RecordBatch]:

        def innner( batch: RecordBatch ) -> RecordBatch:
            pdbatch = batch.to_pandas()
            transformed = f(pdbatch, *args)
            return pa.RecordBatch.from_pandas(transformed, preserve_index=False)

        return innner

    return inner

def apply_args(
        f: "Callable[[RecordBatch,...], RecordBatch]"
      ) -> "Callable[[...], Callable[[RecordBatch], RecordBatch]]":
    def inner(*args: ..., **kwargs: ...) -> Callable[[RecordBatch], RecordBatch]:

        def innner( batch: RecordBatch ) -> RecordBatch:
            return f(batch, *args, **kwargs)

        return innner

    return inner

@apply_args
def label_batch(rb: pa.RecordBatch,label:
        Union[int,List[float]],label_col_name: str='etiqueta') -> pa.RecordBatch:
    """
    Label or re-label a record batch.

    rb : pyArrow RecordBarch to label
    label : New label to apply on all records
    label_col_name : Name of the label field.
    """

    many: List[pa.Array] = rb.columns
    how_many: int = len(rb)
    names: List[str] = rb.schema.names
    to_drop: int

    if label_col_name in names:
        to_drop = names.index(label_col_name)
        names = names[:to_drop]+names[to_drop+1:]
        many = many[:to_drop]+many[to_drop+1:]

    rb = pa.RecordBatch.from_arrays([*many,[label]*how_many],
         names=[*names,label_col_name])

    return rb

@apply_pd_args
def scale(batch: pd.DataFrame) -> pd.DataFrame:
    """
    Recenter sample around its mean level.

    rb : pyArrow RecordBarch to label
    label : New label to apply on all records
    label_col_name : Name of the label field.
    """

    for i in ['este','norte','altura']:
        batch.loc[:,i] = batch.loc[:,i] - batch[i].apply(np.nanmean)

    return batch

@apply_args
def channel_mix_w_relabel(batch,times=1,p_true_pos=0.9,p_true_pos_ch=0.9,
                          label_col_name='etiqueta'):
    """
    Given labeled batches (with labels t,f or [1,0],[0,1]
    Mixes the samples and create labels following this rules:
    A single positive sample is said to have P(truth=+ | lab=+) =
    p_true_pos. this accounts for misslabeling

    Since a single channel detection must be a whole sample detection,
    A single channel form a positive sample is said to hold
    P(channel_truth=+ | truth=+) = p_true_pos_ch.
    this accounts for channel oriented earthquakes.

    I.e. a single channel of a single sample has p_true_pos*p_true_pos_ch
    probability of haveing a detectable event.

    Negative samples have P(truth = - | label= -) = 1.
    The underlaying asumtion is: the USGS catalog is up-tu-date and
    complete.
    """

    length = len(batch)

    p_ch = p_true_pos*p_true_pos_ch

    log.debug('mixing {} samples'.format(length))

    #TODO: Un-hardcode channel names and number
    names = ['este','norte','altura']

    cols_ix = [batch.schema.get_field_index(i) for i in names]

    if any([i==-1 for i in cols_ix]):
        raise ValueError('data channel not found')

    label_ix = batch.schema.get_field_index(label_col_name)

    if label_ix == -1:
        raise ValueError('label field not found')

    cols = [batch.column(i) for i in cols_ix]

    orderings = np.argsort(np.random.rand(3*times,len(batch))
                           ).reshape(3,-1)

    #Esto sería mucho mucho mas rapido con pandas!
    data = [ [np.array(col[i].as_py()) for i in o]
          for o,col in zip(orderings,cols)]

    #data.shape is (n_cols,batch,n_epochs)
    data_np = np.array(data,dtype=float)

    mask = data_np.sum(axis=0) # Sum columns to broadcast NaNs
    mask = np.isnan(mask) # Get rows with NaN
    mask = mask.repeat(3,axis=-1).reshape(length,61,3).transpose(2,0,1)
    #Create mask truncates EPOCHS.
    #TODO: too many hardcoded things

    data_np[mask] = np.nan

    # prepare data to batch.
    data = [list(i) for i in data_np]

    labels = batch.column(label_ix)

    # Label normalization could be a separate funcion.
    if isinstance(labels[0],pa.ListValue)\
        and len(labels[0]) == 2:
        if not set(chain(*labels)) == set([1,0]):
            ValueError('expected 1/0 lables')

        new_labels = [ 0.5 + i * 0.5 - j * 0.5 for i,j in labels.to_pylist()]

        if not set(new_labels) == set([1.,0.]):
            ValueError('expected [1,0] or [0,1] in lables')

    if isinstance(labels[0],pa.BooleanValue):
        new_labels = [int(i.as_py()) for i in labels]

    new_labels = np.array(new_labels)

    ix_e,ix_n,ix_u = orderings

    aum_y = [  1-p_ch * new_labels[ix_e], # Terremoto: tiene 20% de ser falso
               1-p_ch * new_labels[ix_n], # Terremoto: tiene 20% de ser falso
               1-p_ch * new_labels[ix_u]  # Terremoto: tiene 30% de ser falso en UP
              ]

    #aum_y = np.min([[1]*len(aum_y),aum_y],axis=0)

    p_falla = np.prod(aum_y,axis=0)
    aum_y = np.array([1-p_falla,p_falla]).transpose() # devuelve en forma de probabilidades.

    aum_y = list(aum_y)

    rb = pa.RecordBatch.from_arrays([*data,aum_y],
                                    names=[*names,label_col_name])

    return rb


@apply_args
def drop_epochs(batch,distribution=[]):
    """
    Randomly drops observations from each sample in the batch

    """
    cols = ['este','norte','altura']
    length = len(batch)

    ix_este = batch.schema.get_field_index('este')
    length_ep = len(batch[ix_este][0])

    prop = np.array(distribution) / sum(distribution)

    orden = np.argsort(np.random.rand(length))
    prop = (np.cumsum(prop) * length).astype(int)

    # Build mask
    nanes = np.zeros((length,length_ep))

    mascara = np.random.rand(length*length_ep).reshape(length,length_ep)

    for e,p in enumerate(prop):
        if e == len(prop):
            continue
        menores = mascara < float(e+1)/length_ep
        menores[orden < p] = False
        nanes += np.array([0,np.nan])[menores.astype(int)]

    # we didn't ask for more than len(prop)nanes
    nanes[np.isnan(nanes).sum(axis=1) > (len(prop) - 1)] = 0

    df = batch.to_pandas()
    # Prepare for assignment
    data = df.loc[:,cols]
    datanp = np.stack([np.stack(i) for i in data.values])
    droped = datanp.transpose(0,2,1) + nanes.reshape(*nanes.shape,1)
    newdata = droped.transpose(2,0,1)
    newdata = [list(i) for i in newdata]
    df.loc[:,cols] = newdata

    return pa.RecordBatch.from_pandas(df,preserve_index=False)

@apply_args
def random_rot_dir(batch):
    """
    Randomly horizontal axes reorientation for each sample.
    """
    cols = ['este','norte']
    length = len(batch)

    ix_este = batch.schema.get_field_index('este')
    length_ep = len(batch[ix_este][0])

    angs = np.random.rand(length)
    c = np.cos(angs*2*np.pi)
    s = np.sin(angs*2*np.pi)

    rots = np.array([[c,-s],
                     [s,c]])

    rots = rots.transpose(2,0,1)

    # Build mask

    df = batch.to_pandas()

    # Prepare data
    data = df.loc[:,cols]

    rotated_data = (rots * data.values.reshape(-1,1,2)).sum(axis=-1)
    ### Note that the maths would be the same if data where east-north
    ### pairs instead of series. numpy broadcasting works against intuiton
    ### becouse data.values is a [n,2] array whose elemts are arrays stored
    ### as <object> datatypes. then, each array are treated as a single
    ### number, resulting in the aplication of a homogeneous rotation for
    ### all times of the series.

    # assign new data
    df.loc[:,cols] = rotated_data

    #return
    return pa.RecordBatch.from_pandas(df,preserve_index=False)

#class BatchProcessing(object):
#    def __init__(self):
#        return None
#
#    def __call__(self,batch):
#        return self.process(batch)
#
#    @staticmethod
#    def label_batch(rb,label,label_col_name='etiqueta'):
#        """
#        Label or re-label a record batch.
#
#        rb : pyArrow RecordBarch to label
#        label : New label to apply on all records
#        label_col_name : Name of the label field.
#        """
#
#        many = rb.columns
#        how_many = len(rb)
#        names = rb.schema.names
#
#        if label_col_name in names:
#            to_drop = names.index(label_col_name)
#            names = names[:to_drop]+names[to_drop+1:]
#            many = many[:to_drop]+many[to_drop+1:]
#
#        rb = pa.RecordBatch.from_arrays([*many,[label]*how_many],
#             names=[*names,label_col_name])
#
#        return rb
#
#    @staticmethod
#    def scale(batch):
#        """
#        Recenter RecordBatch around its mean level.
#
#        rb : pyArrow RecordBarch to label
#        label : New label to apply on all records
#        label_col_name : Name of the label field.
#        """
#
#        df = batch.to_pandas()
#        for i in ['este','norte','altura']:
#            df.loc[:,i] = df.loc[:,i] - df[i].apply(np.nanmean)
#
#        batch = pa.RecordBatch.from_pandas(df, preserve_index=False)
#
#        return batch
#
#    @staticmethod
#    def random_mix(size,batch):
#        """
#        Random mix samples inside batch (NOT IN USE)
#
#        Resulting samples are random weighted averages of <size> samples
#        randomly taken from the batch.
#
#        size : number samples for each mix
#        batch : pyArroy Batch of samples to mix
#        """
#
#        length = len(batch)
#
#        log.debug('mixing {} samples'.format(length))
#
#        df = batch.to_pandas()
#        coords = df.loc[:,['este','norte','altura']]
#
#        mixes = [np.random.rand(length).argsort() for i in range(size) ]
#        w = np.random.rand(length*size).reshape(size,-1)
#        norm = (w**2).sum(0)**0.5
#        w = w/norm
#
#        coords_mix = sum([coords.iloc[mixes[i]].values.T * w[i]
#                          for i in range(size) ])
#
#        df.loc[:,['este','norte','altura']] = coords_mix.T
#
#        return pa.RecordBatch.from_pandas(df, preserve_index=False)
#
#    @staticmethod
#    def simple_shuffle(batch):
#        return NotImplemented
#
#    @staticmethod
#    def channel_mix_w_relabel(batch,times=1,p_true_pos=0.9,p_true_pos_ch=0.9,
#                              label_col_name='etiqueta'):
#        """
#        Given labeled batches (with labels t,f or [1,0],[0,1]
#        Mixes the samples and create labels following this rules:
#        A single positive sample is said to have P(truth=+ | lab=+) =
#        p_true_pos. this accounts for misslabeling
#
#        Since a single channel detection must be a whole sample detection,
#        A single channel form a positive sample is said to hold
#        P(channel_truth=+ | truth=+) = p_true_pos_ch.
#        this accounts for channel oriented earthquakes.
#
#        I.e. a single channel of a single sample has p_true_pos*p_true_pos_ch
#        probability of haveing a detectable event.
#
#        Negative samples have P(truth = - | label= -) = 1.
#        The underlaying asumtion is: the USGS catalog is up-tu-date and
#        complete.
#        """
#
#        length = len(batch)
#
#        p_ch = p_true_pos*p_true_pos_ch
#
#        log.debug('mixing {} samples'.format(length))
#
#        #TODO: Un-hardcode channel names and number
#        names = ['este','norte','altura']
#
#        cols_ix = [batch.schema.get_field_index(i) for i in names]
#
#        if any([i==-1 for i in cols_ix]):
#            raise ValueError('data channel not found')
#
#        label_ix = batch.schema.get_field_index(label_col_name)
#
#        if label_ix == -1:
#            raise ValueError('label field not found')
#
#        cols = [batch.column(i) for i in cols_ix]
#
#        orderings = np.argsort(np.random.rand(3*times,len(batch))
#                               ).reshape(3,-1)
#
#        #Esto sería mucho mucho mas rapido con pandas!
#        data = [ [np.array(col[i].as_py()) for i in o]
#              for o,col in zip(orderings,cols)]
#
#        #data.shape is (n_cols,batch,n_epochs)
#        data_np = np.array(data,dtype=float)
#
#        mask = data_np.sum(axis=0) # Sum columns to broadcast NaNs
#        mask = np.isnan(mask) # Get rows with NaN
#        mask = mask.repeat(3,axis=-1).reshape(length,61,3).transpose(2,0,1)
#        #Create mask truncates EPOCHS.
#        #TODO: too many hardcoded things
#
#        data_np[mask] = np.nan
#
#        # prepare data to batch.
#        data = [list(i) for i in data_np]
#
#        labels = batch.column(label_ix)
#
#        # Label normalization could be a separate funcion.
#        if isinstance(labels[0],pa.ListValue)\
#            and len(labels[0]) == 2:
#            if not set(chain(*labels)) == set([1,0]):
#                ValueError('expected 1/0 lables')
#
#            new_labels = [ 0.5 + i * 0.5 - j * 0.5 for i,j in labels.to_pylist()]
#
#            if not set(new_labels) == set([1.,0.]):
#                ValueError('expected [1,0] or [0,1] in lables')
#
#        if isinstance(labels[0],pa.BooleanValue):
#            new_labels = [int(i.as_py()) for i in labels]
#
#        new_labels = np.array(new_labels)
#
#        ix_e,ix_n,ix_u = orderings
#
#        aum_y = [  1-p_ch * new_labels[ix_e], # Terremoto: tiene 20% de ser falso
#                   1-p_ch * new_labels[ix_n], # Terremoto: tiene 20% de ser falso
#                   1-p_ch * new_labels[ix_u]  # Terremoto: tiene 30% de ser falso en UP
#                  ]
#
#        #aum_y = np.min([[1]*len(aum_y),aum_y],axis=0)
#
#        p_falla = np.prod(aum_y,axis=0)
#        aum_y = np.array([1-p_falla,p_falla]).transpose() # devuelve en forma de probabilidades.
#
#        aum_y = list(aum_y)
#
#        rb = pa.RecordBatch.from_arrays([*data,aum_y],
#                                        names=[*names,label_col_name])
#
#        return rb
#
#    @staticmethod
#    def drop_epochs(batch,distribution=[]):
#        """
#        Randomly drops observations from each sample in the batch
#
#        """
#        cols = ['este','norte','altura']
#        length = len(batch)
#
#        ix_este = batch.schema.get_field_index('este')
#        length_ep = len(batch[ix_este][0])
#
#        prop = np.array(distribution) / sum(distribution)
#
#        orden = np.argsort(np.random.rand(length))
#        prop = (np.cumsum(prop) * length).astype(int)
#
#        # Build mask
#        nanes = np.zeros((length,length_ep))
#
#        mascara = np.random.rand(length*length_ep).reshape(length,length_ep)
#
#        for e,p in enumerate(prop):
#            if e == len(prop):
#                continue
#            menores = mascara < float(e+1)/length_ep
#            menores[orden < p] = False
#            nanes += np.array([0,np.nan])[menores.astype(int)]
#
#        # we didn't ask for more than len(prop)nanes
#        nanes[np.isnan(nanes).sum(axis=1) > (len(prop) - 1)] = 0
#
#        df = batch.to_pandas()
#        # Prepare for assignment
#        data = df.loc[:,cols]
#        datanp = np.stack([np.stack(i) for i in data.values])
#        droped = datanp.transpose(0,2,1) + nanes.reshape(*nanes.shape,1)
#        newdata = droped.transpose(2,0,1)
#        newdata = [list(i) for i in newdata]
#        df.loc[:,cols] = newdata
#
#        return pa.RecordBatch.from_pandas(df,preserve_index=False)
#
#    @staticmethod
#    def random_rot_dir(batch):
#        """
#        Randomly horizontal axes reorientation for each sample.
#        """
#        cols = ['este','norte']
#        length = len(batch)
#
#        ix_este = batch.schema.get_field_index('este')
#        length_ep = len(batch[ix_este][0])
#
#        angs = np.random.rand(length)
#        c = np.cos(angs*2*np.pi)
#        s = np.sin(angs*2*np.pi)
#
#        rots = np.array([[c,-s],
#                         [s,c]])
#
#        rots = rots.transpose(2,0,1)
#
#        # Build mask
#
#        df = batch.to_pandas()
#
#        # Prepare data
#        data = df.loc[:,cols]
#
#        rotated_data = (rots * data.values.reshape(-1,1,2)).sum(axis=-1)
#        ### Note that the maths would be the same if data where east-north
#        ### pairs instead of series. numpy broadcasting works against intuiton
#        ### becouse data.values is a [n,2] array whose elemts are arrays stored
#        ### as <object> datatypes. then, each array are treated as a single
#        ### number, resulting in the aplication of a homogeneous rotation for
#        ### all times of the series.
#
#        # assign new data
#        df.loc[:,cols] = rotated_data
#
#        #return
#        return pa.RecordBatch.from_pandas(df,preserve_index=False)
#
#    @classmethod
#    def dropper(cls,**kwargs):
#        ch_mixer = cls()
#        distribution = kwargs.get('distribution',None)
#
#        if distribution is None:
#            raise ValueError('need distribution')
#
#        process = partial(cls.drop_epochs,
#                          distribution=distribution)
#
#        ch_mixer.process = process
#
#        return ch_mixer
#
#
#    @classmethod
#    def ch_mixer(cls,**kwargs):
#        ch_mixer = cls()
#        times = kwargs.get('times',1)
#        p_true_pos = kwargs.get('p_true_pos',0.8)
#        p_true_pos_ch = kwargs.get('p_true_pos_ch',0.8)
#
#        process = partial(cls.channel_mix_w_relabel,
#                     times=times,p_true_pos=p_true_pos,
#                     p_true_pos_ch= p_true_pos_ch)
#
#        ch_mixer.process = process
#
#        return ch_mixer
#
#    @classmethod
#    def scaler(cls):
#        scaler = cls()
#        scaler.process = cls.scale
#        return scaler
#
#    @classmethod
#    def random_mixer(cls,size):
#        mixer = cls()
#        mixer.process = partial(cls.random_mix, size)
#        return mixer
#
#    @classmethod
#    def random_rotator(cls):
#        mixer = cls()
#        mixer.process = cls.random_rot_dir
#        return mixer
#
#    @classmethod
#    def labeler(cls,label,label_col_name='etiqueta'):
#        labeler = cls()
#        process = partial(cls.label_batch,
#                            label=label,
#                            label_col_name=label_col_name)
#        labeler.process = process
#        return labeler
