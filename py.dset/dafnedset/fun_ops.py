
#Core Imports
from functools import partial
import os.path
import logging

#External Imports
import pyarrow as pa
import pyarrow.parquet as pq

#Local imports
from .helpers import FuncGen
from . import base_simple as bs
from . import split_simple as ss
from ._db_connect import default_conn
from ._constants import QUERY_SIZE,MAX_BATCHSIZE


log = logging.getLogger(__name__)

def yield_batched_query(query, query_size=QUERY_SIZE,
                        conn=None, label=None,**kwargs):
    """
    given a Conection and a query (dictionary with 'size' and 'query'),
    connects to db and provides a generator of pa.RecordBatch es.
    with the data
    """
    if conn is None:
        conn = default_conn()
        to_close=True
    else:
        to_close=False

    with conn.cursor() as curs:
        curs.execute(query['size'])
        q1 = curs.fetchall()
        maxii = q1[0][0]
        log.info('query going to {}'.format(maxii))
        top = 1

        while top < maxii:
            top = top + query_size
            curs.execute(query['query'],vars=[top - query_size,top])

            many = curs.fetchall()
            how_many = len(many)
            if how_many > 0:
                rb = pa.RecordBatch.from_arrays([*zip(*many)],
                     names=query['columns'].split('\t'))
                #TODO: Names as constant
                yield rb

    if to_close:
        conn.close()

def read_db(query,**kwargs):

    fbo_kwargs = {'batch_size':100,'niter':1}
    fbo_kwargs.update({k:v for k,v in kwargs.items() if k in bs.FunBufferOptions.__annotations__})

    opts = bs.FunBufferOptions(**fbo_kwargs)

    gen = partial(yield_batched_query,
                            query,**kwargs)

    buf = bs.FunBuffer(options=opts, providers=[FuncGen(gen)])

    #TODO: turn yield_batched_query into a proper generator
    return buf

def read_parquet(parquet_path,**kwargs):

    try:
        open(parquet_path,'r').close()
    except OSError:
        raise

    def gen():
        parq = pq.ParquetFile(parquet_path)
        for i in range(parq.num_row_groups):
            yield parq.read_row_group(i).to_batches()[0]

    fbo_kwargs = {'batch_size':100,'niter':1}
    fbo_kwargs.update({k:v for k,v in kwargs.items() if k in bs.FunBufferOptions.__annotations__})

    opts = bs.FunBufferOptions(**fbo_kwargs)

    buf = bs.FunBuffer(options=opts, providers=[FuncGen(gen)])

    return buf

def write_parquet(fb,parquet_path):
    gen = fb
    init = None

    for batch in gen:
        if init is None:
            init = True
            parq = pq.ParquetWriter(parquet_path,batch.schema)

        parq.write_table(pa.Table.\
                         from_batches([batch])
                         )
    parq.close()

def split(fb,splits):
    #Simple wrapper to be more "functinal like"
    # maybe we should do some checks here.
    return ss.FunSplitter(fb,splits)

def map(fb,func):
    #Simple wrapper to be more "functinal like"
    # maybe we should do some checks here.
    return fb.map(func)
