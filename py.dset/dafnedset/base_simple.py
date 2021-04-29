from functools import singledispatch, singledispatchmethod
from collections.abc import Iterable

from typing import Union,List,Iterable,Set,Optional,Tuple,Callable,NoReturn

import pyarrow as pa
import pandas as pd
import numpy as np

Feedable = Union[pa.RecordBatch,pd.DataFrame,pd.Series,np.ndarray]

def pa_common_cols(batches: List[pa.RecordBatch]) -> Set[str]:
    if len(batches) > 1:
        return pa_common_cols(batches[:1]) & pa_common_cols(batches[1:])
    else:
        return set(batches[0].schema.names)

def pa_keep_cols(cols: Iterable[str],batches: List[pa.RecordBatch]) -> List[pa.RecordBatch]:
    if batches:
        b = batches[0]
        return [ pa.RecordBatch.from_pandas(b.to_pandas().loc[:,cols]),
                 *pa_keep_cols(cols,batches[1:])]
    else:
        return []

def pa_cols_differs(batches: List[pa.RecordBatch]) -> bool:
    if len(batches) > 1:
        if set(batches[0].schema.names) - set(batches[1].schema.names) or\
           set(batches[1].schema.names) - set(batches[0].schema.names):
            return True
        else:
            return pa_check_cols(batches[1:])
    else:
        return False

@singledispatch
def field_normalize(*args: Feedable,
        columns: Optional[List['str']] = None) -> List[Feedable]:
    raise TypeError("Type {} batches not implemented".\
                    format(type(args[0])))
    return []

@field_normalize.register(pa.RecordBatch)
def _(*args: pa.RecordBatch,
        columns: Optional[List['str']] = None) -> List[pa.RecordBatch]:
    pcolumns = pa_common_cols(self.accum)

    if columns is None:
        columns = list(pcolumns)

    if pcolumns == set(columns):
        if not pa_cols_differs(args):
            return args
    elif not columns <= pcolumns:
        raise ValueError("Missing columns {}".format(columns - pcolumns))

    return pa_keep_cols(columns,args)


@singledispatch
def join_batches(first: Feedable, *rest: List[Feedable],
                 until: Optional[int] = None) -> List[Feedable]:
    raise TypeError("Type {} batches not implemented".\
                    format(type(first)))
    return []

@join_batches.register(pa.RecordBatch)
def _(first: pa.RecordBatch, *rest: List[pa.RecordBatch],
        until: Optional[int] = None) -> List[pa.RecordBatch]:
    to_join: List[pa.RecordBatch] = [first]
    cum_len: int = len(first)

    while ((until is None) or (cum_len < until))\
            and (rest != []):

        to_join.append(rest.pop())

    rebatched = pa.Table.\
                from_batches(to_join).\
                combine_chunks().\
                to_batches()

    return [rebatched,*rest]

@singledispatch
def get_batch(first: Feedable, *rest: List[Feedable],
                 length: Optional[int] = None) -> List[Feedable]:
    raise TypeError("Type {} batches not implemented".\
                    format(type(first)))
    return []

@get_batch.register(pa.RecordBatch)
def _(first: pa.RecordBatch, *rest: List[pa.RecordBatch],
        length: Optional[int] = None) -> Tuple[pa.RecordBatch, List[pa.RecordBatch]]:

    if length is None:
        return first,rest

    first_a,*first_b = (first.slice(0,length),
                     first.slice(length))

    if sum([len(i) for i in first_b]) == 0:
        first_b = []

    return first_a, [*first_b, *rest]

class FunBufferOptions:
    force: bool = True
    max_length: Optional[bool] = None
    niter: Optional[int] = None
    batch_size: Optional[int] = None
    columns: Optional[List[str]] = None
    cache: bool = True
    def __init__(self,**kwargs) -> None:
        """
        init just updates class __dict__, making class initialization
        similar to a struct type or something.
        """
        for i,j in kwargs.items():
            if not i in self.__annotations__:
                raise ValueError("Unknown keyboard parameter: {}".format(i))
        self.__dict__.update(kwargs)
        pass

def stop() -> NoReturn:
    raise StopIteration

class FunBuffer:
    buffer: List[Feedable]
    cache: List[Feedable]
    streams: List[Callable[[],Feedable]]
    stream_last_valid: List[bool]
    iteration: List[int]
    providers: List[Optional[Iterable[Feedable]]]
    options: FunBufferOptions
    preprocess: Callable[[pa.RecordBatch],pa.RecordBatch]
    exhausted: bool
    def __init__(self,**kwargs) -> None:
        """
        Basic init stores all kw argumens into object dict (like a struct)
        Then, check mandatory conditions and set needed parameters to defaults
        """
        for i,j in kwargs.items():
            if not i in self.__annotations__:
                raise ValueError("Unknown keyboard parameter: {}".format(i))
        self.__dict__.update(kwargs)

        if not "providers" in self.__dict__: self.streams = []

        size = len(self.providers)

        if not "streams" in self.__dict__: self.streams = [stop] * size
        else:
            if len(self.streams) != size:
                raise ValueError("streams of unmatching length")
        if not "iteration" in self.__dict__: self.iteration = [0] * size
        else:
            if len(self.iteration) != size:
                raise ValueError("iteration counter of unmatching length")
        if not "stream_last_valid" in self.__dict__:
            self.stream_last_valid = [False] * size
        else:
            if len(self.stream_last_valid) != size:
                raise ValueError("stream_last_valid of unmatching length")

        pass

    def get_new_next(self) -> Callable[[],pa.RecordBatch]:
        pass

    def fill_streams(self) -> None:
        new_streams = fill_streams(self)
        for n,stream in enumerate(new_streams):
            if stream is None:
                continue
            else:
                self.streams[n] = stream
                self.iteration[n] += 1

    def fill_buffer(self) -> int:
        stopcount = 0
        for ix,f in enumerate(self.streams):
            try:
                piece = f()
                #TODO: validate
                self.buffer.append(piece)
            except StopIteration:
                stopcount += 1
                self.stream_last_valid[ix] = False #TODO: decorate streams
                                                   # also check types
        return stopcount #TODO: StopCount is redundant

    def advance(self, **kwargs) -> Optional[pa.RecordBatch]:
        size = kwargs.get('size',self.options.batch_size)

        batch, buff = advance(self,size)

        if batch is None: # If buffer was not enough
            stopcount = self.fill_buffer()
            # si no había nada en el buffer y se terminó la iteración
            if buff == [] and stopcount == len(self.streams):
                self.fill_streams()
                stopcount = self.fill_buffer()

                if stopcount == len(self.streams):
                    self.exhausted = True

            # Try Again, this time if buffer was not enough but
            # providers were exhausted, will return the last remaining
            # elements
            batch, buff = advance(self,size)

        self.buffer = buff

        if self.options.cache:
            self.cache.append(batch)

        return batch


def advance(buffer: FunBuffer, size: Optional[int] = None, cache: bool= True)\
    -> Tuple[Optional[pa.RecordBatch], List[pa.RecordBatch]]:

    buf_len: int = sum([len(i) for i in buffer.buffer])

    # Si el buffer está vacio:
    if buf_len == 0:
        return None,[]

    if buf_len < size and not buffer.exhausted:
        return None,buffer.buffer

    new = buffer.buffer.copy() #not optimal.
    new = field_normalize(*new, columns=buffer.options.columns)
    new = join_batches(*new, until=size)

    batch, new = get_batch(*new,length=size)

    return batch, new

def fill_streams(buffer: FunBuffer) -> List[Optional[Callable[[],pa.RecordBatch]]]:

    can_reset: bool = [i < buffer.options.niter for i in buffer.iteration]
    last_was_valid = buffer.stream_last_valid
    size = len(buffer.streams)
    providers = buffer.providers

    status: bool = [i and not j for i,j in zip(can_reset,last_was_valid)]

    new_streams = [None] * size

    for n, (status, source) in enumerate(zip(status,providers)):
        if status:
            if isinstance(source,FunBuffer):
                new_streams[n] = source.get_new_next()
            else:
                new_streams[n] = source.__iter__().__next__

    return new_streams
