from functools import singledispatch, singledispatchmethod
from itertools import repeat
from collections.abc import Iterable

from typing import (
    Union,
    List,
    Iterable,
    Set,
    Optional,
    Tuple,
    Callable,
    NoReturn,
    TypeVar,
    NewType,
    Generic,
    Sequence,
    Any,
    overload,
)

import pyarrow as pa
import pandas as pd
import numpy as np


Feedable = TypeVar("Feedable", pa.RecordBatch, pd.DataFrame, pd.Series, np.ndarray)
T = TypeVar("T")
Return = Callable[[], T]
MayReturn = Union[Callable[[], T], Callable[[], NoReturn]]


def pa_common_cols(batches: List[pa.RecordBatch]) -> Set[str]:
    if len(batches) > 1:
        return pa_common_cols(batches[:1]) & pa_common_cols(batches[1:])
    else:
        return set(batches[0].schema.names)


def pa_keep_cols(
    cols: Iterable[str], batches: List[pa.RecordBatch]
) -> List[pa.RecordBatch]:
    if batches:
        b = batches[0]
        return [
            pa.RecordBatch.from_pandas(b.to_pandas().loc[:, cols]),
            *pa_keep_cols(cols, batches[1:]),
        ]
    else:
        return []


def pa_cols_differs(batches: List[pa.RecordBatch]) -> bool:
    if len(batches) > 1:
        if set(batches[0].schema.names) - set(batches[1].schema.names) or set(
            batches[1].schema.names
        ) - set(batches[0].schema.names):
            return True
        else:
            return pa_cols_differs(batches[1:])
    else:
        return False


@singledispatch
def field_normalize(
    *args: Feedable, columns: Optional[List["str"]] = None
) -> List[Feedable]:
    raise TypeError("Type {} batches not implemented".format(type(args[0])))
    return []


@field_normalize.register(pa.RecordBatch)  # type: ignore[no-redef]
def _(
    *args: pa.RecordBatch, columns: Optional[List["str"]] = None
) -> List[pa.RecordBatch]:
    pcolumns = pa_common_cols(args)

    if columns is None:
        columns = list(pcolumns)

    if pcolumns == set(columns):
        if not pa_cols_differs(args):
            return args
    elif not columns <= pcolumns:
        raise ValueError("Missing columns {}".format(columns - pcolumns))

    return pa_keep_cols(columns, args)


@field_normalize.register(np.ndarray)  # type: ignore[no-redef]
def _(*args: np.ndarray, columns: Optional[List["str"]] = None) -> List[np.ndarray]:

    ndims = [i.ndim for i in args]
    if len(set(ndims)) != 1:
        raise ValueError("ndim must be the same for all batches")

    ndim = ndims[0]

    if ndim > 1:
        ncols = [i.shape[1] for i in args]

        if len(set(ncols)) != 1:
            raise ValueError("all batches must have the same number of columns")

    return args


@singledispatch
def join_batches(
    first: Feedable, *rest: List[Feedable], until: Optional[int] = None
) -> List[Feedable]:
    raise TypeError("Type {} batches not implemented".format(type(first)))
    return []


@join_batches.register(pa.RecordBatch)  # type: ignore[no-redef]
def _(
    first: pa.RecordBatch, *rest: pa.RecordBatch, until: Optional[int] = None
) -> List[pa.RecordBatch]:
    to_join: List[pa.RecordBatch] = [first]
    cum_len: int = len(first)

    rest = list(rest)

    while ((until is None) or (cum_len < until)) and (rest != []):
        to_join.append(rest.pop())
        cum_len += len(to_join[-1])

    rebatched = pa.Table.from_batches(to_join).combine_chunks().to_batches()

    return [*rebatched, *rest]


@join_batches.register(np.ndarray)  # type: ignore[no-redef]
def _(
    first: np.ndarray, *rest: np.ndarray, until: Optional[int] = None
) -> List[np.ndarray]:
    to_join: List[np.ndarray] = [first]
    cum_len: int

    rest = list(rest)

    ndim = first.ndim

    if ndim == 1:
        cum_len = len(first)
        while ((until is None) or (cum_len < until)) and (rest != []):

            to_join.append(rest.pop())

            cum_len += len(to_join[-1])

        rebatched = np.concatenate(to_join)

    else:
        cum_len = first.shape[1]

        while ((until is None) or (cum_len < until)) and (rest != []):

            to_join.append(rest.pop())

            cum_len += to_join[-1].shape[1]

        rebatched = np.stack(to_join, axis=0)

    return [rebatched, *rest]


@singledispatch
def get_batch(
    first: Feedable, *rest: List[Feedable], length: Optional[int] = None
) -> Tuple[Feedable, List[Feedable]]:
    raise TypeError("Type {} batches not implemented".format(type(first)))


@get_batch.register(pa.RecordBatch)  # type: ignore[no-redef]
def _(first, *rest, length: Optional[int] = None):

    if length is None:
        return first, list(rest)

    first_a, *first_b = (first.slice(0, length), first.slice(length))

    if sum([len(i) for i in first_b]) == 0:
        first_b = []

    return first_a, [*first_b, *rest]


@get_batch.register(np.ndarray)  # type: ignore[no-redef]
def _(first, *rest, length: Optional[int] = None):

    if length is None:
        return first, list(rest)

    first_a, *first_b = (first[0:length], first[length:])

    if sum([len(i) for i in first_b]) == 0:
        first_b = []

    return first_a, [*first_b, *rest]


class FunBufferOptions:
    force: bool = True
    max_length: Optional[bool] = None
    niter: int = 1
    batch_size: Optional[int] = None
    columns: Optional[List[str]] = None
    cache: bool = True

    def __init__(self, **kwargs) -> None:
        """
        init just updates class __dict__, making class initialization
        similar to a struct type or something.
        """
        for i, j in kwargs.items():
            if not i in self.__annotations__:
                raise ValueError("Unknown keyboard parameter: {}".format(i))
        self.__dict__.update(kwargs)
        pass


class FunBuffer(Generic[Feedable]):
    buffer: Optional[List[Feedable]] = None
    cache: Optional[List[Feedable]] = None
    streams: Optional[List[Return[Feedable]]] = None
    stream_last_valid: List[bool]
    iteration: List[int]
    providers: Optional[List[Iterable[Feedable]]] = None
    options: FunBufferOptions
    preprocess: Callable[[Feedable], Feedable]
    exhausted: bool = False

    def __init__(self, **kwargs) -> None:
        """
        Basic init stores all kw argumens into object dict (like a struct)
        Then, check mandatory conditions and set needed parameters to defaults
        """
        size: Optional[int]

        for i, j in kwargs.items():
            if not i in self.__annotations__:
                raise ValueError("Unknown keyboard parameter: {}".format(i))
        self.__dict__.update(kwargs)

        if "providers" in self.__dict__ and not self.providers is None:
            size = len(self.providers)
        elif "streams" in self.__dict__ and not self.streams is None:
            size = len(self.streams)
        else:
            size = None

        if size is not None:
            if not "streams" in self.__dict__ or self.streams is None:
                # self.streams = None
                pass
            elif len(self.streams) != size:
                raise ValueError("streams of unmatching length")

            if not "iteration" in self.__dict__:
                self.iteration = [0] * size
            else:
                if len(self.iteration) != size:
                    raise ValueError("iteration counter of unmatching length")
            if not "stream_last_valid" in self.__dict__:
                self.stream_last_valid = [False] * size
            else:
                if len(self.stream_last_valid) != size:
                    raise ValueError("stream_last_valid of unmatching length")

        pass

    def get_new_next(self) -> Callable[[], Feedable]:
        pass

    def fill_streams(self: "FunBuffer[Feedable]") -> None:

        new_streams = fill_streams(self)
        if new_streams == []:
            return None

        streams, iters = combine_streams(self.streams, new_streams)

        self.streams = streams
        self.iteration = [i + j for i, j in zip(self.iteration, iters)]


    def fill_buffer(self: "FunBuffer[Feedable]") -> None:
        """
        Try to fill Buffer:

        No Streams -> StopIteration
        All Streams StopIterated -> StopIteration
        """
        if self.streams is None:
            raise StopIteration
        for ix, f in enumerate(self.streams):
            try:
                self.stream_last_valid[ix] = True  # TODO: decorate streams
                piece = f()
                # TODO: validate
                if self.buffer is None:
                    self.buffer = [piece]
                else:
                    self.buffer.append(piece)
            except StopIteration:
                self.stream_last_valid[ix] = False  # TODO: decorate streams

        if not any(self.stream_last_valid):
            raise StopIteration

        return

    def advance(self: "FunBuffer[Feedable]", **kwargs) -> Optional[Feedable]:
        """
        Trys to draw a new batch

        Buffer empty -> try to fill it.
        Fails? -> try to fill streams then buffer
        Fails? -> Set exhaust
        """

        size = kwargs.get("size", self.options.batch_size)
        batch, buff = advance(self, size)
        self.buffer = buff

        if batch is None:  # If buffer was not enough
            try:
                self.fill_buffer()

            except StopIteration:
                try:
                    self.fill_streams()
                    self.fill_buffer()

                except StopIteration:
                    self.exhausted = True

        elif self.options.cache:
            if self.cache is None:
                self.cache = [batch]
            else:
                self.cache.append(batch)

        return batch

    def __iter__(self: "FunBuffer[Feedable]") -> "FunBuffer[Feedable]":
        return self

    def __next__(self: "FunBuffer[Feedable]") -> "Feedable":
        was_exhausted = self.exhausted

        batch = self.advance()

        if batch is None:  # if advance returned nothing
            if was_exhausted:  # and streams were already exhausted
                # if self.exhausted changed to True we must not Stop,
                # there can be a remainder in the buffer
                raise StopIteration  # means there is nothing to return
            else:  # If streams were not exhausted we must try again.
                return self.__next__()

        return batch  #

    def __mul__(self: "FunBuffer[Feedable]", a: int) -> "FunBuffer[Feedable]":
        if not isinstance(a, int):
            return NotImplemented
        else:
            options = FunBufferOptions(**self.options.__dict__)
            options.niter *= a
            return FunBuffer(options=options, providers=[*self.providers])

    def __div__(self: "FunBuffer[Feedable]", a: int) -> "FunBuffer[Feedable]":
        if not isinstance(a, int):
            return NotImplemented
        else:
            options = FunBufferOptions(**self.options.__dict__)
            options.batch_size = a
            return FunBuffer(options=options, providers=[*self.providers])

    def __add__(
        self: "FunBuffer[Feedable]", other: "FunBuffer[Feedable]"
    ) -> "FunBuffer[Feedable]":
        if not isinstance(other, self.__class__):
            return NotImplemented

        if self.providers is None or other.providers is None:
            return NotImplemented
        elif not isinstance(self.providers[-1], other.providers[-1].__class__):
            return NotImplemented
        else:
            options = FunBufferOptions(**self.options.__dict__)
            return FunBuffer(
                options=options, providers=[*self.providers, *other.providers]
            )

    def map(
        self: "FunBuffer[Feedable]", transformation: "Callable[[Feedable],Feedable]"
    ) -> "FunBuffer[Feedable]":
        new_providers = [map(transformation,i) for i in self.providers]
        options = FunBufferOptions(**self.options.__dict__)

        return FunBuffer(
                options=options, providers=new_providers
                )

    # def __>>__ add preprocessing


def advance(
    buffer: FunBuffer[Feedable], size: Optional[int] = None, cache: bool = True
) -> Tuple[Optional[Feedable], List[Feedable]]:

    buf_len: int = (
        sum([len(i) for i in buffer.buffer]) if not buffer.buffer is None else 0
    )

    # Si el buffer está vacio:
    if buf_len == 0:
        return None, []

    assert buffer.buffer is not None

    new = buffer.buffer.copy()  # not optimal.
    new = field_normalize(*new, columns=buffer.options.columns)
    new = join_batches(*new, until=size)

    if not size is None and buf_len < size and not buffer.exhausted:
        return None, new

    batch, new = get_batch(*new, length=size)

    return batch, new


def fill_streams(buffer: FunBuffer[Feedable]) -> List[Optional[Callable[[], Feedable]]]:

    if buffer.providers is None:
        if buffer.streams is None:
            return []
        else:
            return [None] * len(buffer.streams)

    can_reset: List[bool]
    can_reset = [i < buffer.options.niter for i in buffer.iteration]

    last_was_valid = buffer.stream_last_valid

    size = len(buffer.providers)
    providers = buffer.providers

    status: List[bool] = [i and not j for i, j in zip(can_reset, last_was_valid)]

    new_streams: List[Optional[Callable[[], Feedable]]] = [None] * size

    for n, (st, source) in enumerate(zip(status, providers)):
        if st:
            if source is None:
                continue
            if isinstance(source, FunBuffer):
                new_streams[n] = source.get_new_next()
            else:
                new_streams[n] = source.__iter__().__next__

    return new_streams


def combine_streams(
    *stlist: Union[
        Optional[List[Optional[Return[Feedable]]]], Optional[List[Return[Feedable]]]
    ]
) -> Tuple[List[Return[Feedable]], List[int]]:

    tmp = list(stlist)

    lens = list(set(map(lambda x: 0 if x is None else len(x), tmp)).difference([0]))

    if len(lens) == 0:
        raise ValueError("attemping to combine all empty streams")
    elif len(lens) > 1:
        raise ValueError("streams lengths differs")

    size = lens[0]

    clean: List[List[Optional[Return[Feedable]]]] = [
        i for i in tmp if not i is None
    ]  # type: ignore[misc]

    if clean == []:
        raise ValueError("Combining empty Streams")

    to_ret: List[Return[Feedable]] = []

    for g in zip(*clean):
        to_add: Optional[Return[Feedable]] = None
        for i in g:
            if i is not None:
                to_add = i
        if to_add is None:
            raise ValueError("Missing Slots in stream list")
        else:
            to_ret.append(to_add)

    if stlist[0] is None:
        iters = [1] * size
    else:
        iters = [0 if i is j else 1 for i, j in zip(to_ret, stlist[0])]

    return to_ret, iters
