from . import base_simple as bs
from typing import List, Any
import pyarrow as pa
import numpy as np

class FunSplitter:
    def __init__(self:"FunSplitter", source:bs.FunBuffer, splits: List[float]):
        self.source = source
        total = sum(splits)
        self.splits = [i/total for i in splits]
        self.cache = [[] for i in splits]
        self.ref_schema = None

    def __iter__(self:"FunSplitter") -> "FunSplitter":
        return self

    def __next__(self:"FunSplitter") -> Any:
        data = next(self.source)

        if not isinstance(data,pa.RecordBatch):
            raise TypeError(f'Batches of {type(piece)} not Implemented')

        # Need to save schema for reference.
        if self.ref_schema is None:
            self.ref_schema = data.schema
            self.schema_len = len(data.schema)

        try:

            if data.num_rows == 0:
                raise StopIteration()

            # Create random vector for splitting
            sorting = np.argsort(np.random.rand(data.num_rows))
        except AttributeError:
            # Sometimes prior to StopIterating
            # data can be an empty pa.String object
            # or zero length record batch, this behavior
            # Depends on the PreProcess dataflow and is hard to spot its origin by now
            # TODO: Check if this is reasonable
            # This exception should be enough to catch this behavior.

            raise StopIteration()

        # Create integer indexes
        ix = np.arange(len(sorting),dtype=np.int)
        cumsplits = np.cumsum([0,*self.splits]) * len(ix)
        ixes = [ix[ ( sorting >= i) & ( sorting < j) ] \
                for i,j in zip(cumsplits,cumsplits[1:]) ]

        # Split things up
        data_pd = data.to_pandas()
        splits = [pa.RecordBatch.from_pandas(s,preserve_index=False) \
                  if len(s) > 0 else\
                  pa.RecordBatch.from_arrays( [[]]*self.schema_len,
                                              schema=self.ref_schema    )\
                  for s in [ data_pd.iloc[ix] for ix in ixes] ]

        for lista,piece in zip(self.cache,splits):
            lista.append(piece)

        return splits

    def __getitem__(self,index):

        options = bs.FunBufferOptions(**self.source.options.__dict__)

        return bs.FunBuffer(options=options, providers=[FunPart(self,index)])

class FunPart:
    def __init__(self:"FunPart",source:FunSplitter,index:int):
        self.source = source
        self.count = 0
        if index >= len(self.source.splits):
            raise IndexError("Index out of Range")
        self.index = index

    def __iter__(self):
        return FunPart(self.source,self.index)

    def __next__(self):
        try:
            part = self.source.cache[self.index][self.count]
        except IndexError:
            part = next(self.source)[self.index]

        self.count += 1
        return part
