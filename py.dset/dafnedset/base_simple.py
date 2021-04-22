from functools import singledispatch, singledispatchmethod
from collections.abc import Iterable

Feedable = Union[pa.RecordBatch,pd.DataFrame,pd.Series,np.ndArray]


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

class Buffer:

    force: bool
    callargs: Tuple[List[Any],Dict[str,Any]]
    batch_size: Optional[int]
    accum: List[Feedable]
    columns_must: List[str]
    feed_parms: Optional[Dict[str,Any]]
    feeder: Optional[Union[Iterable[Feedable],List[Iterable[Feedable]]]]
    length: Union[PromisedInt,int]
    max_length: Optional[int]
    count: int
    is_multy: bool

    @singledispatchmethod
    def __init__(self,*args,**kwargs):
        """
        batch_size: int or PromisedInt. cannot yield until set.
        force: yield all when asked for next if data length is
               below batch_size

        kwargs
        """
        try:
            bs,*_ = args
            if isinstance(bs,int) or isinstance(bs,PromisedInt):
                self.batch_size = bs
            elif: bs is None:
                self.batch_size = bs
            else:
                raise TypeError('batch size must be integer')
        except ValueError:
            self.batch_size = None


        self.force = kwargs.get('force',True)
        self.callargs = (args,kwargs)
        self.accum = []
        self.is_multy = False

    @__init__.register
    #def __init__(self,feeder,batch_size,*args,force=False,
    #             repeat=False,max_length=None,**kwargs):
    def __init__(self,feeder: Iterable,*args,**kwargs):
        """
        feeder: any iterable.
        batch_size: int or PromisedInt
        max_length: int or PromisedInt limit before reset feeder.
        """
        self.feeder = feeder

        if hasattr(feeder,'length'):
            self.length = feeder.length
        else:
            self.length = PromisedInt()

        if args == []: #if no Batch Size, lenght = Batch Size
            args = [self.length]

        self.__init__(*args,**kwargs)

        self.repeat = kwargs.get('repeat',False)
        self.max_length = kwargs.get('max_length',None)
        self.count = 0

        self.callargs = ([feeder,*args],kwargs)

    @__init__.register
    def __init__(self,feeder: list,*args,**kwargs):
        self.callargs = ([feeder,*args],kwargs)

        self.feeder = feeder

        try:
            batch_size = sum([i.batch_size for i in feeder])
        except AttributeError:
            batch_size = None #???

        has_len = [hasattr(f,'length') for f in feeder]

        if all(has_len):
            self.length = sum([f.length for f in feeder])
        else:
            self.length = PromisedInt()

        if args == []: #if no Batch Size, lenght = Batch Size
            if batch_size = None:
                args = [self.length]
            else:
                args = [batch_size]

        self.__init__(*args,**kwargs)

        self.repeat = kwargs.get('repeat',False)
        self.max_length = kwargs.get('max_length',None)
        self.count = 0
        self.is_multy = True

