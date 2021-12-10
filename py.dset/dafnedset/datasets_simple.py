import os

import pyarrow as pa
import pyarrow.parquet as pq

#from .base import CachedSaver,CachedLoader,Cacher
from .base_simple import FunBuffer,FunBufferOptions
from . import fun_ops as fop
#NOPQ: from ._constants import DATOS_PATH

from typing import Dict
### Define querys

Query = Dict[str,str]

TOTAL: Query = { 'size': "SELECT max(chiid) FROM indice_t_chiid;",
          'query': """
                   SELECT max(estacion) estacion, max(iid) iid, max(ch) ch,
                   array_agg(to_date(i.yymmmdd,'YYMONDD') ORDER BY to_date(i.yymmmdd,'YYMONDD') ) tiempo,
                   array_agg(north::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) norte,
                   array_agg(east::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) este,
                   array_agg(up::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) altura
                   FROM indice_t i
                   LEFT JOIN http_tseries t USING (estacion, yymmmdd)
                   JOIN indice_t_chiid USING (iid,ch)
                   GROUP BY chiid HAVING chiid <@ int8range(%s,%s) AND count(*) = 61;
                   """,
          'columns': '\t'.join(['est','iid','ep','t','norte','este','altura'])
          }

NO_CASES: Query = { 'size': "SELECT max(chiid) FROM indice_nc_chiid;",
             'query': """
                      SELECT max(estacion) estacion, max(iid) iid, max(ch) ch,
                      array_agg(to_date(i.yymmmdd,'YYMONDD') ORDER BY to_date(i.yymmmdd,'YYMONDD') ) tiempo,
                      array_agg(north::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) norte,
                      array_agg(east::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) este,
                      array_agg(up::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) altura
                      FROM indice_nc i
                      LEFT JOIN http_tseries t USING (estacion, yymmmdd)
                      JOIN indice_nc_chiid USING (iid,ch)
                      GROUP BY chiid HAVING chiid <@ int8range(%s,%s) AND count(*) = 61;
                      """,
             'columns': '\t'.join(['est','iid','ep','t','norte','este','altura'])
          }

CASES: Query = { 'size': "SELECT max(iid) FROM indice_c;",
             'query': """
                      SELECT a.estacion, a.sid, s.time, a.tiempo, a.norte, a.este, a.altura
                      FROM
                        (SELECT max(estacion) estacion, max(sid) sid,
                                array_agg(to_date(i.yymmmdd,'YYMONDD')
                                    ORDER BY to_date(i.yymmmdd,'YYMONDD') ) tiempo,
                                array_agg(north::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) norte,
                                array_agg(east::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) este,
                                array_agg(up::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) altura
                         FROM indice_c i
                         LEFT JOIN http_tseries t USING (estacion, yymmmdd)
                         GROUP BY iid HAVING iid <@ int8range(%s,%s) ) AS a
                      JOIN usgs_sismos s ON a.sid = s.ogc_fid;
                      """,
             'columns': '\t'.join(['est','iid','ep','t','norte','este','altura'])
          }


class DefaultQuerys(FunBuffer):
    @classmethod
    def positive(cls,**kwargs):
        this = fop.read_db(CASES,**kwargs) #NOPQ
        return this

    @classmethod
    def negative(cls,**kwargs):
        this = fop.read_db(NO_CASES,**kwargs) #NOPQ
        return this

    @classmethod
    def undiscriminated(cls,**kwargs):
        return NotImplemented # loading this into the database is not
                              # enabled by default

    @classmethod
    def fake(cls,generator,size,batch):
        this = cls(providers=[synth_yielder(generator,size,batch)],
                       options=FunBufferOptions(batch_size=batch))
        return this

    def write_parquet(self,name):
        fop.write_parquet(self,name)

from typing import Callable,List,Tuple, Generator, Iterable

def synth_yielder(generator: Callable[[],Tuple[List[float],List[float],List[float]]] ,
        size: int,
        batch: int) -> Generator[pa.RecordBatch,None,None]:

    def genbatch(length: int) -> pa.RecordBatch:
        este: List[List[float]] = []
        norte: List[List[float]] = []
        altura: List[List[float]] = []
        e: Iterable[float]
        n: Iterable[float]
        u: Iterable[float]

        for i in range(length):
            e,n,u = generator()
            este.append(list(e))
            norte.append(list(n))
            altura.append(list(u))

        rbatch = pa.RecordBatch.from_arrays([este,norte,altura],
                                           names=['este','norte','altura'])
        return rbatch

    cumsize: int = 0
    while cumsize < (size - batch):
        yield genbatch(batch)
        cumsize += batch

    yield genbatch(size-cumsize)
