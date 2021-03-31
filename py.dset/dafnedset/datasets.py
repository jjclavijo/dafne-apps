import os

from .base import CachedSaver,CachedLoader
#NOPQ: from ._constants import DATOS_PATH

class DefaultQuerys(CachedSaver,CachedLoader):
    Q_T = "SELECT max(chiid) FROM indice_t_chiid;"

    QUERY_T = """
    SELECT max(estacion) estacion, max(iid) iid, max(ch) ch,
    array_agg(to_date(i.yymmmdd,'YYMONDD') ORDER BY to_date(i.yymmmdd,'YYMONDD') ) tiempo,
    array_agg(north::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) norte,
    array_agg(east::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) este,
    array_agg(up::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) altura
    FROM indice_t i
    LEFT JOIN http_tseries t USING (estacion, yymmmdd)
    JOIN indice_t_chiid USING (iid,ch)
    GROUP BY chiid HAVING chiid <@ int8range(%s,%s) AND count(*) = 61;
    """

    Q_NC = "SELECT max(chiid) FROM indice_nc_chiid;"

    QUERY_NC = """
    SELECT max(estacion) estacion, max(iid) iid, max(ch) ch,
    array_agg(to_date(i.yymmmdd,'YYMONDD') ORDER BY to_date(i.yymmmdd,'YYMONDD') ) tiempo,
    array_agg(north::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) norte,
    array_agg(east::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) este,
    array_agg(up::real ORDER BY to_date(i.yymmmdd,'YYMONDD') ) altura
    FROM indice_nc i
    LEFT JOIN http_tseries t USING (estacion, yymmmdd)
    JOIN indice_nc_chiid USING (iid,ch)
    GROUP BY chiid HAVING chiid <@ int8range(%s,%s) AND count(*) = 61;
    """

    Q_C = "SELECT max(iid) FROM indice_c;"

    QUERY_C = """
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
    """

    @classmethod
    def positive(cls,**kwargs):
        this = super().read_db(cls.Q_C,cls.QUERY_C,**kwargs) #NOPQ
        return this

    @classmethod
    def negative(cls,**kwargs):
        this = super().read_db(cls.Q_NC,cls.QUERY_NC,**kwargs) #NOPQ
        return this

    @classmethod
    def undiscriminated(cls,**kwargs):
        return NotImplemented # loading this into the database is not
                              # enabled by default

    def write_parquet(self):
        pass
