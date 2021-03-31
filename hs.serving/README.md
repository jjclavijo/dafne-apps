# App para trabajar con datos de eventos + tf-serving (tag:serving)

Conecta con la base de datos que tiene la disponibilidad en tiempos de los
datos + los tiempos de los terremotos (docker.io/jjclavijo/dafne-db:times-latest)

Y con los modelos que estén acordemente preparados en tf-serving (ver compose)

API:

```
type API = "get_event" :> Capture "event" Integer :> Capture "dist" Double :> Capture "days" Integer :> Get '[JSON] T.EvSeries
      :<|> "predict" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe EvPreds)
      :<|> "predictSteps" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe EvSteps)
      :<|> "map" :> Capture "event" Integer :> Capture "dist" Double :> Get '[JSON] (Maybe P.EvtPtsGeo)
```

Sirve principalmente para ponerlo en producción y poder masomenos visualizar
sin depender de python y todo la maquinaria.

TODO: Habría que pulir los mapas.
