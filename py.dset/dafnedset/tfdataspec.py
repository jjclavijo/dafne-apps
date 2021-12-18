import pyarrow as pa
from tfx_bsl.public import tfxio
from tensorflow_metadata.proto.v0.schema_pb2 import TensorRepresentation, FixedShape

# Para test-ear
# import pyarrow as pa
# import numpy as np
# import pandas as pd
# pd.DataFrame({'norte':[np.random.rand(61) for i in range(9)]})
# df = pd.DataFrame({'norte':[np.random.rand(61) for i in range(9)]})
# pa.RecordBatch.from_pandas(df)
# rb = pa.RecordBatch.from_pandas(df)

dim = FixedShape(dim=[FixedShape.Dim(size=61)])
dim_l = FixedShape(dim=[FixedShape.Dim(size=2)])

norte = TensorRepresentation.DenseTensor(column_name='norte',shape=dim)
este = TensorRepresentation.DenseTensor(column_name='este',shape=dim)
altura = TensorRepresentation.DenseTensor(column_name='altura',shape=dim)
etiqueta = TensorRepresentation.DenseTensor(column_name='etiqueta',shape=dim_l)

rep_etiquetadas =  {'norte':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='norte',shape=dim)),
                    'este':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='este',shape=dim)),
                    'altura':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='altura',shape=dim)),
                    'etiqueta':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='etiqueta',shape=dim_l))
                    }

rep_crudas =       {'norte':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='norte',shape=dim)),
                    'este':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='este',shape=dim)),
                    'altura':TensorRepresentation(dense_tensor =
                TensorRepresentation.DenseTensor(column_name='altura',shape=dim))
                    }

def adapter(arrowSchema: pa.Schema) -> tfxio.TensorAdapter:
    if 'etiqueta' in arrowSchema.names:
        ta = tfxio.TensorAdapterConfig(arrowSchema,rep_etiquetadas)
    else:
        ta = tfxio.TensorAdapterConfig(arrowSchema,rep_crudas)

    return tfxio.TensorAdapter(ta)
