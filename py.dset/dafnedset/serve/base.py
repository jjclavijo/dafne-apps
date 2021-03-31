#Core Imports
import logging
from functools import partial
from io import BytesIO

#External imports
import numpy as np
import pyarrow as pa
import socket

log = logging.getLogger(__name__)

BATCH_SIZE=500

class DataServer(object):
    """
    A DataServer is an object with a .data property which is a generator
    and a serve_data method, which asyncronically writes pyArrow RecordBatches
    (from the generator) as a BytesIO stream into asyncio writer stream (passed as argument),
    using pyArrow.RecordBatchStreamWriter.
    """
    def __init__(self):
        """
        Avoid direct construction
        """
        return None

    @classmethod
    def from_batch_generator(cls, generator, **kwargs):
        """
        Create instance from a generator returning batches.
        """
        server = cls(**kwargs)
        server.data = generator
        return server

    @classmethod
    def from_pandas(cls, dataset, **kwargs):
        """
        Create instance from pandas DataFrame.
        """
        return NotImplemented
        # This is deprecated

    async def serve_data(self,write_stream):
      # Create a BytesStream for buffering
      bytes_buffer = BytesIO()

      writer = None

      try:
          for batch in self.data:
            # Initialize the pyarrow writer on first batch
            if writer is None:
              writer = pa.RecordBatchStreamWriter(bytes_buffer, batch.schema)

            # Write the batch to the client stream
            # First Write into buffer
            writer.write_batch(batch)

            # Seek to first bit of buffer
            bytes_buffer.seek(0)
            # Write into socket
            write_stream.write(bytes_buffer.read())
            # Rewind and drop old bytes. (flush)
            bytes_buffer.seek(0)
            bytes_buffer.truncate()

            log.info('Written Batch')

            # wait until socket is writeable
            await write_stream.drain()

      # Try to catch connection errors
      except (socket.error, BrokenPipeError) as e:
        log.info("{}, Dataset Connection Closed".format(e))

      # Allways Cleanup client connection
      finally:
        if writer is not None:
          writer.close()

        bytes_buffer.close()
        write_stream.close()
        await write_stream.wait_closed()

        log.info('Connection Closed')
