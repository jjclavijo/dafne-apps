import numpy as np
from dafnedset import base_simple as bs
opts = bs.FunBufferOptions(batch_size=25,niter=1)
buf = bs.FunBuffer(options=opts, providers=[[np.ones(25)*i for i in range(5)]])
buf.stream_last_valid
bs.fill_streams(buf)
bs.advance(buf)
buf.buffer = []
bs.advance(buf)
buf.fill_buffer()
buf.fill_buffer()
buf.advance?
buf.advance()
ls
cd test-scripts/
hist
hist -f test-base-simple.py
