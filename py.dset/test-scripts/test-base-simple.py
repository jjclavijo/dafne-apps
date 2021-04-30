import numpy as np
from dafnedset import base_simple as bs

opts = bs.FunBufferOptions(batch_size=25,niter=1)
buf = bs.FunBuffer(options=opts, providers=[[np.ones(25)*i for i in range(5)]])

a = bs.fill_streams(buf)
buf.buffer = []
b = bs.advance(buf)
c = buf.fill_buffer()
buf.fill_streams()
buf.fill_buffer()
b = bs.advance(buf)
hist
hist -f test-base-simple.py
