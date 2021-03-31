from pydafne.data.presets.pos10 import data as datapos
from pydafne.data.transformations import BatchProcessing
import numpy as np

rot = BatchProcessing.random_rot_dir
datarot = datapos.preprocess(mix=rot)
d = next(iter(datarot))
inorte = d[4][0]
ieste = d[5][0]
mod = np.array(ieste.as_py())**2+np.array(inorte.as_py())**2

dp = next(iter(datapos))
este = dp[5][0]
norte = dp[4][0]
modo = np.array(este.as_py())**2+np.array(norte.as_py())**2

ienorm = np.array(ieste.as_py())/mod**0.5
innorm = np.array(inorte.as_py())/mod**0.5
enorm = np.array(este.as_py())/modo**0.5
nnorm = np.array(norte.as_py())/modo**0.5

angi = np.arctan2(ienorm,innorm)
ang = np.arctan2(enorm,nnorm)

angs = (angi-ang)%(2*np.pi)

assert ((angs - angs[0]) < 1e-10).all()
