import pydafne.data.batchers as ba
import pydafne.data.query_batch as qb
import pydafne.data.preprocessing as pp
import pydafne.data.tools as tools
from pydafne.data.contain import Container

data = qb.DefaultQuerys.positive()
data = tools.label(tools.scale(data),label=[1.,0.],batch_size=data.length)

splited = tools.split_feed(data,[0.7,0.2,0.1])
cached = tools.cache(splited)

cached.length = Container.pack([i.length for i in cached])
ch_mixer = pp.BatchProcessing.ch_mixer(times=1,p_true_pos=0.9,p_true_pos_ch=0.9)
pos10x8 = tools.feed(cached,repeat=True,max_length=cached.length*8,batch_size=cached.length)
pos_mix8 = tools.preprocess(pos10x8,mix=[ch_mixer])
import pydafne.data.batchers as ba
import pydafne.data.query_batch as qb
import pydafne.data.preprocessing as pp
import pydafne.data.tools as tools
from pydafne.data.contain import Container

data = qb.DefaultQuerys.positive()
data = tools.label(tools.scale(data),label=[1.,0.],batch_size=data.length)

splited = tools.split_feed(data,[0.7,0.2,0.1])
cached = tools.cache(splited)

cached.length = Container.pack([i.length for i in cached])
ch_mixer = pp.BatchProcessing.ch_mixer(times=1,p_true_pos=0.9,p_true_pos_ch=0.9)
pos10x8 = tools.feed(cached,repeat=True,max_length=cached.length*8,batch_size=cached.length)
pos_mix8 = tools.preprocess(pos10x8,mix=[ch_mixer])
import pydafne.data.batchers as ba
import pydafne.data.query_batch as qb
import pydafne.data.preprocessing as pp
import pydafne.data.tools as tools
from pydafne.data.contain import Container

data = qb.DefaultQuerys.positive()
data = tools.label(tools.scale(data),label=[1.,0.],batch_size=data.length)

splited = tools.split_feed(data,[0.7,0.2,0.1])
cached = tools.cache(splited)

cached.length = Container.pack([i.length for i in cached])
ch_mixer = pp.BatchProcessing.ch_mixer(times=1,p_true_pos=0.9,p_true_pos_ch=0.9)
pos10x8 = tools.feed(cached,repeat=True,max_length=cached.length*8,batch_size=cached.length)
pos_mix8 = tools.preprocess(pos10x8,mix=[ch_mixer])
cached_div = tools.feed(cached,max_length=(cached.length//8)*8,batch_size=cached.length//8)
cached_div
cached_div[0]
cached_div[0].batch_size
cached_div[0].batch_size.promise
cached_div[0].batch_size.promise[1][0].promise
cached_div[0].batch_size.promise[1][0]
cached_div[0].max_length.promise[1][0]
cached_div[0].max_length.promise[1][0].promise
cached_div[0].max_length.promise
suma = pos_mix8 + cached_div
l=0
for i in suma[0]:
    print(len(i))
    l += len(i)
    print(l)
    print('---')
l=0
for i in suma[1]:
    print(len(i))
    l += len(i)
    print(l)
    print('---')
l=0
for i in suma[2]:
    print(len(i))
    l += len(i)
    print(l)
    print('---')
cached_div[-1].batch_size
cached_div[-1].batch_size.eval()
%hist -f /home/javier/dafne/test-presets-v2.py
