"""
Monkey pathch all Base clases used in dataset transformation and augmentation.
adding methods for easy representation of transformations as
data.transform().etc...
"""

from typing import Tuple
classes_to_patch: Tuple

from .base import BatchBuffer,FeededBuffer,MultiFeededBuffer,SyncdBuffer,\
                   Repeater,Cacher,CachedLoader,Loader

from .datasets import DefaultQuerys

from .tools import toolify as add_tools


classes_to_patch = (BatchBuffer,FeededBuffer,MultiFeededBuffer,SyncdBuffer,\
                   Repeater,Cacher,CachedLoader,Loader,\
                   DefaultQuerys)

def patch_toolify(classes=classes_to_patch):
    for cls in classes:
        add_tools(cls)
        #This should verify containerization patching is already applied
        try:
            add_tools(cls.Contained)
        except AttributeError:
            raise AttributeError("This patch should be applyed after contain")

"""
This is for enabling "Contained" versions of clasess.

Any

Feed -> params -> Food

process can be represented as

Feed.Contained(*n) -> params.Contained(*n) -> Food.Contained(*n)

"""

from .base import BatchBuffer,FeededBuffer,MultiFeededBuffer,SyncdBuffer,\
                   Repeater,Cacher,CachedSaver,CachedLoader,Loader,Saver

from .datasets import DefaultQuerys
from .extensions import PromisedInt,PromisedResult,Container
#from .extensions.patch import contained as patch_contained

classes_to_patch = (BatchBuffer,FeededBuffer,MultiFeededBuffer,SyncdBuffer,\
                   Repeater,Cacher,CachedSaver,CachedLoader,Loader,Saver,\
                   DefaultQuerys,\
                   PromisedInt,PromisedResult)

def patch_contain(classes=classes_to_patch):
    for cls in classes:
        patch_contained(cls)

"""
Monkey patch classes

patch_contained to create contained version of classes
Simply wrapping a list and exposing @acumulable methods,
those methods will apply one-to-one mappings of the
underlaying methods.
"""

import logging
log = logging.getLogger(__name__)

def patch_contained(cls,*args):

    name = 'Contained{}'.format(cls.__name__)
    log.debug('_Containable:decorating {}'.format(name))
    base = (Container,)
    attr = {'contained_class':cls,'decorate_extra':args}
    contained_version = type(name,base,attr)

    def repeat(self,times):
        return Container.pack([self]*times)

    setattr(cls,'repeat',repeat)
    setattr(cls,'Contained',contained_version)

    return cls

#_monky_patch_contained(PromisedInt)
#_monky_patch_contained(PromisedResult)

