"""
This little pachage only loads datasets from dafnedset server and makes
tensorflow caches of it on the desired (see _constants.py) directory
"""
__version__ = "0.0.1"

from .cacher import build_cache as main
