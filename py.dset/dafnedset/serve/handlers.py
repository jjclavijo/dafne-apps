import os
import asyncio
from ast import literal_eval
from importlib import import_module
from io import BytesIO
from urllib.parse import parse_qs

import logging

log = logging.getLogger(__name__)

import numpy as np
import pyarrow as pa
import pandas as pd

from .base import DataServer

from .._constants import BATCH_SIZE

dataset_cache = {} # type: ignore

async def handle_cmd(cmd,writer,*args):

    print('Handling Command {}'.format(cmd))

    cmd,*opts = cmd.split('&')

    if cmd == 'serve_dataset':

        opts = parse_qs('&'.join(opts))
        #keep only first element,
        opts = {i:j[0] for i,j in opts.items()}

        # A Partir de Aqui debería ser parte de base
        batch_size = literal_eval(opts.get('batch',str(BATCH_SIZE))),
        repeat = literal_eval(opts.get('repeat','False'))
        data_name = opts.get('name',False)

        if not data_name:
            raise ValueError('Should Specify Dataset Name')

        if literal_eval(opts.get('cached','False')):
            cache = {**dataset_cache}
        else:
            cache = {}

        if data_name in cache:
            log.info('Using Memory Cached Version')
            data = cache[data_name]

        else:
            # TODO This doesn't has sense anymore since all imports on
            # presets are delayed operations, dont consume resources until
            # iterated.
            try:
                dset = import_module('.{}'.format(data_name),'dafnedset.presets')
                data = dset.data
            except ModuleNotFoundError:
                psets = import_module('.presets','dafnedset')
                data = psets.__dict__[data_name]

            if literal_eval(opts.get('cached','False')):
                dataset_cache[data_name] = data

        if literal_eval(opts.get('save_cache','False')):
            raise ValueError("we are not writing parquets anymore")

        if literal_eval(opts.get('serve','True')):
            server = DataServer.from_batch_generator(data)
            await server.serve_data(writer)
        # Hasta Aqui debería ser parte de base

    if cmd == 'quit':
        return False

    return True


async def handle_dummy(*args):
    return True

async def handle_quit(*args):
    return False

_dict = { 'cmd':handle_cmd,
          'quit':handle_quit,
          'dummy':handle_dummy,
          }
