from importlib import import_module

import logging

log = logging.getLogger(__name__)

import click

from ..base import Saver
#from ..base import Loader

@click.command()
@click.argument('name',type=str)
@click.argument('output',type=str)
def save(name: str,output: str) -> bool:
    try:
        dset = import_module('.{}'.format(name),'dafnedset.presets')
        data = dset.data
    except ModuleNotFoundError:
        psets = import_module('.presets','dafnedset')
        data = psets.__dict__[name]

    Saver(data).write_parquet(output)

    return True
