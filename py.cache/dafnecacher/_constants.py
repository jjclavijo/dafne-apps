import os

ENV_DEFAULTS= { 'SI_SOCKDIR':'sockets/',
                'DAFNE_HOME':'/'}

DAFNE_HOME = os.environ.get('DAFNE_HOME',ENV_DEFAULTS.get('DAFNE_HOME'))

SI_SOCKDIR = os.environ.get('SI_SOCKDIR',
                            os.path.join(DAFNE_HOME,ENV_DEFAULTS.get('SI_SOCKDIR'))
                            )

SI_SOCKET_FILE = os.path.join(SI_SOCKDIR,'sidb_cmd')

CACHE_DIR = os.path.join(DAFNE_HOME,'cache/')
