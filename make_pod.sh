#!/bin/bash

# Crear pod compartiendo el puerto de Jupyter
podman pod create -n dafne -p 8888:8888

# Container de Jupyter
podman run --pod dafne --name dafne_jupyter \
  localhost/dafnepython:venv \
  /opt/venv/bin/python -m jupyterlab --allow-root --ip "*"

# Container de la base de datos
podman run --pod dafne --name dafne_db \
  docker.io/jjclavijo/dafne-db:dset-latest
