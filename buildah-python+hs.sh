#!/bin/zsh

primero () {
  CONT=$(buildah from docker.io/python:3.7.12-slim)

  buildah run $CONT apt-get update
  buildah run $CONT apt-get -y -qq install $(cat hs.readdata/artifacts/required.list)

  MPOINT=$(buildah mount $CONT)

  mkdir .p $MPOINT/opt/bin

  cp hs.readdata/build/bin/* $MPOINT/opt/bin/
  cp -r hs.readdata/data $MPOINT/opt/

  buildah commit $CONT dafnepython:base

  buildah rm $CONT
}

#primero

CONT=$(buildah from docker.io/python:3.7.12-slim)
MPOINT=$(buildah mount $CONT)

buildah run $CONT python -m venv /opt/venv
buildah run $CONT /opt/venv/bin/python -m pip install -U pip wheel
buildah run $CONT /opt/venv/bin/python -m pip install tensorflow tfx_bsl jupyterlab flit

cp -r py.dset $MPOINT/opt/

buildah run $CONT sh -c 'cd /opt/py.dset ;env FLIT_ROOT_INSTALL=1 /opt/venv/bin/python -m flit install -s'

CONT2=$(buildah from dafnepython)
MPOINT2=$(buildah mount $CONT2)

cp -r --preserve=all $MPOINT/opt/venv $MPOINT2/opt/
cp -r py.dset $MPOINT/opt/

buildah umount $CONT
buildah umount $CONT2

buildah commit $CONT2 dafnepython:venv

buildah rm $CONT
buildah rm $CONT2
