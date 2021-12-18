#!/bin/zsh

first_step () {

    CONT=$(buildah from docker.io/debian:bullseye-slim)

    buildah run $CONT apt-get update

    MPOINT=$(buildah mount $CONT)

    BASE_URL='https://downloads.haskell.org/debian/pool/main'

    GHC_VER='9.0.1'
    GHC_REL='9.0.1-10~deb10_amd64'

    CABAL_VER='3.4'
    CABAL_REL='3.4.0.0%2Bgit20210220.2.be18bb7-6~deb10_amd64'


    curl -J -O "${BASE_URL}/g/ghc-${GHC_VER}/ghc-${GHC_VER}_${GHC_REL}.deb"
    curl -J -O "${BASE_URL}/c/cabal-install-${CABAL_VER}/cabal-install-${CABAL_VER}_${CABAL_REL}.deb"

    mv "ghc-${GHC_VER}_${GHC_REL}.deb"\
       "cabal-install-${CABAL_VER}_${CABAL_REL}.deb" \
       $MPOINT/

    buildah run $CONT apt -q -y install \
      "./ghc-${GHC_VER}_${GHC_REL}.deb" \
      "./cabal-install-${CABAL_VER}_${CABAL_REL}.deb" \
      --no-install-recommends

    rm "$MPOINT/ghc-${GHC_VER}_${GHC_REL}.deb" 
       "$MPOINT/cabal-install-${CABAL_VER}_${CABAL_REL}.deb" 

    pushd $MPOINT
    ln -s /opt/cabal/bin/cabal bin/cabal
    ln -s /opt/ghc/bin/ghc bin/ghc
    popd

    buildah commit $CONT dafne_haskell:base

    buildah umount $CONT
    buildah rm $CONT
  }

second_step () {
    CONT=$(buildah from dafne_haskell:base)

    POSTGRES_VER='13'

    #TODO: Versioning postgres server and liblzma

    buildah run $CONT apt -q -y install liblzma-dev postgresql-server-dev-$POSTGRES_VER

    buildah commit $CONT dafne_haskell:libs

    buildah rm $CONT
  }

third_step () {
    CONT=$(buildah from dafne_haskell:libs)
    MPOINT=$(buildah mount $CONT)

    mkdir -p $MPOINT/opt/proyect/

    cp -r app src proy.cabal $MPOINT/opt/proyect/

    buildah run $CONT sh -c "cabal update; cd /opt/proyect/; cabal new-build"

    mkdir -p build/bin

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "chunk" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') build/bin/

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "proy" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') build/bin/

    buildah commit $CONT dafne_haskell:cache
    buildah umount $CONT
    buildah rm $CONT
  }

fourth_step () {
    CONT=$(buildah from dafne_haskell:cache)
    MPOINT=$(buildah mount $CONT)

    rm -r $MPOINT/opt/proyect/

    buildah commit $CONT dafne_haskell:cache-clean
    buildah umount $CONT
    buildah rm $CONT
  }

echo -------------- 1 --------------
#first_step

echo -------------- 2 --------------
second_step

echo -------------- 3 --------------
third_step

echo -------------- 4 --------------
fourth_step
