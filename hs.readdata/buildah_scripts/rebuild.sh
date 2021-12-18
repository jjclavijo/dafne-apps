#!/bin/zsh


third_step () {
    CONT=$(buildah from dafne_haskell:cache)
    MPOINT=$(buildah mount $CONT)

    #update sources
    cp -r app src proy.cabal $MPOINT/opt/proyect/

    buildah run $CONT sh -c "cabal update; cd /opt/proyect/; cabal new-build"

    mkdir -p build/bin

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "chunk" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') build/bin/

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "proy" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') build/bin/

    buildah umount $CONT
    buildah rm $CONT
  }

echo -------------- 3 --------------
third_step
