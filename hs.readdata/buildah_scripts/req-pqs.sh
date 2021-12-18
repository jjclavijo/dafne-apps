#!/bin/zsh


third_step () {
    CONT=$(buildah from dafne_haskell:cache)
    MPOINT=$(buildah mount $CONT)

    #update sources
    cp -r app src proy.cabal $MPOINT/opt/proyect/

    buildah run $CONT sh -c "cd /opt/proyect/; cabal new-build" >/dev/null


    mkdir -p $MPOINT/tmpfiles

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "chunk" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') $MPOINT/tmpfiles

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "proy" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') $MPOINT/tmpfiles

    buildah run $CONT sh -c 'ldd /tmpfiles/chunk | cut -d\  -f3 | xargs -i dpkg -S {} | cut -d: -f1 | sort | uniq > /pkgs.list'
    buildah run $CONT sh -c 'ldd /tmpfiles/proy | cut -d\  -f3 | xargs -i dpkg -S {} | cut -d: -f1 | sort | uniq >> /pkgs.list'

    mkdir -p artifacts

    sort $MPOINT/pkgs.list | uniq >! artifacts/required.list

    buildah umount $CONT
    buildah rm $CONT
  }

echo -------------- 3 --------------
third_step
