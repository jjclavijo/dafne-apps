#!/bin/zsh


third_step () {
    CONT=$(buildah from dafne_haskell:cache)
    MPOINT=$(buildah mount $CONT)

    #update sources
    cp -r app src proy.cabal $MPOINT/opt/proyect/

    buildah run $CONT sh -c "cd /opt/proyect/; cabal new-build" >/dev/null

    mkdir -p build/bin

    mkdir -p $MPOINT/tmpfiles

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "chunk" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') $MPOINT/

    cp $(find $MPOINT/opt/proyect/dist-newstyle -type f -name "proy" -exec ls -l {} \; | awk '($1 ~ /x/){print $9}') $MPOINT/

    echo --- Finding required libs ---

    echo "libc6\nlibc-bin\nlibgcc1\ngcc-8-base" > $MPOINT/tmpfiles/pkgs

    buildah run $CONT sh -c 'ldd /chunk | cut -d\  -f3 | xargs -i dpkg -S {}' | cut -d: -f1 | sort | uniq >> $MPOINT/tmpfiles/pkgs
    buildah run $CONT sh -c 'ldd /proy | cut -d\  -f3 | xargs -i dpkg -S {}' | cut -d: -f1 | sort | uniq >> $MPOINT/tmpfiles/pkgs

    echo --- Finding required Files ---

    echo "/proy\n/chunk" > $MPOINT/tmpfiles/reqfiles

    echo '___'

    buildah run $CONT bash -c 'sort /tmpfiles/pkgs | uniq | xargs dpkg -L | xargs -i find {} -maxdepth 0 -type f,l 2>/dev/null 1>> /tmpfiles/reqfiles || true'

    echo '___'
    
    pushd $MPOINT
    sed 's/^\///' tmpfiles/reqfiles | tar -czf tmpfiles/reqfiles.tar.gz -T -
    popd

    mkdir -p artifacts

    cp $MPOINT/tmpfiles/reqfiles.tar.gz artifacts/

    echo --- Tar Generated ---

    buildah umount $CONT
    buildah rm $CONT
  }

echo -------------- Start --------------
third_step
