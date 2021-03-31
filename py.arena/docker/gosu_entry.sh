#!/bin/bash

# Add local user
# Either use the LOCAL_USER_ID if passed in at runtime or
# fallback
# DOCS: https://denibertovic.com/posts/handling-permissions-with-docker-volumes/

USER_ID=${LOCAL_USER_ID:-9001}

echo "Starting with UID : $USER_ID"
useradd --shell /bin/bash -u $USER_ID -o -c "" -m user
export HOME=/home/user

cd $HOME

cp -r /pytmp .

chown -R user pytmp
cd pytmp
# /usr/local/bin/gosu user pip -- install flit
/usr/local/bin/gosu user python -m flit install -s

if [ -d /arena ] 
then
  rm -r arena
  ln -s /arena
fi

# Dummy cleanup

function cleanup(){
  echo "no cleaning needed"
}

if [ $DAFNE_HOME = "/dafne_home" ]
then
  cd $DAFNE_HOME
  if [ $(stat --format '%u' ".") = 0 ]; then
    echo Using internal DAFNE_HOME
    [ -d cache ] || mkdir cache
    [ -d saved_models ] || mkdir saved_models
    chown $USER_ID:$USER_ID .
    chown -R $USER_ID:$USER_ID cache
    chown -R $USER_ID:$USER_ID saved_models
  elif [ $(stat --format '%u' ".") = $USER_ID ]; then
      echo Using same-user mounted DAFNE_HOME
  else
      orig_user=$(stat --format '%u' ".")
      echo Using othed-user mounted DAFNE_HOME
      [ -d cache ] || mkdir cache
      [ -d saved_models ] || mkdir saved_models
      chown $USER_ID:$USER_ID .
      chown -R $USER_ID:$USER_ID cache
      chown -R $USER_ID:$USER_ID saved_models

      function cleanup () {
        echo "restoring permissions"
        chown $orig_user:$orig_user "$DAFNE_HOME"
        chown -R $orig_user:$orig_user "$DAFNE_HOME/cache"
        chown -R $orig_user:$orig_user "$DAFNE_HOME/saved_models"
      }

  fi
fi

# if user mounted a notebooks volume work there
[ -d /notebooks ] && cd /notebooks || true

/usr/local/bin/gosu user "$@" &

cmdpid=$!

function realclean() {
  echo "Closing mainpid"
  kill -TERM $cmdpid
  echo "Cleaning"
  cleanup
  exit
}

function killandclean() {
  echo "Killing mainpid"
  kill -KILL $cmdpid
  echo "Cleaning"
  cleanup
  exit
}

trap realclean SIGTERM;
trap realclean SIGINT;
trap killandclean SIGKILL;

wait $!

cleanup
