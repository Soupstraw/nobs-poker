#!/bin/bash

watch_app(){
  while :
  do
    local cs="x"
    while [ $cs != $(checksum app/) ]
    do
      echo "Changes detected in app, recompiling"
      cs=$(checksum app/)
      stack build
    done
    stack run &
    local pid=$!
    sleep 3
    inotifywait -rqe modify app/
    kill $pid 2>/dev/null
  done
}

watch_client(){
  while :
  do
    elm make --output=elm.js src/Main.elm
    sleep 3
    inotifywait -rqe modify src/
    echo "Changes detected in client source files, recompiling"
  done
}

cleanup(){
  echo "Killing child processes $1 and $2"
  kill $1; kill $2
  exit
}

checksum(){
  find $1 -type f -exec md5sum {} \; | md5sum | cut -d " " -f 1
}

watch_app &
app_pid=$!
cd client
watch_client &
client_pid=$!
trap "cleanup $app_pid $client_pid" SIGINT

while :
do
  sleep 1000
done
