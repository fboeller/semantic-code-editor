#!/usr/bin/env bash
if [ $# -eq 0 ]
  then
    tag='latest'
  else
    tag=$1
fi

docker build -t fboeller/semantic-code-editor:$tag --target app .

docker build -t fboeller/semantic-code-editor-webterminal:$tag --target webterminal .
