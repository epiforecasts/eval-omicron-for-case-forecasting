#!/bin/bash

git add --all
date=$(date +'%Y-%m-%d')
git commit -m "$date - forecast update"
git pull -Xours
git push
