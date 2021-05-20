#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

cd $rootDir
mkdir -p scaladoc
find src/main/scala | egrep '[^.].*\.scala$' | xargs scaladoc -d scaladoc
