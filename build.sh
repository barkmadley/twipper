#!/bin/sh

includes=`find src -type d | sed 's/^/-I /'`

target=${1-main.native}

# echo corebuild ${includes} -package cohttp.async ${target}
corebuild -j 2 ${includes} -package cohttp.async,cow,cow.syntax ${target}
