#!/bin/bash

cd ./packwiz
for f in ./mods/*
do
    mod=${f#./mods/}
    mod=${mod%.pw.toml}
    echo "$mod"
    packwiz mr install "$mod"
    if [[ ! $? ]]
    then
        packwiz cf install "$mod"
    fi
done
