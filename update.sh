#!/bin/bash

cd ./packwiz || exit
for f in ./mods/*
do
    mod=${f#./mods/}
    mod=${mod%.pw.toml}
    echo "Processing $mod ..."
    packwiz mr install "$mod"
    if [[ $? -ne 0 ]]
    then
        echo "Installing $mod from CurseForge ..."
        packwiz cf install "$mod"
    fi
done
