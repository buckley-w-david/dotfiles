#!/usr/bin/bash

updates=$(checkupdates)
if [[ -n "$updates" ]]; then
    if [[ "$updates" = "0" ]]; then
        echo "0"
    else
        echo "$updates" | wc -l
    fi
else
    echo "0"
fi
