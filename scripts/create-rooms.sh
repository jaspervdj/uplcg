#!/usr/bin/env bash
set -o nounset -o pipefail -o errexit

for ((r=1; r <= 20; r++)); do
    echo $r
    curl 'https://uplcg.jaspervdj.be/rooms' \
        -H 'Content-Type: application/x-www-form-urlencoded' \
        --data-raw "id=room$(printf "%02d" "$r")&password=&deck=icfp2020"
done
