#!/usr/bin/env bash
set -o nounset -o pipefail -o errexit

ROOMS=(
    monoidalmu
    loopinglambda
    phantomphi
    pointfreepsi
    typedtheta
    differentialdelta
    covariantkappa
    bifunctorialbeta
    existentialeta
    universalupsilon
)

for r in ${ROOMS[@]}; do
     curl -i 'https://uplcg.jaspervdj.be/rooms' \
         -H 'Content-Type: application/x-www-form-urlencoded' \
         --data-raw "id=$r&password=&deck=munihac2020"
done
