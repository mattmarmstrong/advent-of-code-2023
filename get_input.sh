#!/usr/bin/env bash

set -eu

SESSION_VALUE="53616c7465645f5f69e1f1a41d3588dadfa562eb773f88fe839183df3bbbe677bd935ea24fa94eadc166c501f438d0bb65937f0dcf7411a070ecb101183015b3"
curl -b "session=$SESSION_VALUE" "https://adventofcode.com/2023/day/"$1"/input" > ~/Misc/advent-of-code-2023/day_"$1"/input.txt
