#!/usr/bin/env bash

set -eu

curl -b "session=$SESSION_VALUE" "https://adventofcode.com/2023/day/"$1"/input" > ~/Misc/advent-of-code-2023/day_"$1"/input.txt
