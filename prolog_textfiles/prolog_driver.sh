#!/bin/bash

# you need to send ONE argument to this bash script.
# that way, $1 will have the name of a prolog source file

swipl -q -s "$1" -q -t main -f ""