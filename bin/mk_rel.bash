#!/bin/sh

set -e

if [ -z "$1" ]; then
    make dist
else
    home_rev=$(git name-rev --name-only HEAD)
    make clean
    git checkout "$1"
    make dist
    git checkout "$home_rev"
fi