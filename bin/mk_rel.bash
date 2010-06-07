#!/usr/bin/env bash

set -e

USAGE="usage: ${0##*/} <tag>"

tag="${1}"

[ ${#} -lt 1 ] && {
    echo ${USAGE} >&2
    exit 2
}

# does the specified tag exist?
if ! git tag | grep "${tag}"; then
  echo "Make sure you've tagged '${tag}'"
  exit 3
fi

# grab that tag
git co "${tag}"

# clean up if we need to
[ -f Makefile ] && make distclean

./autogen.sh

./configure

make dist

git co master
