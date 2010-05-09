#!/usr/bin/env bash

set -e

USAGE="usage: ${0##*/} <tag>"

tag="${1}"

[ ${#} -lt 1 ] && {
    echo ${USAGE} >&2
    exit 2
}

# does the specified tag exist?
if ! git tag | grep "${tag}" &>/dev/null; then
  echo "Make sure you've tagged '${tag}'"
  exit 3
fi

# grab the manual out of specified tag
git show "${tag}":magit.texi > magit.texi

makeinfo --css-include style/manual.css --html --no-split magit.texi
