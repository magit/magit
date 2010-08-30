#!/usr/bin/env bash

set -e

function configure_ac_ver_ok {
  cat Makefile | grep "VERSION=${1}" || return 1
}

function magit_el_ver_ok {
  grep -e ";; Version: *$1" magit.el || return 1
}

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

# correct version in magit?
if ! magit_el_ver_ok "$tag"; then
  echo "Please set version in magit.el to $tag"
  git co master
  exit 1
fi

# correct version in configure.ac?
if ! configure_ac_ver_ok "$tag"; then
  echo "Please set AC_INIT to $tag in configure.ac"
  git co master
  exit 1
fi

# clean up if we need to
make clean

make dist

# back to master
git co master
