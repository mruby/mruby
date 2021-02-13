#!/usr/bin/env bash

cd .. || exit
# print first
grep -EHInr '( +)$' ./*

var=$(grep -EHInr '( +)$' ./*)
# then exit with fail if found
if test -z "$var"; then
  exit 0
else
  exit 1
fi
