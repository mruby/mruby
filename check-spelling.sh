#!/bin/bash

# creates a file of spelling mistakes if there are any
errors=0
path="tests/spelling/"
mkdir -p "$path"
misspelled="$path"misspelled-words.txt
if [ -f "$misspelled" ]; then
  rm "$misspelled"
fi
while read -r file; do
  echo "$file" >>"$path"misspelled-words-temp.txt
  aspell list --lang=en --encoding=utf-8 --personal=./.aspell.en.pws <"$file" | sort -u >>"$path"misspelled-words-temp.txt
  if [ "$(wc -l <"$path"misspelled-words-temp.txt)" -ge 2 ]; then
    echo >>"$path"misspelled-words-temp.txt
    cat "$path"misspelled-words-temp.txt
    cat "$path"misspelled-words-temp.txt >>"$path"misspelled-words.txt
    errors=1
  fi
  : >"$path"misspelled-words-temp.txt
done < <(find . -name "*.md")
if [ -f "$path"misspelled-words-temp.txt ]; then
  rm "$path"misspelled-words-temp.txt
fi
if [ "$errors" -ge 1 ]; then
  exit 1
else
  exit 0
fi
