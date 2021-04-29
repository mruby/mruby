#!/bin/bash

# prints an ordered list of spelling mistakes
while read -r file; do
  aspell list --lang=en --encoding=utf-8 <"$file" | sort -u
done < <(find . -name "*.md")
