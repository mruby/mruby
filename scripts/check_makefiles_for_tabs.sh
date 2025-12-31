#!/bin/bash

# Iterate over all files passed as arguments by pre-commit
for makefile in "$@"; do
  # Check if the file exists and is a regular file
  if [[ -f "$makefile" ]]; then
    if grep -P '^\s' "$makefile" | grep -vP '^\t' > /dev/null; then
      echo "Error: File '$makefile' contains spaces at the beginning of lines instead of tabs."
      exit 1
    fi
  fi
done
exit 0
