#!/usr/bin/env bash

CODESPELL_WORDLIST="codespell.txt"
temp_file=$(mktemp)
sort <"${CODESPELL_WORDLIST}" | uniq >"${temp_file}"
cat "${temp_file}" >"${CODESPELL_WORDLIST}"
rm "${temp_file}"
