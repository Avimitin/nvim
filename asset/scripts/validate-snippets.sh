#!/bin/bash

snippets=$(find ./vsnip -type f -name '*.json')

validate() {
  cat $1 | jq -e . >/dev/null 2>&1
}

for file in ${snippets[@]}; do
  if ! validate $file; then
    echo "$file is invalid"
    exit 1
  fi
done

echo "All file is valid"
