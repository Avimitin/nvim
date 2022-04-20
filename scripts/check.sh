#!/bin/sh

# exit immediately
set -e

echo -n "Formatting code..."
stylua --check lua/ init.lua
echo "Done"

echo -n "Linting code..."
selene --config ./scripts/lint/config.toml lua
echo "Done"
