#!/bin/sh

# exit immediately
set -e

echo -n "Formatting code..."
stylua lua/ init.lua
echo "Done"

echo -n "Linting code..."
selene --config ./fixtures/lint/config.toml lua
echo "Done"
