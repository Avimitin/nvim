#!/bin/sh

# exit immediately
set -e

echo -n "Formatting code..."
stylua --check lua/ init.lua
echo "Done"

echo -n "Linting code..."
selene lua init.lua after
echo "Done"
