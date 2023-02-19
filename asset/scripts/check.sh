#!/bin/sh

# exit immediately
set -ex

stylua --check lua/ init.lua after

selene --config ./asset/lint/config.toml lua
