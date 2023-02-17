#!/bin/sh

# exit immediately
set -ex

stylua --check lua/ init.lua ftplugin

selene --config ./asset/lint/config.toml lua
