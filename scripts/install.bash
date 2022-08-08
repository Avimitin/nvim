#!/bin/bash

# exit when any of the command fail
set -e

ok_or_exit ()
{
  if [[ "$1" != "y" && "$1" != "Y" ]]; then
    echo
    echo "exit"
    exit 0
  fi
}

# Create tmp directory
read -p "I am going to create a directory under /tmp directory.
This directory is used for storing temparary download files.

Do you agree? [y/N] " confirm && ok_or_exit $confirm

temp_download_dir=$(mktemp -d -t 'NVIM_CFG_DOWNLOAD_DIR_XXX')
echo
echo "* tmpdir: $temp_download_dir"
echo


# The "latest" tag will be tagged multiple time
url="https://github.com/Avimitin/nvim/archive/refs/tags/latest.tar.gz"
read -p "I am going to download the neovim source file from $url

Do you agree? [y/N]" confirm && ok_or_exit $confirm

# download the files
echo
echo "Downloading..."
curl -sSL $url | tar xz -C $temp_download_dir

target_dir="$HOME/.config/nvim"

# if the configuration is already exist, rename it to nvim.backup-{current datetime}
if [[ -d "$target_dir" ]]; then
  echo "Find existing configuration, trying to backup"
  mv -r $target_dir "$HOME/.config/nvim.backup-$(date +%Y%m%d-%H-%M)"
fi

read -p "I am going to install the neovim configuration to $target_dir.

Do you agree? [y/N]" confirm && ok_or_exit $confirm

echo
mkdir -p $target_dir
# move the downloaded file to the .config/nvim directory
mv -f $temp_download_dir/nvim-latest $target_dir

# copy and rename config file
cp $target_dir/lua/custom.example.lua $target_dir/lua/custom.lua

echo "Process done!"
