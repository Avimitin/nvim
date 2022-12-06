#!/bin/bash

# exit when any of the command fail
set -e

trap "echo Exit..." 1 2 3 6

# Create tmp directory
read -q "?I am going to create a directory under /tmp directory.
This directory is used for storing temparary download files.

Do you agree? [y/N] "

temp_download_dir=$(mktemp -d -t 'NVIM_CFG_DOWNLOAD_DIR_XXX')
echo
echo "* tmpdir: $temp_download_dir"
echo


# The "latest" tag will be tagged multiple time
url="https://github.com/Avimitin/nvim/archive/refs/tags/latest.tar.gz"
read -q "?I am going to download the neovim source file from $url

Do you agree? [y/N]"

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

read -q "?I am going to install the neovim configuration to $target_dir.

Do you agree? [y/N]"

echo
mkdir -p $target_dir
# move the downloaded file to the .config/nvim directory
mv -f $temp_download_dir/nvim-latest $target_dir

echo "Process done!"
