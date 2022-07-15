#!/bin/bash

temp_download_dir=$(mktemp -d -t 'NVIM_CFG_DOWNLOAD_DIR_XXX')

# The "latest" tag will be tagged multiple time
url="https://github.com/Avimitin/nvim/archive/refs/tags/latest.tar.gz"

echo "Downloading..."
curl -sSL $url | tar xz -C $temp_download_dir

target_dir="$HOME/.config/nvim"

if [[ -d "$target_dir" ]]; then
  echo "Find existing configuration, trying to backup"
  mv -r $target_dir "$HOME/.config/nvim.backup-$(date +%Y%m%d-%H-%M)"
fi

mv -f $temp_download_dir/nvim-latest $target_dir
