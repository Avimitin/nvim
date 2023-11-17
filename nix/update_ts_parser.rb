#! /usr/bin/env nix-shell
#! nix-shell -p ruby -i ruby

# frozen_string_literal: true

require 'json'

srcs = `nix eval --json .#nvim-treesitter-parsers --apply \
  "drv: map (p: { name = p.pname; url = p.src.url; hash = p.src.outputHash; }) drv.tsParsers"`

JSON.parse(srcs).each do |info|
  name, url, hash = info.values_at("name", "url", "hash")
  puts "Downloading #{name} source from #{url}"
  new_file = `nix-prefetch-url #{url} --print-path --type sha256 | tail -n1`.strip
  new_hash = `nix hash file --base16 --type sha256 --sri #{new_file}`.strip
  if new_hash != hash
    puts "hash for parser #{name} changed from #{hash} to #{new_hash}, updating"
    `sed -i "s|#{hash}|#{new_hash}|" overlay.nix`
  end
end
