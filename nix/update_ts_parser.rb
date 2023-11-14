#! /usr/bin/env nix-shell
#! nix-shell -p ruby -i ruby

# frozen_string_literal: true

require 'json'

pkg = `nix eval --impure --raw --expr \
  "with import <nixpkgs> { overlays = [(import ./overlay.nix)]; }; nvim-treesitter-parsers"`
puts "Updating dependencies for #{pkg}"

drv_path = `nix-store --query --deriver "#{pkg}"`.strip
puts "Derivation for #{pkg}: #{drv_path}"

def filter_treesitter_drv(drv)
  raw_output = `nix derivation show #{drv}^*`
  JSON.parse(raw_output)[drv]['inputDrvs'].keys.select { |name| name.match(/tree-sitter-\w+-/) }
end

parser_inputs = filter_treesitter_drv(drv_path)
src_inputs = parser_inputs.map { |drv| filter_treesitter_drv(drv) }.flatten

srcs = src_inputs.map do |drv|
  info = JSON.parse(`nix derivation show #{drv}^*`)[drv]['env']
  { name: info['name'], url: info['urls'], hash: info['outputHash'] }
end

srcs.each do |info|
  name, url, hash = info.values_at(:name, :url, :hash)
  puts "Downloading #{name} source from #{url}"
  new_file = `nix-prefetch-url #{url} --print-path --type sha256 | tail -n1`.strip
  new_hash = `nix hash file --base16 --type sha256 --sri #{new_file}`.strip
  if new_hash != hash
    puts "hash for parser #{name} changed from #{hash} to #{new_hash}, updating"
    `sed -i "s|#{hash}|#{new_hash}|" overlay.nix`
  end
end
