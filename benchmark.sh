#!/bin/sh
set -xef

# run <LOOP_COUNT> <FILE_NAME>
# example: run 3 lib.rs
run () {
  count=0
  while [ $count -lt $1 ]
  do
  if [ -n "$2" ]; then
    nvim --headless --startuptime /tmp/nvim-startuptime -c 'au VimEnter * quitall' $2
  else
    nvim --headless --startuptime /tmp/nvim-startuptime -c 'au VimEnter * quitall'
  fi
  tail -n 1 /tmp/nvim-startuptime |\
    awk -F: '{print $1}' \
      >> benchmark.txt
  count=`expr $count + 1`
  done
}

# reset the benchmarks
echo "BENCHMARK (Test 3 times) (Unit: millisecond)" > benchmark.txt
echo "==============================================" >> benchmark.txt
echo "Elapse time | self + source time | source time" >> benchmark.txt

echo "=== Test 1: Open empty buffer ===" >> benchmark.txt
run 3 ''
echo "==================================" >> benchmark.txt

echo "=== Test 2: Open markdown file ===" >> benchmark.txt
run 3 'README.md'
echo "==================================" >> benchmark.txt

echo "=== Test 3: Open Lua code ===" >> benchmark.txt
run 3 'lua/config/lsp.lua'
echo "==================================" >> benchmark.txt
