#!/bin/sh
set -ef

OUTPUT_FILE="./fixtures/benchmark.txt"

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
      >> $OUTPUT_FILE
  count=`expr $count + 1`
  done
}

# reset the benchmarks
echo "BENCHMARK (Test 3 times) (Unit: millisecond)" > $OUTPUT_FILE
echo "==============================================" >> $OUTPUT_FILE
echo "Elapse time | self + source time | source time" >> $OUTPUT_FILE

echo "=== Test 1: Open empty buffer ===" >> $OUTPUT_FILE
run 3 ''
echo "==================================" >> $OUTPUT_FILE

echo "=== Test 2: Open markdown file ===" >> $OUTPUT_FILE
run 3 'README.md'
echo "==================================" >> $OUTPUT_FILE

echo "=== Test 3: Open Lua code ===" >> $OUTPUT_FILE
run 3 'lua/config/lsp.lua'
echo "==================================" >> $OUTPUT_FILE
