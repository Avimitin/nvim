#!/usr/bin/perl

my $output_file = "./fixtures/benchmark.txt";
my $sttime_file = "/tmp/nvim-startuptime";
my $nvim_cmd = "nvim --headless --startuptime ".$sttime_file." -c 'au VimEnter * quitall'";

sub read_last_line {
  return `tail -n1 /tmp/nvim-startuptime`;
}

sub split_score {
  return split(' ', shift);
}

sub run_nvim {
  if (@_[0]) {
    system($nvim_cmd, shift);
  } else {
    system($nvim_cmd);
  }
}

sub run_test {
  my $times = shift;
  my $file = shift;
  my @records = ();

  for ($i = 0; $i < $times; $i++) {
    if ($file) {
      run_nvim($file);
    } else {
      run_nvim();
    }
    my @scores = split_score(read_last_line());
    my $score = @scores[0];
    push @records, $score;
  }

  my @records = sort @records;
  my $sum = 0;
  foreach $n (@records) {
    $sum = $sum + $n
  }
  my $avg = $sum / @records;
  print "Max: @records[-1] ms\n";
  print "Min: @records[0] ms\n";
  print "Avg: $avg ms\n";
}

sub write_result {
  open(fh, ">", $output_file);
  my $hint = << "EOF";
BENCHMARK (TEST 10 Times) (Unit: millisecond)
=============================================

==== Test 1, Open empty buffer ==============
Max elapse time: %.2f
Min elapse time: %.2f
Avg elapse time: %.2f

==== Test 2, Open markdown file =============
Max elapse time: %.2f
Min elapse time: %.2f
Avg elapse time: %.2f

==== Test 3, Open Lua code ==================
Max elapse time: %.2f
Min elapse time: %.2f
Avg elapse time: %.2f
EOF
  print fh "$hint\n";
  close(fh);
}
