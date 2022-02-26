#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long qw(GetOptions);

my $git_log_cmd = "git log --oneline --pretty=\"format:%s (See commit %h)\"";

sub parse_cmdline {
  my $from;
  my $to;
  my $range;
  GetOptions(
    'from=s' => \$from,
    'to=s' => \$to,
    'range=s' => \$range,
  );

  return (
    from => $from,
    to => $to,
    range => $range,
  )
}

sub get_log {
  my $iargs = shift;
  if (!$iargs) {
    die "No arguments\n";
  }

  my $range;
  if (!$iargs->{range}) {
    if ($iargs->{from} && $iargs->{to}) {
      $range = $iargs->{from}."..".$iargs->{to};
    } else {
      die "You must give a range to generate changelog\n";
    }
  } else {
    $range = $iargs->{range};
  }

  my $exec = $git_log_cmd." ".$range;
  my $output = `$exec`;
  my @output = split("\n", $output);

  for my $line (@output) {
    if ($line =~ m/\[([A-Z!]{3})\]/) {
      print "Type: $1\n";
    }
  }
}

my %cliargs = parse_cmdline();
get_log(\%cliargs);
