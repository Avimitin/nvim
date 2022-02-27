#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use Data::Dumper;

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

  return @output;
}

sub parse_log {
  my $in = shift;

  # new feature collector
  my @nc = ();
  # fix collector
  my @fc = ();
  # refactor collector
  my @rc = ();
  # breaking change collector
  my @bc = ();

  for my $line (@$in) {
    if ($line =~ m/\[([A-Z!]{3})(!?)\] ([a-zA-z\/-_]+): (.*)/) {
      my %cmt = (
        type => $1,
        break => $2 ? 1 : 0,
        module => $3,
        summary => $4,
      );
      print Dumper \%cmt;
    } else {
      print "WARN: commit not in valid format: $line\n";
    }
  }
}


my %cliargs = parse_cmdline();
my @output = get_log(\%cliargs);
parse_log(\@output);
