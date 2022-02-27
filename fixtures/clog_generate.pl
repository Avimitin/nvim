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

sub beautify {
  my @in = shift;
  foreach my $cmt (@in) {
    print Dumper $cmt;
  }
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
  # normal commit format
  my @other = ();

  for my $line (@$in) {
    if ($line =~ m/\[([A-Z!]{3})(!?)\] ([a-zA-z\/-_]+): (.*)/) {
      my %cmt = (
        type => $1,
        break => $2 ? 1 : 0,
        module => $3,
        summary => $4,
      );

      if ($cmt{break}) {
        push @bc, \%cmt;
        next;
      }

      SWITCH:
      for ($cmt{type}) {
        if (/NEW|FET/) { push @nc, \%cmt; last SWITCH; }
        if (/FIX/) { push @fc, \%cmt; last SWITCH;}
        if (/RFT|RWT/) { push @rc, \%cmt; last SWITCH;}
        if (/DOC|MSC/) { push @other, \%cmt; last SWITCH;}
      }
    } else {
      print "WARN: commit not in valid format: $line\n";
    }
  }

  return (
    feat     => \@nc,
    fix      => \@fc,
    refactor => \@rc,
    breaking => \@bc,
    other    => \@other,
  )
}

sub parse {
  my $title = shift;
  my (@inp) = @_;
  # if there is nothing in the input
  if ( !@inp ) {
    return;
  }

  my $temp = "$title:\n";
  $temp = $temp.'-' x (length($title) + 1);
  $temp = $temp."\n";

  foreach my $cmt (@inp) {
    $temp = $temp."  * $cmt->{module}: $cmt->{summary}\n";
  }

  return $temp;
}

my %cliargs = parse_cmdline();
my @output = get_log(\%cliargs);
my %result = parse_log(\@output);

print parse("BREAKING CHANGES!", @ { $result{breaking} });
print "\n";
print parse("New Features", @ { $result{feature} });
print "\n";
print parse("Fix", @ { $result{fix} });
print "\n";
print parse("Changes", @ { $result{refactor} });
print "\n";
print parse("Others", @ { $result{other} });
print "\n";
