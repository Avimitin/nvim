#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use Data::Dumper;

my $git_log_cmd = "git log --oneline --pretty=\"format:%s (See commit %h)\"";

# Accept commandline argument like:
# --from verA --to verB
# OR:
# --range verA..verB
# Follow the git worktree syntax
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

# Get log message by git log
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

  # split the whole result to line by line array
  my @output = split("\n", $output);

  return @output;
}

# Helper function
sub beautify {
  my @in = shift;
  foreach my $cmt (@in) {
    print Dumper $cmt;
  }
}

# Categorize log
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
    # Match rule like
    # [TYPE] MODULE: SUMMARY
    if ($line =~ m/([NFRDM]{1})(\.|!) ([a-zA-Z\/\-_]+): (.*)/) {
      my %cmt = (
        type => $1,
        break => $2 eq "!" ? 1 : 0,
        module => $3,
        summary => $4,
      );

      if ($cmt{break}) {
        push @bc, \%cmt;
        next;
      }

      SWITCH:
      for ($cmt{type}) {
        if (/N/) { push @nc, \%cmt; last SWITCH; }
        if (/F/) { push @fc, \%cmt; last SWITCH;}
        if (/R/) { push @rc, \%cmt; last SWITCH;}
        if (/D|M/) { push @other, \%cmt; last SWITCH;}
      }
    } else {
      print "WARN: commit not in valid format: $line\n";
    }
  }

  # Return reference to the array to save memory.
  return (
    feat     => \@nc,
    fix      => \@fc,
    refactor => \@rc,
    breaking => \@bc,
    other    => \@other,
  )
}

# Parse result in restructure text format.
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

# Main
my %cliargs = parse_cmdline();
my @output = get_log(\%cliargs);
my %result = parse_log(\@output);

print parse("BREAKING CHANGES!", @ { $result{breaking} });
print "\n";
print parse("New Features", @ { $result{feat} });
print "\n";
print parse("Fix", @ { $result{fix} });
print "\n";
print parse("Changes", @ { $result{refactor} });
print "\n";
print parse("Others", @ { $result{other} });
print "\n";
