use strict;
package NetServer::ProcessTop;
use Event 0.38 qw(time);
use Carp;
use Symbol;
use Socket;
use Sys::Hostname;
use Event::Stats 0.5;
use constant NICE => -1;
use vars qw($VERSION @ISA $BasePort $Host $OurInstance);
$VERSION = '1.02';

$BasePort = 7000;
$Host = eval { hostname } || 'somewhere';

sub import {
    eval {
	$OurInstance = NetServer::ProcessTop->new();
#	warn "Listening on ".(7000+($$%1000))."\n";
    };
    if ($@) { warn; return }
}

sub new {
    my ($class, $port) = @_;
    $port ||= $BasePort + $$ % 1000;
    
    # Mostly snarfed from perlipc example; thanks!
    my $proto = getprotobyname('tcp');
    my $sock = gensym;
    socket($sock, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
    setsockopt($sock, SOL_SOCKET, SO_REUSEADDR, pack('l', 1))
	or die "setsockopt: $!";
    bind($sock, sockaddr_in($port, INADDR_ANY)) or die "bind: $!";
    listen($sock, SOMAXCONN);

    my $o = bless { port => $port }, $class;
    $o->{io} = Event->io(fd => $sock, poll => 'r', nice => NICE,
			 cb => [$o, 'new_client'],
			 desc => "NetServer::ProcessTop");
    $o->{io}{topserver} = 1;
    $o;
}

sub new_client {
    my ($o, $e) = @_;
    my $sock = gensym;
    my $paddr = accept $sock, $e->w->fd or die "accept: $!";
    my ($port,$iaddr) = sockaddr_in($paddr);
    (bless {
	    stats => $o,
	    from => gethostbyaddr($iaddr, AF_INET) || inet_ntoa($iaddr),
	    sock => $sock,
	   }, ref($o).'::Client')->init;
}

sub DESTROY {
    my ($o) = @_;
#    warn "$o->DESTROY";
    close $o->{io}->fd;
    (delete $o->{io})->cancel; #has circular ref
    bless $o, $ISA[0];
}

package NetServer::ProcessTop::Client;
use Carp;
use vars qw(@Argv $Terminal $NextID);
use Event qw(all_watchers QUEUES time);
use Event::Watcher qw(ACTIVE SUSPEND QUEUED RUNNING);
use Event::Stats qw(round_seconds idle_time total_time);
use constant NICE => -1;
BEGIN {
    @Argv = @ARGV;
}
$NextID=1;

require Term::Cap;
$Terminal = 'xterm';
my $Term;

sub init {
    Event::Stats::collect(1);
    $Term ||= Term::Cap->Tgetent({ TERM => $Terminal, OSPEED => 9600 });
    my ($o) = @_;
    my $sock = $o->{sock};
    $o->{start_row} = 4;
    $o->{page} = 1;
    $o->{filter} = '';
    $o->{by} = 't';
    $o->{msg} = '';
    $o->{seconds} = round_seconds(60);
    $o->{io} = Event->io(fd => $sock, poll => 'r', nice => NICE,
			 timeout => 4,
			 cb => [$o, 'cmd'],
			 desc => ref($o)." $o->{from}");
    $o->{io}{topserver} = 1;
    @$o{'col', 'row'} = (80,24);
    $o->refresh();
}

sub ln {
    my ($o,$l) = @_;
    $l ||= '';
    my $c = $o->{col} - 1;
    if (length $l < $c) { $l .= ' 'x($c - length $l); }
    elsif (length $l > $c) { $l = substr($l,0,$c) }
    $l .= "\n";
    $l;
}

sub refresh {
    my ($o) = @_;

    $o->{rows_per_page} = $o->{row} - $o->{start_row} - 4;
    my $b = $Term->Tputs('cl',1,$o->{sock});

    $o->update(undef, $b);
}

sub edit {
    my ($o,undef,$s) = @_;
    $s .= $Term->Tgoto('cm', 0, 0, $o->{sock});
    $s .= "Event Minieditor"."\n"x3;
    my $e = $o->{edit};
    my $f = 'a';
    for my $k ($e->attributes) {
	my $v = $e->$k();
	$v = defined $v? $v:'<undef>';
	if (length $v > 40) {
	    $v = substr($v,0,40) . ' ...';
	}
	$v =~ s/\0/^0/g;
	$v =~ s/\n/\\n/g;
	$s .= $o->ln(sprintf("%s %-16s %-s", $f++, $k, $v));
    }
    $s .= $o->ln();
    $s .= $o->ln("(Use Zvalue to assign value to field 'Z'.  'x' when you are done.)");
    $s .= $o->ln();
    $s .= $o->ln($o->{msg});
    $s .= "% ";

    return $o->cancel if !defined syswrite $o->{sock}, $s, length $s;
}

sub help {
    my ($o,undef,$s) = @_;
    $s .= $Term->Tgoto('cm', 0, 0, $o->{sock});
    $s .= "NetServer::ProcessTop Help


  CMD      DESCRIPTION                                  
  -------- -----------------------------------------------------------
  ! <code> eval arbitrary perl code
  d #      set Event::DebugLevel                         [$Event::DebugLevel]
  e #id    edit event
  h        this screen
  o #how   order by t=time, i=id, r=ran, d=desc, p=prio  [$o->{by}]
  p #page  switch to page #page                          [$o->{page}]
  r #id    resume event
  s #id    suspend event
  t #sec   show stats for the last #sec seconds          [$o->{seconds}]
  x        exit
  z r,c    size screen to (rows, columns)                [$o->{row}, $o->{col}]
  /regexp  show events with matching descriptions        [$o->{filter}]






(Press return to continue.)";
    
    return $o->cancel if !defined syswrite $o->{sock}, $s, length $s;
}

sub update {
    my ($o, undef, $s) = @_;
    return if !exists $o->{io};

    if ($o->{screen}) {
	my $m = $o->{screen};
	$o->$m(undef,$s);
	return;
    }

    $s ||= '';

    $s .= $Term->Tgoto('cm', 0, 0, $o->{sock});
    my $name = $0;
    $name =~ s,^.*/,,;
    $s .= $o->ln("$name PID=$$ \@ $NetServer::ProcessTop::Host");

    my ($sec,$min,$hr) = localtime(time);
    my $tm = sprintf("| %02d:%02d:%02d [%4ds]", $hr,$min,$sec,$o->{seconds});
    $s .= $Term->Tgoto('cm', $o->{col} - (1+length $tm), 0, $o->{sock});
    $s .= $tm."\n";

    my @load;
    my @events = all_watchers();
    for my $sec (15,60,60*15) {
	my $busy = 0;
	for (@events) { $busy += ($_->stats($sec))[2] }
	my $idle = (idle_time($sec))[2];
	my $tm = $idle + $busy;
	push @load, $tm? $busy / $tm : 0;
    }

    my @all = map {
	$_->{id} ||= $NextID++;
	[{ obj => $_, id =>$_->{id}, desc => $_->desc, prio => $_->prio },
	 $_->stats($o->{seconds})]
    } @events;
    push @all, [{ id => 0, desc => 'idle', prio => QUEUES },
		idle_time($o->{seconds})];
    my $total = 0;
    for (@all) { $total += $_->[3] }
    my $other_tm = total_time($o->{seconds}) - $total;
    $other_tm = 0 if $other_tm < 0;
    push @all, [{ id => 0, desc => 'other processes', prio => -1 },
		0, 0, $other_tm];

    # $lag should not be affected by other processes
    my $lag = $total - $o->{seconds};
    $lag = 0 if $lag < 0;

#    $s .= $Term->Tgoto('cm', 0, 1, $o->{sock});
    $s .= $o->ln(sprintf("%d events; load averages: %.2f, %.2f, %.2f; lag %2d%%",
			 scalar @events, @load, $total? 100*$lag/$total : 0));
    $s .= "\n";

    $total += $other_tm; # add in other processes for %time [XXX optional?]

    my $filter = $o->{filter};
    @all = grep { $_->[0]{desc} =~ /$filter/ } @all
	if length $filter;

    $o->{page} = 1 if $o->{page} < 1;
    my $maxpage = int((@all + $o->{rows_per_page} - 1)/$o->{rows_per_page});
    $o->{page} = $maxpage if $o->{page} > $maxpage;

    my $page = " P$o->{page}";
    $s .= $o->ln("  EID PRI STATE   RAN  TIME   CPU TYPE DESCRIPTION");
    $s .= $Term->Tgoto('cm', $o->{col} - (1+length $page), $o->{start_row}-1, $o->{sock});
    $s .= $page."\n";

    if ($o->{by} eq 't') {
	@all = sort { $b->[3] <=> $a->[3] } @all;
    } elsif ($o->{by} eq 'i') {
	@all = sort { $a->[0]{id} <=> $b->[0]{id} } @all;
    } elsif ($o->{by} eq 'r') {
	@all = sort { $b->[1] <=> $a->[1] } @all;
    } elsif ($o->{by} eq 'd') {
	@all = sort { $a->[0]{desc} cmp $b->[0]{desc} } @all;
    } elsif ($o->{by} eq 'p') {
	@all = sort { $a->[0]{prio} cmp $b->[0]{prio} } @all;
    } else {
	warn "unknown sort by '$o->{by}'";
    }
    splice @all, 0, $o->{rows_per_page} * ($o->{page} - 1)
	if $o->{page} > 1;

    for (my $r = 0; $r < $o->{rows_per_page}; $r++) {
	my $st = shift @all;
	if ($st) {
	    my $e = $st->[0]{obj};
	    my ($type, $fstr);
	    if (!$e) {
		$type = 'sys';
		$fstr = '';
	    } else {
		$type = ref $e;
		$type =~ s/^Event:://;
		$fstr = do {
		    # make look pretty!
		    if ($e->is_suspended) {
			'S'.(($e->is_active? 'W':'').
			     ($e->is_queued?'Q':'').
			     ($e->is_running?'R':''))
		    } elsif ($e->is_running) {
			'cpu'
		    } elsif ($e->is_queued) {
			'queue'
		    } elsif ($e->is_active) {
			$type eq 'idle'? 'wait' : 'sleep'
		    } else {
			$type eq 'idle'? 'sleep' : 'zomb'
		    }
		};
	    }
	    my @prf = ($st->[0]{id},
		       $st->[0]{prio},
		       $fstr,
		       $st->[1],
		       int($st->[3]/60), $st->[3] % 60,
		       $total? 100 * $st->[3]/$total : 0,
		       substr($type,0,length($type)>4? 4:length($type)),
		       $st->[0]{desc});
#	    warn join('x', @prf)."\n";
	    my $line = sprintf("%5d  %2d %-5s %5d %2d:%02d%5.1f%% %4s %s", @prf);
	    $s .= $o->ln($line);
	} else {
	    $s .= $o->ln();
	}
    }

    $s .= "\n".$o->ln($o->{msg})."% ";

    return $o->cancel if !defined syswrite $o->{sock}, $s, length $s;
}

sub cmd {
    my ($o, $e) = @_;
    if ($e->got eq 't') {
	$o->update;
	return;
    }
    my $in;
    return $o->cancel if !sysread $e->w->fd, $in, 200;

    $in =~ s/\s+$//;
    $o->{msg} = '';

    my $scr = $o->{screen} || '';
    if ($scr eq 'edit') {
	if ($in eq '') {
	    # ignore
	} elsif ($in eq 'x') {
	    $o->{screen} = undef;
	    delete $o->{edit};
	} elsif ($in =~ m/^(\w)\s*(.+)$/) {
	    my $f = ord(lc $1) - ord 'a';
	    my $v = $2;
	    my $ev = $o->{edit};
	    my @k = $ev->attributes;
	    if ($f < 0 || $f >= @k) {
		$o->{msg} = "Field '$f' is not available";
	    } else {
		eval {
		    my $m = $k[$f];
		    $ev->$m($v)
		}; #hope safe enough!
		$o->{msg} = $@ if $@;
	    }
	} else {
	    $o->{msg} = "'$in'?";
	}
    } elsif ($scr eq '') {
	if ($in eq '') {
	    # ignore
	} elsif ($in eq 'h' or $in eq '?') {
	    $o->{screen} = 'help';
	} elsif ($in =~ m/^d\s*(\d+)$/) {
	    $Event::DebugLevel = int $1;
	} elsif ($in =~ m/^[xq]$/) {
	    $o->cancel;
	    return;
	} elsif ($in =~ m/^o\s*(\w+)$/) {
	    my $by = $1;
	    if ($by =~ m/^(t|i|r|d|p)$/) {
		$o->{by} = $by;
	    } else {
		$o->{msg} = "Sort by '$by'?  Type 'h' for help!";
	    }
	} elsif ($in =~ m/^p\s*(\d+)$/) {
	    $o->{page} = $1 || 1;
	} elsif ($in =~ m/^e\s*(\d+)$/) {
	    my @got = grep { $_->{id} == $1 } all_watchers();
	    if (@got) {
		if (exists $got[0]{topserver}) {
		    $o->{msg} = "I'm not allowed to edit myself.  Sorry.";
		} else {
		    $o->{edit} = $got[0];
		    $o->{screen} = 'edit';
		}
	    } else {
		$o->{msg} = "Can't find event id '$1'";
	    }
	} elsif ($in =~ m{ ^/ (.*) $ }x) {
	    $o->{filter} = $1;
	} elsif ($in =~ m/^z\s*(\d+)(\s*,\s*|\s+)(\d+)$/) {
	    my ($r,$c) = ($1,$3);
	    $r = 12 if $r < 12;
	    $c = 70 if $c < 70;
	    $o->{row} = $r;
	    $o->{col} = $c;
	} elsif ($in =~ m/^t\s*(\d+)$/) {
	    my $s = $1;
	    my $max = &Event::Stats::MAXTIME;
	    if ($s < 0) {
		$o->{msg} = "Sorry, past performance is not an indication of future performance.";
	    } else {
		$o->{seconds} =	round_seconds($1);
	    }
	} elsif ($in =~ m/^(s|r)\s*(\d+)$/) {
	    my $do = $1;
	    my $id = $2;
	    my $ev = (grep { $_->{id} == $id } all_watchers())[0];
	    if (!$ev) {
		$o->{msg} = "Can't find event '$id'.";
	    } elsif (exists $ev->{topserver}) {
		$o->{msg} = "Can't suspend/resume part of the TopServer.";
	    } else {
		$ev->suspend($do eq 's')
	    }
	} elsif ($in =~ s/^\!//) {
	    my $v;
	    $v = eval $in;
	    $v = '<undef>' if !defined $v;
	    $o->{msg} = $@? $@ : $v;
	} else {
	    $o->{msg} = "'$in'?  Type 'h' for help!";
	}
    } elsif ($scr eq 'help') {
	$o->{screen} = undef;
	$in = '';  #refresh
    } else {
	warn "? $scr";
    }

    $o->refresh;
}

sub cancel {
    my ($o) = @_;
#    warn "$o->cancel\n";
    close $o->{sock};
    $o->{io}->cancel;
    Event::Stats::collect(-1);
}

1;

__END__

=head1 NAME

NetServer::ProcessTop - Make event loop statistics easily available

=head1 SYNOPSIS

  require NetServer::ProcessTop;

  'NetServer::ProcessTop'->import();  # creates server
  warn "NetServer::ProcessTop listening on port ".(7000+($$ % 1000))."\n";

=head1 DESCRIPTION

This module implements a C<top>-like server that makes it easier to
debug complicated event loops.

All statistics collected by L<Event> are displayed in a format similar
to the popular (and excellent) C<top> program.  ProcessTop server
listens on port 7000+($$%1000) by default.

=head1 PRECISE STAT DEFINITIONS

=over 4

=item * idle

Idle tracks the amount of time that the process cooperatively
reliquished control to the operating system.  (Usually via L<select>
or L<poll>.)

=item * other processes

Attempts to estimate the process's non-idle time that the operating
system instead gave to other processes. (Actual clock time minus the
combined total time spent in idle and in running event handlers.)
This stat is an underestimate (lower bound) since the process can also
be preemptively interrupted I<during> event processing.

=item * lag

Lag is the percent over the I<planned amount of time> that the event
loop took to complete.  ((Actual time - planned time) / planned time)

=back

It is unfortunately that more intuitive stats are not available.
Benchmarking is a slippery business.  If you have ideas for
improvements, don't be shy!

=head1 SCREENSHOT

 pl/3bork  [12836 @ eq1062]                                    12:15:18 [  15s]
 21 events (6 zombies); load averages: 0.33, 0.32, 0.32                        
                                                                               
  EID PRI STATE  RAN  TIME  CPU  TYPE DESCRIPTION                            P1
    0   8 sleep  247  0:09 66.7%  sys idle                                     
   24   4 sleep  105  0:04 28.1% idle QSGTable sweep                           
    3   4 sleep   15  0:00  2.4% time server status                            
   22   4 sleep   15  0:00  1.8% time QSGTable                                 
   12   4 cpu      4  0:00  0.8% time NetServer::ProcessTop::Client localhost  
   23   4 sleep   15  0:00  0.1% time QSGTable Detail                          
    2   4 sleep   15  0:00  0.0% time Qt                                       
    7   4 sleep    8  0:00  0.0% time ObjStore::Serve checkpoint               
   10   4 sleep    8  0:00  0.0% time SSL items                                
    9   4 sleep    8  0:00  0.0% time SSL service                              
    6  -1 sleep    5  0:00  0.0% time Event::Stats                             
   21   4 zomb     0  0:00  0.0% idle QSGTable sweep                           
   19   4 zomb     0  0:00  0.0% time QSGTable                                 
   11   4 sleep    0  0:00  0.0%   io NetServer::ProcessTop::Client localhost  
    8   4 sleep    0  0:00  0.0%   io SSL                                      
   13   4 zomb     0  0:00  0.0% time QSGTable                                 

The three load averages are for the most recent 15 seconds, 1 minute,
and 15 minutes, respectively.

For efficiency, not all time intervals are available.  When you change
the time interval, it will be rounded to the closest for which there
is data.

=head1 BUGS

The potential impact of multiple CPUs and kernel-level thread support
is ignored.

=head1 SUPPORT

The best place for questions about this extension is
perl-loop@perl.org.

If you wish to subscribe to this mailing list, send email to
majordomo@perl.org.

=head1 COPYRIGHT

Copyright © 1998-1999 Joshua Nathaniel Pritikin.  All rights reserved.
This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
