use strict;
package NetServer::ProcessTop;
use Event 0.10;
use Carp;
use Symbol;
use Socket;
use base 'Event::Stats';
use vars qw($VERSION @ISA $BasePort $Host);
$VERSION = '0.50';

$BasePort = 7000;
chop($Host = `hostname`);
$Host =~ s/\..*$//; #ok?

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
    $o->{io} = Event->io(handle => $sock, events => 'r',
			 callback => [$o, 'new_client'],
			 desc => "NetServer::ProcessTop");
    $o->{io}{topserver} = 1;
    $o->restart();
    $o;
}

sub new_client {
    my ($o, $e) = @_;
    my $sock = gensym;
    my $paddr = accept $sock, $e->{handle};
    my ($port,$iaddr) = sockaddr_in($paddr);
    (bless {
	    stats => $o,
	    mod => time,
	    from => gethostbyaddr($iaddr, AF_INET) || inet_ntoa($iaddr),
	    sock => $sock,
	   }, ref($o).'::Client')->init;
}

sub DESTROY {
    my ($o) = @_;
#    warn "$o->DESTROY";
    close $o->{io}{handle};
    (delete $o->{io})->cancel; #has circular ref
    bless $o, $ISA[0];
}

package NetServer::ProcessTop::Client;
use Carp;
use builtin qw(min);
use vars qw(@Argv $Terminal);
BEGIN { @Argv = @ARGV }

$Terminal = 'vt100';
require Term::Cap;
my $Term;

sub init {
    $Term ||= Term::Cap->Tgetent({ TERM => $Terminal, OSPEED => 9600 });
    my ($o) = @_;
    my $sock = $o->{sock};
    $o->{start_row} = 4;
    $o->{page} = 1;
    $o->{filter} = '';
    $o->{by} = 't';
    $o->{msg} = '';
    $o->{seconds} = 15;
    $o->{io} = Event->io(handle => $sock, events => 'r',
			 callback => [$o, 'cmd'],
			 desc => ref($o)." $o->{from}");
    $o->{timer} = Event->timer(interval => 4, callback => [$o,'update'],
			       desc => ref($o)." $o->{from}");
    for (@$o{'io','timer'}) { $_->{topserver} = 1 }
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

    @$o{'col', 'row'} = (80,24); #XXX can get dynamically? how?!
    $o->{rows_per_page} = $o->{row} - $o->{start_row} - 4;
    my $b = $Term->Tputs('cl',1,$o->{sock});

    $b .= $o->ln("$0 ".join(' ', @Argv)." [$$ \@ $NetServer::ProcessTop::Host]");
    $b .= $o->ln();
    $b .= $o->ln();
    $b .= $o->ln("  EID PRI STATE  RAN  TIME  CPU  TYPE DESCRIPTION");

    $o->update(undef, $b);
}

sub help {
    my ($o) = @_;
    my $s = $Term->Tputs('cl',1,$o->{sock});
    $s .= "NetServer::ProcessTop Help


  CMD      DESCRIPTION                                  
  -------- -----------------------------------------------------------
  b #how   sort by t=time, i=id, r=ran, d=desc           [$o->{by}]
  h        this screen
  p #page  switch to page #page                          [$o->{page}]
  r #id    resume event
  s #id    suspend event
  t #sec   show stats for the last #sec seconds          [$o->{seconds}]
  x        exit
  /regexp  show events with matching descriptions        [$o->{filter}]








(Press return to continue.)";
    
    return $o->cancel if !defined syswrite $o->{sock}, $s, length $s;
}

my $statusMask = (Event::ACTIVE | Event::SUSPEND |
		  Event::QUEUED | Event::RUNNING);
sub update {
    my ($o, $e, $s) = @_;
    return if !exists $o->{io} || $o->{help};

    $s ||= '';

    my ($sec,$min,$hr) = localtime(time);
    my $tm = sprintf(" %02d:%02d:%02d [%4ds]", $hr,$min,$sec,$o->{seconds});
    $s .= $Term->Tgoto('cm', $o->{col} - (1+length $tm), 0, $o->{sock});
    $s .= $tm."\n";

    my @load;
    my @events = $o->{stats}->events();
    my $zombies = 0;
    for (@events) { ++$zombies if (($_->{flags} & $statusMask) == 0) }
    for my $sec (15,60,60*15) {
	my $busy = 0;
	for (@events) { $busy += ($_->stats($sec))[1] }
	my $idle = ($o->{stats}->idle($sec))[1];
	my $tm = $idle + $busy;
	push @load, $tm? $busy / $tm : 0;
    }
#    $s .= $Term->Tgoto('cm', 0, 1, $o->{sock});
    $s .= $o->ln(sprintf("%d events (%d zombies); load averages: %.2f, %.2f, %.2f",
			 scalar @events, $zombies, @load));

    my $filter = $o->{filter};
    @events = grep { $_->{desc} =~ /$filter/ } @events
	if $filter;

    $o->{page} = 1 if $o->{page} < 1;
    my $maxpage = int((@events + $o->{rows_per_page})/$o->{rows_per_page}); #+idle
    $o->{page} = $maxpage if $o->{page} > $maxpage;

    my $page = " P$o->{page}";
    $s .= $Term->Tgoto('cm', $o->{col} - (1+length $page), $o->{start_row}-1, $o->{sock});
    $s .= $page."\n";

    my @all = map { [$_, $_->stats($o->{seconds})]  } @events;
    push @all, [{ id => 0, flags => Event::ACTIVE,
		  desc => 'idle', priority => Event::Loop::QUEUES()+1 },
		$o->{stats}->idle($o->{seconds})];
    if ($o->{by} eq 't') {
	@all = sort { $b->[2] <=> $a->[2] } @all;
    } elsif ($o->{by} eq 'i') {
	@all = sort { $a->[0]{id} <=> $b->[0]{id} } @all;
    } elsif ($o->{by} eq 'r') {
	@all = sort { $b->[1] <=> $a->[1] } @all;
    } elsif ($o->{by} eq 'd') {
	@all = sort { $a->[0]{desc} cmp $b->[0]{desc} } @all;
    } else {
	warn "unknown sort by '$o->{by}'";
    }
    my $total = 0;
    for (@all) { $total += $_->[2] }
    splice @all, 0, $o->{rows_per_page} * ($o->{page} - 1)
	if $o->{page} > 1;

    for (my $r = 0; $r < $o->{rows_per_page}; $r++) {
	my $st = shift @all;
	if ($st) {
	    my $e = $st->[0];
	    my $flags = $e->{flags} & $statusMask;
	    my $fstr = do {
		# make look pretty!
		if ($flags == Event::ACTIVE) {
		    'sleep'
		} elsif (($flags & ~Event::ACTIVE) == Event::RUNNING) {
		    'cpu'
		} elsif ($flags == 0) {
		    'zomb'
		} elsif ($flags & Event::SUSPEND) {
		    'stop'
		} elsif ($flags == Event::QUEUED) {
		    'queue'
		} else {
		    ($flags & Event::ACTIVE? 'W':'').
			($flags & Event::SUSPEND? 'S':'').
			    ($flags & Event::QUEUED? 'Q':'').
				($flags & Event::RUNNING? 'R':'')
		}
	    };
	    my $type = $e->{id}==0? 'sys' : ref $e;
	    $type =~ s/^Event:://;
	    my @prf = ($e->{id},
		       $e->{priority},
		       $fstr,
		       $st->[1],
		       int($st->[2]/60), $st->[2] % 60,
		       $total? 100 * $st->[2]/$total : 0,
		       substr($type,0,min(length $type, 4)),
		       $e->{desc});
#	    warn join('x', @prf)."\n";
	    my $line = sprintf("%5d  %2d %-5s %4d %2d:%02d%5.1f%% %4s %s", @prf);
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
    my $in;
    return $o->cancel if !sysread $e->{handle}, $in, 200;

    $o->{msg} = '';
    if ($o->{help}) {
	$o->{help} = 0;
	$in = 'c';
    }

    $in =~ s/\s+$//;
    if ($in eq '') {
	# ignore
    } elsif ($in eq 'h' or $in eq '?') {
	$o->{help} = 1;
	return $o->help;
    } elsif ($in =~ m/^[xq]$/) {
	$o->cancel;
	return;
    } elsif ($in =~ m/^by?\s+(\w+)$/) {
	my $by = $1;
	if ($by =~ m/^(t|i|r|d)$/) {
	    $o->{by} = $by;
	} else {
	    $o->{msg} = "Sort by '$by'?  Type 'h' for help!";
	}
    } elsif ($in =~ m/^p\s*(\d+)$/) {
	$o->{page} = $1;
    } elsif ($in =~ m{ ^/ (\w*) $ }x) {
	$o->{filter} = $1;
    } elsif ($in =~ m/^t\s*(\d+)$/) {
	my $s = $1;
	my $max = Event::Stats::MAXTIME;
	if ($s < 1 or $s > $max) {
	    $o->{msg} = "Statistics are only available for the last $max sec.";
	} else {
	    $o->{seconds} = $1;
	}
    } elsif ($in =~ m/^(s|r)\s*(\d+)$/) {
	my $do = $1;
	my $id = $2;
	my $ev = (grep { $_->{id} == $id } $o->{stats}->events())[0];
	if (!$ev) {
	    $o->{msg} = "Can't find event '$id'.";
	} elsif (exists $ev->{topserver}) {
	    $o->{msg} = "Can't suspend/resume part of the TopServer.";
	} else {
	    $do eq 's'? $ev->suspend : $ev->resume;
	}
    } else {
	$o->{msg} = "'$in'?  Type 'h' for help!";
    }
    $o->refresh;
}

sub cancel {
    my ($o) = @_;
#    warn "$o->cancel\n";
    close $o->{sock};
    map { $_->cancel } delete @$o{'io','timer'};
}

1;

__END__

=head1 NAME

NetServer::ProcessTop - Make event loop statistics easily available

=head1 SYNOPSIS

  require NetServer::ProcessTop;

  my $TopServer = NetServer::ProcessTop->new();
  warn "NetServer::ProcessTop listening on port ".(7000+($$ % 1000))."\n";

=head1 DESCRIPTION

This module implements a server that makes it much easier to debug
complicated event loops.

All statistics collected by L<Event> are displayed in a format similar
to the popular C<top> program.  A C<vt100> terminal is assumed. The
server listens on port 7000+($$%1000) by default.

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

=head1 SUPPORT

The best place for questions about this extension is
perl-loop@perl.org.

If you wish to subscribe to this mailing list, send email to
majordomo@perl.org.

=head1 COPYRIGHT

Copyright © 1998 Joshua Nathaniel Pritikin.  All rights reserved.
This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
