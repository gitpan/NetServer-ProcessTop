#!./perl -w
#!./perl -wT

BEGIN { 
    # untaint
    @INC = map { $_ =~ /^(.*)$/; $1 } @INC;
    require Config;
    push @INC, (map { $_, "$_/$Config::Config{archname}" }
		split /:+/, $ENV{PERL5LIB});
    @INC = map { $_ =~ /^(.*)$/; $1 } @INC;
}
use strict;
use Event qw(loop);
require NetServer::ProcessTop;

for (1..40) { 
    my $i = 1 + rand;
    Event->timer(interval => $i, cb => sub {}, desc => sprintf("%.2f",$i));
}

my $top = NetServer::ProcessTop->new();
warn "Listening on port $top->{port}...\n";

loop();
