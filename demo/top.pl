#!./perl -w

use strict;
use Event 0.27 qw(loop);
require NetServer::ProcessTop;

for (1..40) { 
    my $i = 1 + rand;
    Event->timer(interval => $i, cb => sub {}, desc => sprintf("%.2f",$i));
}

my $top = NetServer::ProcessTop->new();
warn "Listening on port $top->{port}...\n";

loop();
