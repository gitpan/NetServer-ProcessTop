use ExtUtils::MakeMaker;

@PREFIX = (PREFIX => $ENV{PERL5PREFIX})
    if exists $ENV{PERL5PREFIX};

WriteMakefile(
	      @PREFIX,
	      PREREQ_PM => { Event => '0.11' },
	      VERSION_FROM => "./ProcessTop.pm",
	      NAME => 'NetServer::ProcessTop',
	     );
