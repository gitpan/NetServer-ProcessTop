use ExtUtils::MakeMaker;

WriteMakefile(
	      PREREQ_PM => { Event => '0.42', 'Event::Stats' => '0.53' },
	      VERSION_FROM => "./ProcessTop.pm",
	      NAME => 'NetServer::ProcessTop',
	     );
