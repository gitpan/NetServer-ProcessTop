use ExtUtils::MakeMaker;

WriteMakefile(
	      PREREQ_PM => { Event => '0.38', 'Event::Stats' => '0.5' },
	      VERSION_FROM => "./ProcessTop.pm",
	      NAME => 'NetServer::ProcessTop',
	     );
