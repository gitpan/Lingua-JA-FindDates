use 5.010000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Lingua::JA::FindDates',
    VERSION_FROM      => 'lib/Lingua/JA/FindDates.pm', # finds $VERSION
    PREREQ_PM         => {}, # Has no module dependencies.
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Lingua/JA/FindDates.pm', # retrieve abstract from module
       AUTHOR         => 'Ben Bullock <benkasminbullock@gmail.com>') : ()),
);
