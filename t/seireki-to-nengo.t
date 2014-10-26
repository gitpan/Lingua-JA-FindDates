use warnings;
use strict;
use Lingua::JA::FindDates qw/seireki_to_nengo/;
use utf8;
use Test::More;

my $date1 = '1989年1月1日';
my $date2 = '1989年10月10日';

is (seireki_to_nengo ($date1), '昭和64年1月1日');
is (seireki_to_nengo ($date2), '平成1年10月10日');

done_testing ();
