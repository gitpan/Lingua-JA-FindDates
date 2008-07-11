=encoding UTF-8

=head1 NAME

Lingua::JA::FindDates - find Japanese dates & convert them

=head1 SYNOPSIS

Find and replace Japanese dates in a string.

  use Lingua::JA::FindDates 'subsjdate';

Given a string, find and substitute all the Japanese dates in it.

  my $dates = '昭和４１年三月１６日';
  print subsjdate ($dates);

prints

  March 16, 1966

=item Find dates within a string

This module finds dates and substitutes dates inside a string:

  my $dates = 'blah blah blah 三月１６日';
  print subsjdate ($dates);

prints

  blah blah blah March 16

It can call back a routine each time a date is found:

  sub replace_callback
  {
    my ($data, $before, $after) = @_;
    print "$before was replaced by $after\n";
  }
  my $dates = '三月１６日';
  my $data = 'xyz'; # something to send to replace_callback
  subsjdate ($dates, \&replace_callback, $data);

prints

  三月１６日 was replaced by March 16

Use L<make_date_callback> to format the date any way:

  sub my_date
  {
    return join '/', @_[1,2];
  }
  my $dates = '三月１６日';
  print subsjdate ($dates, undef, undef, \&my_date);

prints

  3/16

=head1 DESCRIPTION

This module uses a set of regular expressions to detect Japanese-style
dates in a string. Dates includes year/month/day-style dates such as 平
成20年七月十日 I<Heisei nij?nent?ka>, but may also include
combinations such as years alone, years and months, month and day
without a year, fiscal years, parts of the month like 中旬 (ch?jun),
and periods of time.

=item Matches 99.99% of Japanese dates

This module has been road-tested on hundreds of documents, and it can
cope with virtually any kind of Japanese date. If you find any date
which it can't cope with, please report that as a bug.

=back

=head2 More examples

If you would like to see more examples of how this module works, look
at the testing code in C<t/Lingua-JA-FindDates.t>.

=head2 Exports

This module can export two functions, L<subsjdate> and
L<kanji2number>, on request.

=cut

package Lingua::JA::FindDates;

use 5.008000;
require Exporter;
use AutoLoader qw(AUTOLOAD);

our @ISA = qw(Exporter);

@EXPORT_OK= qw/subsjdate kanji2number/;

our $VERSION = '0.005';
use warnings;
use strict;
use utf8;

my %kanjinums = 
(
一 => 1,
二 => 2,
三 => 3,
四 => 4,
五 => 5,
六 => 6,
七 => 7,
八 => 8,
九 => 9,
十 => 10,
百 => 100,
千 => 1000, # Dates shouldn't get any bigger than this X a digit
); 

my $kanjidigits = join ('', keys %kanjinums);

=head2 kanji2number

=over

=item kanji2number ($knum)

C<kanji2number> is a very simple kanji number convertor. Its input is
one string of kanji numbers only, like '三十一'. It can deal with
kanji numbers with or without ten/hundred/thousand kanjis.

The return value is the numerical value of the kanji number, like 31,
or zero if it can't read the number.

=back

=head3 Bugs

kanji2number only goes up to thousands, because usually dates only go
that far. If you need a comprehensive Japanese number convertor, use
L<Lingua::JA::Numbers> instead of this. Also, it doesn't deal with
mixed kanji and arabic numbers.

=cut

sub kanji2number
{
    my ($knum) = @_;
#    print "$knum\n";
    my @kanjis = split '', $knum;
    my $value = 0;
    my $keta = 1;
    my @values;
    while (1) {
	my $k = pop @kanjis;
	return $value if !$k;
	my $val = $kanjinums{$k};
	if (!$val) {
	    print STDERR __PACKAGE__,
		"::kanji2number can't cope with '$k' of input '$knum'.\n";
	    return 0;
	}
	if ($val >= 10) {
	    $keta = $val;
	    my $knext = pop @kanjis;
	    if (!$knext) {
		return $value + $val;
	    }
	    my $val_next = $kanjinums{$knext};
	    if (!$val_next) {
		print STDERR __PACKAGE__,
		    "::kanji2number can't cope with '$knext' of input '$knum'.\n";
		return 0;
	    }
	    if ($val_next > 10) {
		push @kanjis, $knext;
		$value += $val;
	    } else {
		$value += $val_next * $val;
	    }
	} else {
	    $value += $val * $keta;
	    $keta *= 10;
	}
    }
}

# Map double-byte numbers to single byte numbers.

my $nums = '０１２３４５６７８９';
my @wnums = split '', $nums;
my %wtonarrow;
for (0..9) {$wtonarrow{$wnums[$_]} = $_}
my $jdigit = '[０-９0-9]';
# Japanese number
my $jnumber = "($jdigit+|[$kanjidigits]+)";
# Western year
my $wyear = '('.$jdigit.'{4}|['.$kanjidigits.']?千['.$kanjidigits.']*)\s*年';

# Japanese eras (Heisei, Showa, Taisho, Meiji). Japanese people
# sometimes write these eras using the letters H, S, T, and M.
my $jera = '(H|Ｈ|平成|S|Ｓ|昭和|T|Ｔ|大正|M|Ｍ|明治)';
# Map of Japanese eras to Western dates.
my %jera2w = (
H    => 1988,
Ｈ   => 1988,
平成 => 1988,
S    => 1925,
Ｓ   => 1925,
昭和 => 1925,
T    => 1911,
Ｔ   => 1911,
大正 => 1911,
M    => 1869,
Ｍ   => 1869,
明治 => 1869,
);

# Japanese year, with era like "Heisei" at the beginning.
my $jyear = $jera.'\s*'.$jnumber.'\s*年';
# Ten day periods (thirds of a month)
my %jun = qw/初 1 上 1 中 2 下 3/;
my @jun2english = ('invalid', 'early ', 'mid-', 'late ');
# Japanese days of the week, from Monday to Sunday.
my $weekdays = '月火水木金土日';
my @weekdays = split '',$weekdays;
# Match a string for a weekday, like 月曜日 or (日)
my $match_weekday = '[（(]?(['.$weekdays.'])(?:曜日|曜)?[)）]?';
# Match a day of the month, like 10日
my $match_dom = $jnumber.'\s*日';
# Match a month
my $match_month = $jnumber.'\s*月';
# Match jun
my $match_jun = '(['.join ('', keys %jun).'])\s*旬';
# Match a month+jun
my $match_month_jun = $match_month.'\s*'.$match_jun;
# Match a month and day of month pair
my $match_month_day = $match_month.'\s*'.$match_dom;
# Match a Japanese year, month, day string
my $matchymd = $jyear.'\s*'.$match_month_day;
# Match a Western year, month, day string
my $matchwymd = $wyear.'\s*'.$match_month_day;
# Match a Japanese year and month only
my $match_jyear_month = $jyear.'\s*'.$match_month;
# Match a Western year and month only
my $match_wyear_month = $wyear.'\s*'.$match_month;
# Separators used in date strings
my $separators = '\s*[〜−]\s*';
# 

=head2 Matching patterns

I<The module can be used without reading this section>.

The Japanese date regular expressions are stored in an array
B<jdatere> containing a pair of a regular expression to match a kind
of date, and a string like "ymdw" which contains letters saying what
to do with $1, $2, etc. from the regular expression. The array
B<jdatere> is ordered from longest match (like "year / month / day /
weekday") to shortest (like "year" only). For example, if the first
letter is "y", then $1 is a year in Western format like 2008, or if
the third letter is "w", then $3 is the day of the week, from 1 to 7.

=over

=item e

Japanese era (string).

=item j

Japanese year (string representing small number)

=item x

empty month and day

=item m

month number (from 1 to 12, 13 for a blank month, 0 for an invalid month)

=item d

day of month (from 1 to 31, 0 for an invalid day)

=item w

weekday (from Monday = 1 to Sunday = 7, zero or undefined for an
invalid weekday)

=item z

jun (旬), a ten day period.

=item 1

After another code, indicates the first of a pair of two things. For
example, the matching code for

  平成９年１０月１７日〜２０日

is

  ejmd1d2

=back

=cut

my @jdatere = (
# Match an empty string like 平成 月 日 as found on a form etc.
[$jyear.'(\s+)月\s+日'          , "ejx"],
# Add match for dummy strings here

# Match a Japanese era, year, month1 day1 - month 2 day2 combination
[$matchymd.$separators.$match_month_day, "ejm1d1m2d2"],
# Match a Japanese era, year, month1 - month 2 combination
[$jyear.'\s*'.$jnumber.'\s*月?'.$separators.$match_month, "ejm1m2"],
# Match a Japanese era, year, month, day1 - day2 combination
[$match_jyear_month.'\s*'.$jnumber.'\s*日?'.$separators.$match_dom, "ejmd1d2"],
# Match a Japanese era, year, month, day, weekday combination
[$matchymd.'\s*'.$match_weekday     , "ejmdw"],
# Match a Japanese era, year, month, day
[$matchymd                     , "ejmd"],
# Match a Japanese era, year, month, jun
[$match_jyear_month.'\s*'.$match_jun    , "ejmz"],
# Match a Western year, month, day, weekday combination
[$matchwymd.'\s*'.$match_weekday    , "ymdw"],
# Match a Western year, month, day combination
[$matchwymd           	       , "ymd"],
# Match a Western year, month, jun combination
[$match_wyear_month.'\s*'.$match_jun     , "ymz"],
# Match a Japanese era, year, month
[$jyear.'\s*'.$jnumber.'\s*月' , "ejm"],
# Match a Western year, month
[$match_wyear_month     , "ym"],
# Match a month, day, weekday
[$match_month_day.'\s*'.$match_weekday     , "mdw"],
# Match a month, day
[$match_month_day              , "md"],
# Match a fiscal year (年度, nendo in Japanese). These usually don't
# have months combined with them, so there is nothing to match a
# fiscal year with a month.
[$jyear.'度'                   , "en"],
# Match a fiscal year (年度, nendo in Japanese). These usually don't
# have months combined with them, so there is nothing to match a
# fiscal year with a month.
[$wyear.'度'                   , "n"],
# Match a Japanese era, year
[$jyear,                        "ej"],
# Match a Western year
[$wyear                        , "y"],
# Match a month with a jun
[$match_month.'\s*'.$match_jun , "mz"],
# Match a month
[$match_month                    , "m"],
);
my @months = qw/Invalid January February March April May June July
		August September October November December MM/;
my @days = qw/Invalid Monday Tuesday Wednesday Thursday Friday Saturday Sunday/;
my %j2eweekday;
@j2eweekday{@weekdays} = (1..7);

=head2 make_date

=over

=item make_date ($year, $month1, $date1, $wday1, $jun, $month2, $date2, $wday2)

This is the default date making routine. It's not exported.

L<subsjdate>, given a date like 平成２０年７月３日（木）, passes this
routine the values (2008, 7, 3, 4) for the year, month, date and day
of the week respectively. Then this makes a string 'Thursday, July 3,
2008' and returns it to L<subsjdate>.

You can use any other format for the date by supplying your own
L<make_date_callback> routine to L<subsjdate>.

=back

=cut

sub make_date
{
    my ($year, $month1, $date1, $wday, $jun, $month2, $date2) = @_;
    my $edate = '';
    $edate = $days[$wday].", " if $wday;
    if ($month1) {
	$month1 = int ($month1); # In case it is 07 etc.
	$edate .= $months[$month1];
	if ($month2) {
	    $edate .= '-'.$months[$month2];
	}
	if ($jun) {
	    $edate = $jun2english[$jun] . $edate;
	}
    }
    if ($date1) {
	$edate .= " " if length ($edate);
	$date1 = int ($date1); # In case it is 07 etc.
	if ($date2) {
	    $date1 .= '-'.int($date2);
	}
	if ($year) {
	    $edate .= "$date1, $year";
	} else {
	    $edate .= "$date1";
	}
    } elsif ($year) {
	$edate .= " " if length ($edate);
	$edate .= $year;
    }
    return $edate;
}

=head2 $verbose

If you want to see what the module is doing, set

  $Lingua::JA::FindDates::verbose = 1;

This makes L<subsjdate> print out each regular expression and reports
whether it matched, which looks like this:

  Looking for y in ([０-９0-9]{4}|[十六七九五四千百二一八三]?千[十六七九五四千百二一八三]*)\s*年
  Found '千九百六十六年': Arg 0: 1966 -> '1966'

=cut

our $verbose = 0;

=head2 subsjdate

=over

=item subsjdate ($L<text>, $L<replace_callback>, $L<data>, $L<make_date_callback>)

"subsjdate", given a string (argument 1) containing some text like 平
成２０年７月３日（木）, looks through the string using a set of
regular expressions, and if it finds anything, it calls L<make_date>
to make the equivalent date in English, and then substitutes it into
$text:

C<$text =~ s/平成２０年７月３日（木）/Thursday, July 3, 2008/g>;

Users can supply a different date making function. See
L<make_date_callback>.

=item text

Argument one is a string of Japanese, encoded in Perl's internal
encoding.

=item replace_callback

If there is a replace_callback value in argument two, it calls that
with the data in argument 3 and the before and after string. If you
don't want to call anything, you can leave this blank. In the original
script, see L<history>, replace_callback is a function which calls
Microsoft Word via Win32::OLE.

=item data

Argument three is any data you want to pass to L<replace_callback>. In
my original version, this is a reference to a hash which contains the
document object to pass to Word.

=item make_date_callback

Argument four is your replacement for the L<make_date> function. If
you don't need to replace the default (if you want American-style
dates), you can leave this blank. If, for example, you want dates in
the form "Th 2008/7/3", you could write a routine like the following:

  sub mymakedate
  {
      my ($year, $month, $date, $wday) = @_;
      return qw{Bad Mo Tu We Th Fr Sa Su}[$wday]." $year/$month/$date";
  }

In practice you need to check for $year, $month, $date, and $wday
being zero, since L<subsjdate> matches "month/day" and "year/month"
only dates.

=back

=head3 Bugs

=over

=item No sanity check of Japanese era dates

It does not detect that dates like 昭和百年 (Showa 100, an impossible
year) are invalid.

=item Only goes back to Meiji

The date matching only goes back to the Meiji era. There is
L<DateTime::Calendar::Japanese::Era> if you need to go back further.

=item Doesn't find dates in order

The dates returned won't be in the order that they are in the text,
but in the order that they are found by the regular expressions, which
means that in a string with two dates, the callbacks might be called
for the second date before they are called for the first one.

=item UTF-8 version only

This module only understands Japanese encoded in Perl's internal form
(UTF-8).

=back

=cut

sub subsjdate
{
    # $text is the text to substitute. It needs to be in Perl's
    # internal encoding.
    # $replace_callback is a routine to call back if we find valid dates.
    # $data is arbitrary data to pass to the callback routine.
    my ($text, $replace_callback, $data, $make_date_callback) = @_;

    for my $datere (@jdatere) {
	my $regex = $$datere[0];
	my @process = split (/(?=[a-z][12]?)/, $$datere[1]);
	print "Looking for ",$$datere[1]," in ",$regex,"\n" if $verbose;
	while ($text =~ /($regex)/g) {
	    my $orig = $1;
	    my @matches = ($2,$3,$4,$5,$6,$7); # uh - oh. Be careful!
	    my ($year, @month, @date, $wday, $jun);
	    print "Found '$orig': " if $verbose;
	    for (0..$#matches) {
		my $arg = $matches[$_];
		last if !$arg;
		$arg =~ s/([０-９])/$wtonarrow{$1}/g;
		$arg =~ s/([$kanjidigits]+)/kanji2number($1)/ge;
		print "Arg $_: $arg " if $verbose;
		my $argdo = $process[$_];
		if ($argdo eq 'e') { # Era name in Japanese
		    $year = $jera2w{$arg};
		} elsif ($argdo eq 'j') { # Japanese year
		    $year += $arg;
		} elsif ($argdo eq 'y') {
		    $year = $arg;
		} elsif ($argdo eq 'n') {
		    $year += $arg;
		    $year = "fiscal ".$year;
		} elsif ($argdo eq 'm' || $argdo eq 'm1') {
		    $month[0] = $arg;
		} elsif ($argdo eq 'd' || $argdo eq 'd1') {
		    $date[0] = $arg;
		} elsif ($argdo eq 'm2') {
		    $month[1] = $arg;
		} elsif ($argdo eq 'd2') {
		    $date[1] = $arg;
		} elsif ($argdo eq 'w') {
		    $wday = $j2eweekday{$arg};
		} elsif ($argdo eq 'z') {
		    $jun = $jun{$arg};
#		    print "Jun of $arg is $jun\n";
		} elsif ($argdo eq 'x') {
		    print "Dummy date '$orig'.\n" if $verbose;
		    $date[0]  = "DD";
		    $month[0] = 13;
		}
	    }
	    my $edate;
	    if ($make_date_callback) {
		$edate = &{$make_date_callback} 
		    ($year, $month[0], $date[0], $wday, $jun, $month[1], $date[1]);
	    } else {
		$edate = make_date ($year, $month[0], $date[0], $wday, $jun, $month[1], $date[1]);
	    }
	    print "-> '$edate'\n" if $verbose;
	    $text =~ s/\Q$orig\E/$edate/g;
	    if ($replace_callback) {
		&{$replace_callback}($data, $orig, $edate);
	    }
	}
    }
    return $text;
}

=head1 Author

Ben Bullock, benkasminbullock@gmail.com

=head1 History

This routine started life as a Visual Basic for Applications (VBA)
script (a "Word Macro") to automatically convert Japanese dates in a
Microsoft Word document into their equivalent English versions. See
http://linuxtnt.wordpress.com/2008/04/16/visual-basic-date-translator-updated/
. Eventually, because I kept finding exceptions & I didn't know Visual
Basic well enough to code that efficiently, I decided to rewrite it
all in Perl, using L<Win32::OLE> to automate the operation of
Microsoft Word. (The Microsoft Word handlers are not included in this
module.)

The basic idea is to ask Word to save a copy of the file as text via
OLE, then read the text file in to Perl, look for dates in the text
using L<subsjdate>, and then call back into Microsoft Word using the
L<replace_callback> argument to L<subsjdate> to substitute the
Japanese dates with English ones.

=head1 See also

These other modules might be more suitable for some purposes:

=over

=item L<DateTime::Locale::JA>

This does the minimal stuff to make a Japanese date. One of those
modules which has been made for completeness rather than for
usefulness, it doesn't represent Japanese language usages very well,
failing to contain Japanese eras, kanji numbers, wide numbers, etc.

=item L<DateTime::Format::Japanese>

This parses Japanese dates. Unlike the present module it claims to
also format them, so it can turn a L<DateTime> object into a Japanese
date, and it also does times. However, the module seems to be broken -
it doesn't install on any system I've tried.

=item L<Lingua::JA::Numbers>

This module has a very full set of kanji / numeral convertors. It
converts numbers including decimal points and numbers into the
billions and trillions.

=item L<DateTime::Calendar::Japanese::Era>

This module contains a full set of Japanese eras in romaji.

=back

=head1 COPYRIGHT AND LICENCE

Copyright (C) 2008 Ben Kasmin Bullock. All rights reserved.


This module is distributed under the same terms as Perl itself, either
Perl version 5.10.0 or, at your option, any later version of Perl 5
you may have available.

=cut

1;

__END__
