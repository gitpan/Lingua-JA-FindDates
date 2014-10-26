
use warnings;
use strict;
use utf8;
use Test::More tests => 31;

BEGIN { use_ok('Lingua::JA::FindDates') };

use Lingua::JA::FindDates qw/kanji2number subsjdate/;

#$JaDates::verbose = 1;

# If you are on Cygwin on Japanese Windows, use the following:

#binmode STDOUT,":encoding(cp932)";
#binmode STDERR,":encoding(cp932)";
binmode STDOUT,":utf8";
binmode STDERR,":utf8";

#print '百百三十五', "\n";
#print JaDates::kanji2number ('3百三十五');
ok (kanji2number ('3百三十五') == 0, 'bad kanji number failure test');
ok (kanji2number ('二百三十五') == 235, 'kanji number');
ok (kanji2number ('二三五') == 235, 'kanji number');
ok (kanji2number ('二三五五') == 2355, 'kanji number');

my @tests= qw/平成２０年７月３日（木） H二十年七月三日(木曜日) 二千八年7月三日(木曜)/;
for my $d (@tests) {
#    print subsjdate ($d);
    ok (subsjdate ($d) eq 'Thursday, July 3, 2008', 
	'year + month + day + weekday');
}

sub mymakedate
{
        my ($year, $month, $date, $wday) = @_;
	return qw{Bad Mo Tu We Th Fr Sa Su}[$wday]." $year/$month/$date";
}

for my $d (@tests) {
#    print subsjdate ($d);
    ok (subsjdate ($d, undef, undef, \&mymakedate)
	eq 'Th 2008/7/3', 'makedate_callback');
}

my @tests2= ('昭和 ４１年　３月１６日', 'Ｓ４１年三月十六日', '千九百六十六年3月16日');
for my $c (@tests2) {
#    print "$d, [>>",subsjdate ($d),"<<]\n";
    ok (subsjdate ($c) eq 'March 16, 1966', 'year + month + day combination');
}
my @tests3= ('昭和 ４１年', 'Ｓ４１年', '千九百六十六年');
for my $c (@tests3) {
#    print "$c, [>>",subsjdate ($c),"<<]\n";
    ok (subsjdate ($c) eq '1966', 'year combination');
}
my @tests4= ('３ 月 １６日', '三月十六日', '3月16日');
for my $c (@tests4) {
#    print "$d, [>>",subsjdate ($d),"<<]\n";
    ok (subsjdate ($c) eq 'March 16', 'month + day combination');
}

my $test5 =<<EOF;
2008年07月03日 01:39:21 投稿
【西村修平・桜井誠編】平成20年7月2日毎日新聞社前抗議行動！
最低賃金法が大きく改正され、平成２０年７月１日から施行されました。
平成二十年七月四日
Opera 9.51(Windows版)を入れてみたけれども相變らず使ひ物にならない。何なんだらう。
日本義塾 平成二十年七月公開講義
公開日時：2008年06月28日 01時09分
更新日時：2008年06月29日 00時08分

◎【日本義塾七月公開講義】

　◎日　時　平成二十年七月二十五日（金）
　　　　　　午後六時半～九時（六時開場）
H20年7月壁紙
7月壁紙（1024×768）を作成しました（クリックすると拡大します）。
EOF

my %jdates = 
(
'2008年07月03日' =>'July 3, 2008',
'平成20年7月2日' =>'July 2, 2008',
'平成２０年７月１日' =>'July 1, 2008',
'平成二十年七月四日' =>'July 4, 2008',
'平成二十年七月' =>'July 2008',
'2008年06月28日' =>'June 28, 2008',
'2008年06月29日' =>'June 29, 2008',
'平成二十年七月二十五日（金）' =>'Friday, July 25, 2008',
'H20年7月' => 'July 2008',
'7月' => 'July',
'七月' => 'July',
);

sub replace_callback
{
    my ($data, $jdate, $edate) = @_;
#    print "[$jdate] [$edate] [$jdates{$jdate}]\n";
    ok ($jdates{$jdate} eq $edate, "replace_callback");
}

subsjdate ($test5, \&replace_callback);

