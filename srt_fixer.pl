#!/usr/bin/perl

while(@ARGV)
{
    $a = $ARGV[0];

    if($a eq "-srt1")
    {
	shift @ARGV;
	$srt1 = shift @ARGV;;
    }

    elsif($a eq "-fs")
    {
	shift @ARGV;
	$fs = shift @ARGV;;
    }

    elsif($a eq "-srt2")
    {
	shift @ARGV;
	$srt2 = shift @ARGV;;
    }

    elsif($a eq "-video_delay1")
    {
	shift @ARGV;
	$vd1 = shift @ARGV;;
    }

    elsif($a eq "-video_delay2")
    {
	shift @ARGV;
	$vd2 = shift @ARGV;;
    }
    elsif($a eq "-vt1")
    {
	shift @ARGV;
	$vt1 = shift @ARGV;;
    }

    elsif($a eq "-vt2")
    {
	shift @ARGV;
	$vt2 = shift @ARGV;;
    }
    elsif($a eq "-m")
    {
	shift @ARGV;
	$m = shift @ARGV;;
    }
    elsif($a eq "-k")
    {
	shift @ARGV;
	$k = shift @ARGV;;
    }
    elsif($a eq "-pin")
    {
	shift @ARGV;
	$pin = shift @ARGV;;
    }
    else {
	last; }
}

if((!defined $srt1 || !defined $srt2 || 
	     (!defined $vd1 || !defined $vd2) &&
	     (!defined $vt1 || !defined $vt2))
   && (!defined $m || (!defined $k && !defined $pin)))
{
    print "Usage: $0 -fs <framespeed (only needed for MicroDvd subs) -srt1 <srt first time hh:mm:ss.sub, mm:ss.sub, ss.sub, etc.> \n";
    print "   -srt2 <srt second time mm:ss or ss>\n";
    print "   -video_delay1 <video delay 1 mm:ss or ss>\n";
    print "   -video_delay2 <video delay 2 mm:ss or ss>\n";
    print "   -vt1 <video time 1 mm:ss or ss>\n";
    print "   -vt2 <video time 2 mm:ss or ss>\n";
    print "or -m <multiplier> -k <addition>\n";
    exit 1;
}

foreach $v (\$srt1, \$srt2, \$vd1, \$vd2, \$vt1, \$vt2, \$pin, \$fs)
{
    if($$v =~ /(-?)([0-9]{1,3}):([0-9]{1,3}):([0-9]{2})([.,]([0-9]*))?/)
    {
	$$v = $2 * 3600 + $3 * 60 + $4 + (".".$6);
	if($1 eq "-")
	{
	    $$v = - $$v;
	}
    }
    if($$v =~ /(-?)([0-9]{1,3}):([0-9]{2})([.,]([0-9]*))?/)
    {
	$$v = $2 * 60 + $3 + (".".$5);
	if($1 eq "-")
	{
	    $$v = - $$v;
	}
    }
}

if(!defined $vt1)
{
    $vt1 = $srt1 + $vd1;
    $vt2 = $srt2 + $vd2;
}

if(!defined $m)
{
    $m = -(-$vt1 + $vt2)/($srt1 - $srt2);
    $k = -($srt2 * $vt1 - $srt1 * $vt2)/($srt1 - $srt2);
}

if(!defined $k)
{
    print STDERR "Adjusting: m = $m, pin = $pin secs\n";
}
else
{
    print STDERR "Adjusting: m = $m, k = $k secs\n";
}

if($m < 0) 
{
    die "m is less than zero?";
}
    
my $mode;
while(<>)
{
    $t1 = $t2 = undef;

    if($_ =~ /([0-9]{2}):([0-9]{2}):([0-9]{2}),([0-9]{3}) --> ([0-9]{2}):([0-9]{2}):([0-9]{2}),([0-9]{3})/)
    {
	$t1 = $1 * 60 * 60 + $2 * 60 + $3 + $4 / 1000.;
	$t2 = $5 * 60 * 60 + $6 * 60 + $7 + $8 / 1000.;
	$mode = 0;
    }
    #{3569}{3654}Command station, this is ST-321.|Code clearance blue.
    elsif($_ =~ /^\{([0-9]+)\}\{([0-9]+)\}(.*)/)
    {
        die "Must specify framespeed!" if !defined $fs;
	$t1 = $1 / $fs;
	$t2 = $2 / $fs;
	$mode = 1;
	$msg = $3;
    }

#	print "$t1, $t2\n";
    if(defined $t1)
    {
	if(!defined $k)
	{
	    $k = ($pin / $m - $t1) * $m;
	    print STDERR "Adjusting: k = $k secs\n";
	}

	$t1 = $t1 * $m + $k;
	$t2 = $t2 * $m + $k;

	use POSIX;
#	print "$t1, $t2\n";

	if($mode == 0)
	{
	    printf("%02d:%02d:%02d,%03d --> %02d:%02d:%02d,%03d\r\n",
		   POSIX::floor($t1 / 3600), 
		   POSIX::floor($t1 % 3600 / 60), 
		   POSIX::floor($t1 % 60), 
		   POSIX::floor($t1 * 1000 % 1000), 
		   POSIX::floor($t2 / 3600), 
		   POSIX::floor($t2 % 3600 / 60), 
		   POSIX::floor($t2 % 60), 
		   POSIX::floor($t2 * 1000 % 1000));
	}
	else
	{
	    printf("{%d}{%d}%s\n",$t1*$fs,$t2*$fs,$msg);
	}
    }
    elsif($_ =~ /^[0-9]+$/)
    {
	print ++$i."\r\n";
    }
    else
    {
	print $_;
    }
}

