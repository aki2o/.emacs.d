#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use File::Basename;
use File::HomeDir;
use File::Path;
use Digest::MD5 qw{ md5_hex };
use List::AllUtils qw{ uniq  first  };

my $tagfile = File::HomeDir->my_home . "/ETAGS";
my $tagsdir = $ENV{ETAGSDIR};
if ( ! $tagsdir ) { $tagsdir = File::HomeDir->my_home . "/.etags.d"; }
if ( ! -d $tagsdir ) { mkdir $tagsdir; }
my @configlist = (File::HomeDir->my_home . "/.ctags");
my @excludelist = ();
my $do_cleanup = 0;
my $do_cleanup_all = 0;
my $is_recursive = 0;
my $suffix = "";
my $language = "";
my $usage = <<EOF;
Usage: 
EOF

GetOptions ('help' => sub { print $usage; exit 0; },
            'clean' => \$do_cleanup,
            'clean-all' => \$do_cleanup_all,
            'recursive' => \$is_recursive,
            'suffix=s' => \$suffix,
            'language=s' => \$language,
            );

if ( $do_cleanup_all ) { cleanup(); exit 0; }

my $targetdir = shift;
$suffix =~ s{ ^\. }{}xms;
if ( $suffix ne "" ) { $suffix = ".".$suffix; }
if ( $language eq "" ) { $language = "_DEFAULT_"; }
$tagsdir .= "/$language";
if ( $language ne "" ) { $tagfile .= ".$language"; }

if ( $do_cleanup ) { cleanup(); exit 0; }

make_excludelist();
CREATE_TAG:
foreach my $dir ( get_targetdirs($targetdir, 1) ) {
    create_tag($dir);
}
build_tagfile();

exit 0;


sub cleanup {
    DELETE_TAGFILE:
    foreach my $node ( glob "${tagfile}*" ) {
        if ( -f $node ) { unlink $node; }
    }
    if ( -d $tagsdir ) { rmtree($tagsdir); }
}
sub build_tagfile {
    if ( -f $tagfile ) { unlink $tagfile; }
    if ( -f $tagfile ) { return; }
    chdir $tagsdir or return;
    APPEND_TAGINFO:
    foreach my $file ( grep { -f $_ } glob "*" ) {
        open my $fh, '<', "$file" or return;
        binmode $fh;
        my $taginfo = do { local $/; <$fh> };
        close $fh or return;
        open $fh, '>>', "$tagfile" or return;
        binmode $fh;
        print $fh $taginfo;
        close $fh or return;
    }
}
sub create_tag {
    my $targetdir = shift || "";
    my $md5value = md5_hex($targetdir);
    if ( ! -d $tagsdir ) { mkdir $tagsdir; }
    chdir $targetdir or return;
    my $taginfo = qx{ etags -o - ${targetdir}/*${suffix} };
    open my $fh, '>', "$tagsdir/$md5value" or return;
    binmode $fh;
    print $fh $taginfo;
    close $fh or return;
}
sub get_targetdirs {
    my $targetdir = shift || "";
    my $is_root = shift || 0;
    my @ret = ();
    if ( ! -d $targetdir ) { return @ret; }
    if ( $is_root ) { push @ret, $targetdir; }
    if ( ! $is_recursive ) { return @ret; }
    chdir $targetdir or return @ret;
    my @excluded = get_excludelist_in($targetdir);
    CHK_VALID_DIR:
    foreach my $dir ( grep { -d $_ } glob "*" ) {
        if ( first { $_ eq $dir } @excluded ) { next CHK_VALID_DIR; }
        push @ret, "${targetdir}/$dir";
        push @ret, get_targetdirs("${targetdir}/$dir", 0);
    }
    return @ret;
}
sub make_excludelist {
    my $configfile = "";
    SEARCH_CONFIG:
    foreach my $file ( @configlist ) {
        if ( -f $file ) { $configfile = $file; last SEARCH_CONFIG; }
    }
    if ( ! -f $configfile ) { return @excludelist; }
    open my $fh, '<', "$configfile" or return;
    binmode $fh;
    GET_EXCLUDE_REGEX:
    while ( my $line = <$fh> ) {
        chomp $line;
        if ( $line !~ m{ ^--exclude=(.+)$ }xms ) { next GET_EXCLUDE_REGEX; }
        push @excludelist, qr{ $1 }xms;
    }
    close $fh or return;
}
sub get_excludelist_in {
    my $targetdir = shift;
    my @ret = ();
    APPEND_EXCLUDED:
    foreach my $globstr ( @excludelist ) {
        push @ret, glob "${targetdir}/$globstr";
    }
    return uniq(@ret);
}



