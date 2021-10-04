#!/usr/bin/env perl -w

# create_bookmarks.pl <outfile>
# Create a large bookmarks file.

use strict;
no warnings qw(experimental::signatures);

use feature 'say';
use feature 'signatures';

use JSON;
use Data::Random qw(:all);
use Data::Random::Structure::UTF8;

my $max_depth = 6;
my $filename  = $ARGV[0] or die();

my $randomiser = Data::Random::Structure::UTF8->new();

sub rint ( $min, $max ) { return int( rand( $max - $min + 1 ) ) + $min }

sub rchars() {
    return "" . rand_chars( set => 'all', min => 5, max => 10 ) =~ s/"//rg;
}

sub rchars_url() {

    return "" . rand_chars( set => 'alphanumeric', min => 3, max => 10 );
}
sub rchars_u() { return $randomiser->random_chars_UTF8( {} ) =~ s/"//rg }
sub rname()    { return rchars() . " " . rchars_u() }

sub rnumber() {
    return "" . rand_chars( set => 'numeric', min => 1, max => 10 );
}

sub rurl() {
    return
        "https://"
      . rchars_url()
      . ".com/a/"
      . rchars_url()
      . ".html?"
      . rchars_url . "="
      . rchars_u();
}

sub str ($str) { return "$str" }

my $nr_of_bookmarks;
my $nr_of_folders;
my $nr_of_icons;

sub create_bookmark() {
    $nr_of_bookmarks++;
    my $bm = {
        date_added => rnumber(),
        id         => rnumber(),
        name       => rname(),
        type       => "url",
        url        => rurl()
    };
    # if ( rint( 0, 4 ) == 0 && $nr_of_icons++ < 5000 ) {
    if (0) {
        $bm->{meta_info} = {
            Thumbnail => "data:image/jpg;base64,/"
              . (
                rand_chars( set => 'alphanumeric', min => 50, max => 50 ) x 100
              )
        };
    }

    return $bm;
}

sub create_folder ( $depth = 0, $name = rname() ) {
    $nr_of_folders++;
    return {
        date_added => rnumber(),
        id         => rnumber(),
        name       => $name,
        type       => "folder",
        children   => create_items( $depth + 1 )
    };
}

sub create_items ($depth) {
    my @items = ();
    for ( my $i = 0 ; $i < rint( 5, 10 ) ; $i++ ) {
        my $item;
        if ( $depth >= $max_depth || rint( 0, 1 ) == 0 ) {
            $item = create_bookmark();
        }
        else {
            $item = create_folder($depth);
        }
        push( @items, $item );
    }
    return \@items;
}

my $bookmarks = {
    "checksum" => "4d98050fc17ec36df4490e5b783cd33d",
    "version"  => 1,
    "roots"    => {
        "bookmarks_bar" => create_folder( 0, "Bookmarks bar" ),
        "other"         => create_folder( 0, "Other bookmarks" ),
        "synced"        => create_folder( 0, "Mobile bookmarks" ),
    },
    "nr_of_bookmarks" => $nr_of_bookmarks,
    "nr_of_folders"   => $nr_of_folders
};

my $folders_outside_trash = $nr_of_folders;
my $bms_outside_trash     = $nr_of_bookmarks;
$nr_of_folders   = 0;
$nr_of_bookmarks = 0;

$bookmarks->{roots}->{"trash"} = create_folder( 0, "Trash" );

open( my $file, ">", $filename ) or die($!);

print $file to_json( $bookmarks, { pretty => 1, utf8 => 1 } );

say(
"Outside trash: $folders_outside_trash folders, $bms_outside_trash bookmarks."
);
say("Inside trash: $nr_of_folders folders, $nr_of_bookmarks bookmarks.");
say(    "Total: "
      . ( $folders_outside_trash + $nr_of_folders )
      . " folders, "
      . ( $bms_outside_trash + $nr_of_bookmarks )
      . " bookmarks." );
