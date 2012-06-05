#!/usr/bin/perl

while(<>){
    if( ! /x([0-9\.]+) \(original [0-9\.]+, planck ([0-9\.]+)\)/ ){ next; }
    my $mag = $1;
    my $tim = $2;
    $_ = <>;
    # Lexer: Bind total 350006, wasted 176, ratio 0.000503
    $_ = <>;
    # Parser: Bind total 716270, wasted 687644, ratio 0.960035
    /Parser: Bind total ([0-9]+), wasted [0-9]+, ratio ([0-9\.]+)/;
    my $binds = $1;
    my $ratio = $2;
    print "$mag, $tim, $binds, $ratio\n";
}
