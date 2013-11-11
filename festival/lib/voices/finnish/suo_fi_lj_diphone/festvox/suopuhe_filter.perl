#!/usr/bin/perl -w
use strict;
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;;                                                                       ;;
#;;          Department of General Linguistics / Suopuhe project          ;;
#;;                      University of Helsinki, FI                       ;;
#;;                  Copyright (c) 2000,2001,2002,2003                      ;;
#;;                        All Rights Reserved.                           ;;
#;;                                                                       ;;
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#; This program is distributed under Gnu Lesser General Public License (cf. the
#; file LICENSE in distribution).
 
#; This program is free software; you can redistribute it and/or modify
#; it under the terms of the GNU Lesser General Public License as published by
#; the Free Software Foundation; either version 2 of the License, or
#; (at your option) any later version.
 
#; This program is distributed in the hope that it will be useful,
#; but WITHOUT ANY WARRANTY; without even the implied warranty of
#; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#; GNU Lesser General Public License for more details.

# suopuheen BASH-filtteri
# syötteen tulisi olla lause/rivi -muotoista 

# Tue Sep  9 11:23:05 EEST 2003
# -Added of forgotten 'g': s/// => s///g which caused list intonation to
# remove wrong words

# Fri Oct 26 12:04:40 EEST 2001 
# -Added partial sayas-replacement already here, because of Festival
#  (rxp-parser?) bug.

# read the whole input into $_
undef $/;
$_ = <>;

# kommentit pois 
s/<!\-\-.*?\-\->//gs;
s/\s+/ /gs;

# XML-parserin bugeja:
s/ original=\"<\"//g;
s/ original=\"\&\"//g;

#> yksi kaksi <# muuttuu muotoon #> yksi-kaksi <# 
# (korjaa raa'asti festarin token-mokan):
while ( s/> ([A-Za-zåäö\-]+) ([A-Za-zåäö])/> $1-$2/ ) {} 
# tyhjät pois:
s/<token( +[a-z]+=\"[^\"]*\")*> +<\/token>\s*//g;


# allow phrase and break only in mid positon of token
while ( s/(<break\/>)\s*(<\/token>)/$2 $1/ ||
	s/(<phrase\/>)\s*(<\/token>)/$2 $1/ ) {}

while ( s/(<token( +[a-z]+=\"[^\"]*\")*>)\s+(<(break|phrase)\/>)/$3 $1/g ) {}

# eliminate <break/> <phrase\/> sequences: the first one wins

s/(<(break|phrase)\/>)( <(break|phrase)\/>)*/$1/g;






s/> />\n/g;
s/ </\n</g;

print $_;
