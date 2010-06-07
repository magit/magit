#!/usr/bin/env perl

# :-)

(/(\d{4}) .*--(.+)/ && $aut{$2}->{$1}++) for(qx(git log --pretty='format: %aD--%an'));
print "Copyright (C) ".join(", ", sort keys %{$aut{$_}})." $_.\n" for(sort keys %aut);
