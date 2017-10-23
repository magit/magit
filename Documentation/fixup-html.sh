#!/bin/sh

CSS='/<link rel="stylesheet" type="text\/css" href="\/assets\/page.css">/a\
<link class="s-css-s--style" rel="stylesheet"           title="Default"               href="/assets/themes/default.css">\
<link class="s-css-s--style" rel="stylesheet alternate" title="Default high contrast" href="/assets/themes/default-high-contrast.css">\
<link class="s-css-s--style" rel="stylesheet alternate" title="Solarized dark xterm"  href="/assets/themes/solarized-dark-xterm.css">\
<link class="s-css-s--style" rel="stylesheet alternate" title="Black on white"        href="/assets/themes/black-on-white.css">\
<script src="/assets/js/simple-css-switch.js"></script>'

ONLOAD='s/<body lang="en">/<body lang="en" onload="simpleCssSwitch()">/'

MENU='/<\/body>/i<div id="s-css-s--menu"><\/div>'

if test -d "$1"
then
    for f in $(find $1 -name '*.html')
    do
        sed -i -e "$CSS" -e "$ONLOAD" -e "$MENU" $f
    done
else
    sed -i -e "$CSS" -e "$ONLOAD" -e "$MENU" $1
fi
