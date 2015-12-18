#!/bin/bash

# customize these
WGET="/usr/bin/wget"
ICS2ORG="$HOME/.emacs.d/ical2org.awk"
ICSFILE="$HOME/.emacs.d/basic.ics"
ORGFILE="$HOME/.emacs.d/gcal.org"
URL="https://www.google.com/calendar/ical/ovmf902dab8a1nilj2l8hoh2os%40group.calendar.google.com/private-bca71e17a20f2b2599d46c337916f327/basic.ics"

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG < $ICSFILE > $ORGFILE

# CRON :
# 5,20,35,50 * * * * <path to above script> &> /dev/null #sync my org files

# link to doc page
# http://orgmode.org/worg/org-tutorials/org-google-sync.html
