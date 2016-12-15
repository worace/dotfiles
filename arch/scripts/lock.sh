#!/bin/bash
# courtesy https://github.com/rokyfox/i3ConfigFiles

import -silent -window root /tmp/lock.png;
notify-send --urgency low "Locking..."
convert /tmp/lock.png -scale 25% -blur 0x20 -scale 400% /tmp/lock.png;
composite -gravity center $HOME/dotfiles/arch/scripts/pad.png /tmp/lock.png /tmp/lock.png;

i3lock -i /tmp/lock.png --textcolor=eee8d5ff --ringcolor=07364295 --keyhlcolor=268bd295 --bshlcolor=dc322f95 --insidecolor=002b3695 --insidevercolor=268bd295 --ringvercolor=268bd295 --insidewrongcolor=dc322f95 --ringwrongcolor=dc322f95 --linecolor=07364295
