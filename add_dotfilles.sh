#!/bin/bash

function add_dotfile {
  from="./$1";
  to="$HOME/.$1";
  mkdir -p `dirname $to`;
  cp $from $to
}

add_dotfile bash_aliases
add_dotfile i3/config
add_dotfile i3status.conf
