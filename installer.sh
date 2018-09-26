#!/bin/bash

# Assert you have apt-get
if [ -z $(command -v apt-get) ]; then
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

# Assert you are root user
if [ $(id -u) != 0 ]; then
	echo 'Error: Must be run as root.' >& 2
	exit 1
fi

flags='--yes --quiet'
install="apt-get install $flags"
ATOMREPO=ppa:webupd8team/atom
DOCKERURL=https://get.docker.com/

# Add repositories
add-apt-repository --yes ATOMREPO
# add-apt-repository --yes ppa:libretro/stable NOTE emulator

#Update and Upgrade
apt-get update $flags && apt-get upgrade $flags
apt-get autoremove $flags

# Basics
$install wget curl

# Media/Games
$install vlc
$install chromium-browser
# $install retroarch retroarch-* libretro-* NOTE emulator

# Languages
$install build-essential #gcc, g++, make, and some libs
$install python{,3} idle{,3} python-pip #Python
pip install --upgrade pip
$install erlang elixir #Erlang/Elixir
$install haskell-platform

# Development
$install i3 #tiling window manager
$install tree #ls alternative
$install terminator #terminal emulator
$install vim
$install texlive #LaTeX

# Docker
if [ -z $(command -v docker) ]; then
	# TODO Add user to docker group with `sudo usermod -aG docker <user-here>`
	wget -qO- $DOCKERURL | sh
fi

# Atom
$install atom
apm install minimap pdf-view todo script #Utility
apm install merge-conflicts git-time-machine #git
amp install atom-paredit linter-gcc
apm install language-{c,clojure,python,erlang,elixir,haskell} #Lang
apm install autocomplete-{python,erlang} atom-elixir #Autocomplete
apm install language-{arduino,docker,doxygen,latex,llvm} #DSL

# Git
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global core.editor vim
git config --global push.default simple
git config --global alias.ls 'log --decorate --oneline --graph -15' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.diffs 'diff --staged'
git config --global alias.edit 'config --global --edit' #Edit config page in default editor
git config --global alias.restart 'reset --hard' #Resets to last commit
git config --global alias.rewind 'reset HEAD~' #Undoes last commit
git config --global alias.root 'rev-parse --show-toplevel' #Path to Git Repo, Ex: cd $(git root)
