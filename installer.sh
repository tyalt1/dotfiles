#!/bin/bash

#Resolve package manager
if [ -z $(command -v apt-get) ]; then
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

#Resolve root access
if [ $(id -u) != 0 ]; then
	echo 'Error: Must be run as root.' >& 2
	exit 1
fi

flags='--yes --quiet'
install="apt-get install $flags"

LEINURL=https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
ERLANGURL=https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
DOCKERURL=https://get.docker.com/
REBARURL=https://s3.amazonaws.com/rebar3/rebar3

#Add repositories
add-apt-repository --yes ppa:webupd8team/java #will still ask for yes/no to install
add-apt-repository --yes ppa:webupd8team/atom
add-apt-repository --yes ppa:libretro/stable

#Erlang Solution repository
wget -O /tmp/erlang-solutions.deb $ERLANGURL && dpkg -i /tmp/erlang-solutions.deb

#Update and Upgrade
apt-get update $flags && apt-get upgrade $flags
apt-get autoremove $flags

#Media/Games
$install vlc
$install chromium-browser
# $install retroarch retroarch-* libretro-* #Emulation

#Languages
$install build-essential #gcc, g++, make, and some libs
$install oracle-java8-installer #Java
$install python{,3} idle{,3} python-pip #Python
pip install -U pip
$install perl
$install haskell-platform
wget -O /bin/lein $LEINURL && chmod +x /bin/lein #Leiningen build of Clojure
$install esl-erlang elixir #Erlang/Elixir (from Erlang Solutions)
$install nodejs{,-legacy} npm #Node.js and npm
npm install -g npm
npm install -g elm

#Development
$install i3 #tiling window manager
$install tree #ls alternative
$install terminator #terminal emulator
$install vim
$install filezilla
$install texlive #LaTeX
$install doxygen doxygen-doc doxygen-gui #Doxygen, Docs, and Doxywizard
$install arduino qtcreator codeblocks #IDEs
if [ -z $(command -v docker) ]; then
	#Install Docker if not already installed.
	#Add user to docker group with `sudo usermod -aG docker <user-here>`
	wget -qO- $DOCKERURL | sh
fi
wget -O /bin/rebar3 $REBARURL && chmod +x /bin/rebar3 #Erlang build tool

#Atom
$install atom
apm install minimap pdf-view todo script #Utility
apm install merge-conflicts git-time-machine #git
amp install atom-paredit linter-gcc
apm install language-{c,clojure,python,erlang,elixir,haskell,elm} #Lang
apm install autocomplete-{python,erlang} atom-elixir #Autocomplete
apm install language-{arduino,docker,doxygen,latex,llvm} #DSL

#Git
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global core.editor vim
git config --global push.default current
git config --global alias.ls 'log --decorate --oneline --graph -15' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.diffs 'diff --staged'
git config --global alias.edit 'config --global --edit' #Edit config page in default editor
git config --global alias.restart 'reset --hard' #Resets to last commit
git config --global alias.rewind 'reset HEAD~' #Undoes last commit
git config --global alias.root 'rev-parse --show-toplevel' #Path to Git Repo, Ex: cd $(git root)
