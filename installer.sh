#!/bin/bash

# assert you have apt-get
if [ -z $(command -v apt-get) ]; then
	echo 'Error: Could not find apt-get.' >& 2 # Print error message into stderr
	exit 1
fi

# assert you are root user
if [ $(id -u) != 0 ]; then
	echo 'Error: Must be run as root.' >& 2
	exit 1
fi

flags='--yes --quiet'
install="apt-get install $flags"

# update and upgrade
apt-get update $flags && apt-get upgrade $flags
apt-get autoremove $flags

# install dependencies
$install build-essential bash gpg curl wget gawk

# install Go
$install golang

# install Docker
if [ -z $(command -v docker) ]; then
	curl -fsSL 'https://get.docker.com/' | sh
	# TODO Add user to docker group with `sudo usermod -aG docker <user-here>`
fi

# install Rust
if [ -z $(command -v rustc) ]; then
	curl --proto '=https' --tlsv1.2 -sSf 'https://sh.rustup.rs' | sh
fi

# Git config
$install git
git config --global user.name 'Tyler Alterio'
git config --global user.email 'tyalt1@gmail.com'
git config --global push.default simple
git config --global init.defaultBranch main
git config --global alias.ls 'log --decorate --oneline --graph -15' #Alternative to git log
git config --global alias.s 'status --short' #Alternative to git status
git config --global alias.diffs 'diff --staged' #Alternative to git diff for staged files

# install asdf
go install github.com/asdf-vm/asdf/cmd/asdf@v0.20.2
for plugin in python nodejs erlang elixir; do
	asdf plugin add $plugin
	# asdf install $plugin latest
done
