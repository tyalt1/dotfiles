#!/bin/bash

# assert you have apt-get
if [ -z $(command -v apt) ]; then
	echo 'Error: Could not find apt' >& 2 # Print error message into stderr
	exit 1
fi

# assert you are root user
if [ $(id -u) != 0 ]; then
	echo 'Error: Must be run as root.' >& 2
	exit 1
fi

# update and upgrade
apt update --yes --quiet
apt upgrade --yes --quiet
apt autoremove --yes --quiet

# install dependencies
apt install --yes --quiet build-essential bash gpg curl wget gawk

# install Go
apt install --yes --quiet golang

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

# install asdf (https://asdf-vm.com/guide/getting-started.html)
go install github.com/asdf-vm/asdf/cmd/asdf@v0.20.2
for plugin in python nodejs erlang elixir; do
	asdf plugin add $plugin
	# asdf install $plugin latest
done

# install opencode (https://opencode.ai/)
if [ -z $(command -v opencode) ]; then
	curl -fsSL https://opencode.ai/v2/install | bash
fi

# install hermes agent (https://hermes-agent.nousresearch.com/)
if [ -z $(command -v hermes) ]; then
	curl -fsSL https://hermes-agent.nousresearch.com/install.sh | bash
fi
