# Dockerfile for developer environment image
FROM debian

# Install packages
RUN apt-get update && \
  apt-get install --yes \
  build-essential sudo tree wget vim rsync \
  python python3 python-pip \
  erlang elixir

# Make Aliases
RUN alias upgrade='apt-get update && apt-get upgrade -y'

# Install/Config git
RUN apt-get install --yes --quiet git; \
  git config --global user.name 'Tyler Alterio'; \
  git config --global user.email 'tyalt1@gmail.com'; \
  git config --global core.editor vim; \
  git config --global push.default current; \
  git config --global alias.ls 'log --decorate --oneline --graph -15'; \
  git config --global alias.s 'status --short'; \
  git config --global alias.diffs 'diff --staged'; \
  git config --global alias.edit 'config --global --edit'; \
  git config --global alias.restart 'reset --hard'; \
  git config --global alias.rewind 'reset HEAD~'; \
  git config --global alias.root 'rev-parse --show-toplevel'; \
