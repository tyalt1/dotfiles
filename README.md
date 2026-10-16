# dotfiles

```bash
bash installer.sh
```

Running this script installs
- Some essentials (curl, wget, gpg, and more)
- Go
- Docker (via get.docker.com)
- Rust (via sh.rustup.rs)
- Git (and git config)
- asdf (via go)
- Python, Node.js, Erlang, Elixir (all via asdf)

## addtional manual steps

For Docker install, you need to create user group
```bash
sudo usermod -aG docker $USER
```

### `asdf` (https://asdf-vm.com/guide/getting-started.html)
For `asdf` install, add this to `.bashrc` for path and bash completion
```bash
# asdf
## add to path
export ASDF_DATA_DIR="$HOME/.asdf"
export PATH="$ASDF_DATA_DIR/shims:$PATH"

## bash completion 
source <(asdf completion bash)
```
