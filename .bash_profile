#
# ~/.bash_profile
#
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH="$PATH:/usr/local/bin"
export TERMINAL=/usr/bin/urxvt
export PASSWORD_STORE_X_SELECTION=clipboard

# Rust
export PATH="$PATH:$HOME/.cargo/bin"
[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# GO
export PATH="$PATH:$(go env GOBIN)"

# NPM
# export PATH="$PATH:$NPM_PACKAGES/bin"
# export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

# Python
# export PATH="$PATH:$HOME/.python_venv/bin"
# [[ -f "$HOME/.python_venv/bin/activate" ]] && . "$HOME/.python_venv/bin/activate"

# Doom emacs
export PATH="$PATH:$HOME/.config/emacs/bin"

# bashrc
[[ -f "$HOME/.bashrc" ]] && . "$HOME/.bashrc"

# Default for bash history
shopt -s histappend
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL=ignoredups
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Personal default
export VIDEO="$HOME/Videos"
export SCREENSHOT="$HOME/Pictures/screenshots"

# Fzf bindings
source /usr/share/doc/fzf/examples/key-bindings.bash
source /usr/share/bash-completion/completions/fzf
