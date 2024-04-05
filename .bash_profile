#
# ~/.bash_profile
#
#NPM_PACKAGES="${HOME}/.npm-packages"

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:/usr/local/bin:$(go env GOBIN):$(go env GOPATH)/bin:$HOME/.local/bin:$HOME/.cargo/bin"
export TERMINAL=/usr/bin/urxvt
export PASSWORD_STORE_X_SELECTION=clipboard

# export PATH="$PATH:$NPM_PACKAGES/bin"
# export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

# Doom emacs
export PATH="$PATH:$HOME/.emacs.d/bin"

alias poweroff='systemctl poweroff'
alias reboot='systemctl reboot'

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
