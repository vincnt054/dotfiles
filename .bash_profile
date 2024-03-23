#
# ~/.bash_profile
#
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:/usr/local/bin:$(go env GOBIN):$(go env GOPATH)/bin:$HOME/.local/bin"
export TERMINAL=/usr/bin/urxvt
export PASSWORD_STORE_X_SELECTION=clipboard

alias poweroff='systemctl poweroff'
alias reboot='systemctl reboot'

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

source /usr/share/bash-completion/completions/fzf-key-bindings
source ~/.local/share/bash-completion/completions/_ans-connect_completion.bash
source ~/.local/share/bash-completion/completions/base16-fzf.config
