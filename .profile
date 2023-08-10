alias ls='ls --color=auto'
alias vim='nvim'

export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"
export GOPATH=~/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin:$HOME/.local/bin:$GEM_HOME/bin"

export EDITOR="nvim"
export VISUAL="nvim"
export TERM="xterm"

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
