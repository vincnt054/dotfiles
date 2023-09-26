#
# ~/.bash_profile
#
[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"

export TERM="xterm"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
