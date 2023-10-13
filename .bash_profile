#
# ~/.bash_profile
#
[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin:$HOME/.local/bin:$HOME/.config/emacs/bin"
