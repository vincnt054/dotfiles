#
# ~/.bash_profile
#
[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc

userresources=$HOME/.Xresources
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export PATH="$PATH:$(go env GOBIN):$(go env GOPATH)/bin"

export TERM="xterm"

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

setxkbmap -option caps:escape

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
