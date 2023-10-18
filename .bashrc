# .bashrc

[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias emacs='emacsclient -c -a "emacs"'
alias pass_show='pass show -c'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\[\033[32m\]\@ \w\[\033[33m\]\$(parse_git_branch)\[\033[00m\]\n> "
export LESSHISTFILE=-
export CLICOLOR=TRUE
export TERM="xterm-256color"
export GO111MODULE=on

export PASSWORD_STORE_X_SELECTION=primary

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash

set -o vi
