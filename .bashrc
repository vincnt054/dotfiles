# .bashrc

[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias emacs='emacsclient -c -a "emacs"'
alias pass_show='pass show -c'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias vim='nvim'
alias gobuster='docker run --rm --network=host -v ~/documents/SecLists:/tmp/wordlist ghcr.io/oj/gobuster'
alias metasploit='docker run --rm -it -p 4444:4444 -p 80:80 -p 8080:8080 -p 443:443 -p 445:445 -p 8081:8081 strm/metasploit'

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\[\033[32m\]\@ \u@\h \w\[\033[33m\]\$(parse_git_branch)\[\033[00m\]\n> "
export LESSHISTFILE=-
export CLICOLOR=TRUE
export GO111MODULE=on

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash

set -o vi
