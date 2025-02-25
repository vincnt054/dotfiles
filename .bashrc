# .bashrc

[[ $- != *i* ]] && return

alias ls='/usr/bin/ls --color=auto'
alias emacs='/usr/bin/emacsclient -c -a ""'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias gobuster='/usr/bin/docker run --rm --network=host -v ~/Documents/SecLists:/tmp/wordlist ghcr.io/oj/gobuster'
alias metasploit='/usr/bin/docker run --rm -it -p 4444:4444 -p 80:80 -p 8080:8080 -p 443:443 -p 445:445 -p 8081:8081 strm/metasploit'
alias discord='flatpak run com.discordapp.Discord'
alias firefox='/usr/bin/firejail firefox'
alias okular='/usr/bin/firejail okular'
alias zathura='/usr/bin/firejail zathura'

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\[\033[33m\]\@\[\033[00m\] \[\033[34m\]\u@\h\[\033[00m\] \[\033[32m\]\w\[\033[00m\]\[\033[33m\]\$(parse_git_branch)\[\033[00m\] > "

export LESSHISTFILE=-
export CLICOLOR=TRUE
export GO111MODULE=auto
export MAN_POSIXLY_CORRECT=1

. "$HOME/.cargo/env"

set -o vi
# Install Ruby Gems to ~/gems
export GEM_HOME="$HOME/gems"
export PATH="$HOME/gems/bin:$PATH"
