#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias
alias ls='ls -la --color=auto'
alias wget='wget --hsts-file="$XDG_DATA_HOME/wget-hsts'
alias dmenu_run="dmenu_run -nb '#000000'"
alias clipmenu="clipmenu -nb '#000000'"

# Power User
export PS1="[\[\e[0;31m\]\u\[\e[m\]@\[\e[0;34m\]\h\[\e[m\]] {\[\e[0;32m\]\w\[\e[m\]} \$ "

# Terminal
export TERM=xterm-256color

# Editor
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

# PATH
export PATH=$PATH:$HOME/.local/script

# XDG Base Directory
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}

export MYSQL_HISTFILE="$XDG_DATA_HOME"/mysql_history
export XDG_DESKTOP_DIR="-"
export PLTUSERHOME="$XDG_DATA_HOME"/racket
export LESSHISTFILE="-"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export DOT_SAGE="$XDG_CONFIG_HOME"/sage
export SSB_HOME="$XDG_DATA_HOME"/zoom
