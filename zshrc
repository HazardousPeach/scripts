#!/bin/zsh

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

source ~/.profile

autoload -U compinit
compinit

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

setopt correctall

autoload -U promptinit
promptinit
prompt gentoo

export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups

setopt autocd
setopt extendedglob

unsetopt correct_all

set -o vi
alias ls='ls --color=auto'
bindkey "jk" vi-cmd-mode
bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^H" backward-delete-char
bindkey "^U" backward-kill-line
bindkey "^R" history-incremental-search-backward

export TERM=rxvt-unicode
export EDITOR=emacsclient
function pass() {
  if [ -n "$1" ]
  then
    ssh -t server pass $@ && ssh server pass $@ | xclip -selection clipboard
  else
    ssh server pass
  fi
}
function lab-monitor() {
    if xrandr | grep -q "HDMI1 connected" ; then
      xrandr --output HDMI1 --mode 1920x1080 --above eDP1 --primary
    elif xrandr | grep -q "^DP1 connected" ; then
      xrandr --output DP1 --mode 1920x1200 --above eDP1 --primary
    else
        echo "No lab monitor found!\n"
    fi
}
alias shutdown="sudo shutdown -h now"
alias sleep="sudo s2ram"
alias start-xmonad="WM=xmonad runx"
alias start-lxde="WM=startlxde runx"
function runx() {
    if `ps -ef | grep 'emacs' | grep -v 'grep' > /dev/null` ; then
       emacs --daemon &
       disown
    fi
    startx
}
[ -n "$XTERM_VERSION" ] && transset --id "$WINDOWID" .7

# added by travis gem
[ -f /home/alex/.travis/travis.sh ] && source /home/alex/.travis/travis.sh
