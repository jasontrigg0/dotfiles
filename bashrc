# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=50000
HISTFILESIZE=50000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi


function cl() { if $(test $# -gt 0); then cd $1; fi; if $(test $(ls | wc -l) -lt 200); then ls; fi; }
# alias .="cl .."
# alias ..="cl ../.."
# alias ...="cl ../../.."
alias sudo="sudo " #without this aliases aren't available when using sudo
alias l="less"
alias emacs="emacsclient --alternate-editor= -t"
export EDITOR="emacsclient --alternate-editor= -t"
# export GOPATH=$HOME/go
# export ANDROID_HOME=$HOME/Files/android_dev/android-sdk-linux
export PATH=$PATH:$HOME/Dropbox/misc_code/utils:$HOME/Dropbox/misc_code/utils/R:$GOPATH/bin:$ANDROID_HOME/platforms:$ANDROID_HOME/tools:$HOME/Files/play
export PYTHONPATH=$PYTHONPATH:$HOME/Dropbox/misc_code/utils

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm


function mic() { amixer set Capture toggle; }
function b() { sudo bash -c "echo $1 > /sys/class/backlight/intel_backlight/brightness"; }
function h() { grep -a $1 ~/.bash_history | tail; }
function kt() { pkill -9 -P $$; } #kill all children of this terminal
function ff() { ls -ltu $(find . -name '*'$1'*') | awk '{print $9}'; } #files with this name under this directory
function ff1() { ls -ltu $(find . -name '*'$1'*') | awk '{print $9}' | head -1; } #find most recently updated file with this name under this directory
function gf() { grep -r -l -i $1 .; } #recursive grep for files containing the string in the current directory
function xsh() { cat - | tr '\n' '\0' | xargs -0 -n1 bash -c; } #run each line of /dev/stdin in bash
function ew() { emacs $(which $1); }
function echoerr() { echo "$@" 1>&2; }

#git shortcuts
function gl() { git diff HEAD~$1 HEAD; }
function gp() { git stash; git pull --rebase; git stash apply; }
function gg() { git log -p -S$1; }

#startup mysql shortcuts
function dbraw() { mysql -B -h pants-me.com -e "$1" | pawk -d '\t'; }
function db() { dbraw "$1" | plook; }
function dbh() { dbraw "select * from "$1" limit 10" | plook; }
function dbt() { dbraw "$1" | ptr | plook -a -n; }
function dbwhere() { db "select * from start.amazon_products where $1"; }
function get() { dbt 'select * from start.amazon_products where '$1' = "'$2'"'; }
function asin() { dbt 'select * from start.amazon_products where asin = "'$1'"'; }
function sibling_asin() { db 'select * from start.amazon_products where parent_asin = (select parent_asin from start.amazon_products where asin = "'$1'")'; }
function ms() { cd $HOME/github/mysize_shopping; }

###
#write to bash_history after each command!
shopt -s histappend                      # append to history, don't overwrite it
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
###


#
alias icbm='python tools/icbm/build.py'
alias play='thirdparty/play/play'
function bplay() {
  echo -e "\033];$1\007" && icbm :$1_dev && thirdparty/play/play test src/com/start/$1 ${@:2}
}

function bplay_prod() {
    echo -e "\033];$1\007" && icbm :$1_deploy && thirdparty/play/play run src/com/start/$1 --%prod ${@:2}
}
