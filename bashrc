# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
#turn this off so ssh sessions can use the bashrc
# case $- in
#     *i*) ;;
#       *) return;;
# esac

#use trap DEBUG below instead of PROMPT_COMMAND or else the show-last-command-in-tab-name functionality breaks


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

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

#color definitions
#https://unix.stackexchange.com/a/140618
COLOR_RESTORE=$(echo -e '\033[0m') # No Color
COLOR_WHITE=$(echo -e '\e[1;37m')
COLOR_BLACK=$(echo -e '\e[0;30m')
COLOR_BLUE=$(echo -e '\e[0;34m')
COLOR_LIGHT_BLUE=$(echo -e '\e[1;34m')
COLOR_GREEN=$(echo -e '\e[0;32m')
COLOR_LIGHT_GREEN=$(echo -e '\e[1;32m')
COLOR_CYAN=$(echo -e '\e[0;36m')
COLOR_LIGHT_CYAN=$(echo -e '\e[1;36m')
COLOR_RED=$(echo -e '\033[0;31m')
COLOR_LIGHT_RED=$(echo -e '\e[1;31m')
COLOR_PURPLE=$(echo -e '\e[0;35m')
COLOR_LIGHT_PURPLE=$(echo -e '\e[1;35m')
COLOR_BROWN=$(echo -e '\e[0;33m')
COLOR_YELLOW=$(echo -e '\e[1;33m')
COLOR_GRAY=$(echo -e '\e[0;30m')
COLOR_LIGHT_GRAY=$(echo -e '\e[0;37m')

if [ "$color_prompt" = yes ]; then
    if [ ! -z "$SSH_CONNECTION" ]; then
        PS1='${debian_chroot:+($debian_chroot)}${COLOR_RED}\u@\h${COLOR_RESTORE}:${COLOR_RED}\w${COLOR_RESTORE}\$ ' #show all red in SSH connection
    else
        PS1='${debian_chroot:+($debian_chroot)}${COLOR_LIGHT_GREEN}\u@\h${COLOR_RESTORE}:${COLOR_LIGHT_BLUE}\w${COLOR_RESTORE}\$ '
    fi
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

#If this is an xterm set the title to user@host:dir
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


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

#load private info like email, phone, etc (things I don't want to put on github)
if [ -f ~/.bash_private ]; then
    source ~/.bash_private
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


###########################


#####
# bash history setup
#####
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth


###
#PROMPT_COMMAND is run after each command:
###
#write to bash_history after each command, also reload bash_history
export PROMPT_COMMAND='history -a; history -c; history -r; promptFunc'
#history -a: append from this tab's history to the history file
#history -c: clear this tab's history
#history -r: read the history file as this tab's history (ie sync with other tabs)
#promptFunc: Jeff Kaufman's function to log and save command (see below)

# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# set .bash_history to a long length
HISTSIZE=250000
HISTFILESIZE=250000

# https://www.jefftk.com/p/you-should-be-logging-shell-history
# add logging information and write to .full_history
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date +%Y-%m-%d--%H-%M-%S) $(hostname) $PWD $(history 1)" \
         >> ~/.full_history
}


# function h () {
#     #https://gist.github.com/redguardtoo/01868d7a13817c9845e8#file-bashrc
#     # reverse history, pick up one line, remove new line characters and put it into clipboard
#     if [ -z "$1" ]; then
#         echo "Usage: h keyword [-v]"
#         echo "  '-v' will filter OUT matched you typing interactively"
#     else
#         history | grep "$1" | sed '1!G;h;$!d' | percol $2 | sed -n 's/^ *[0-9][0-9]* *\(.*\)$/\1/p'| tr -d '\n' | copy
#     fi
# }




#####
# specific fixes
#####

#keep ctrl+s from freezing terminal window:
#http://unix.stackexchange.com/a/12108
#NOTE: caused an error on ubuntu 17.10 + i3 (but not with default gnome)
#fixed by changing like this https://stackoverflow.com/a/25391867
[[ $- == *i* ]] && stty -ixon

#https://serverfault.com/a/3758
# ignore case, long prompt, exit if it fits on one screen, allow colors for ls and grep colors
export LESS="-iMFXR"

#https://serverfault.com/a/3758
# must press ctrl-D 2+1 times to exit shell
export IGNOREEOF="2"

#make aliases available when using sudo
alias sudo="sudo "

#https://serverfault.com/a/9046
#don't clobber existing files with >
#(use >| to force overwrite)
#shopt -o noclobber

######
## compression / extraction
######

# Easy extract -- extracts into a new empty folder
#https://gist.github.com/redguardtoo/01868d7a13817c9845e8#file-bashrc
extract () {
    if [ -f "$1" ] ; then
        case "$1" in
            *.tar.xz|*.tar.bz2|*.tar.gz|*.tar|*.tbz2|*.tgz|*.zip)
                fullpath=$(realpath "$1");
                dir=$(dirname "$fullpath");
                base=$(basename "$fullpath");
                root="${base%.*}" #remove extension
                path="$dir/$root"
                mkdir $path
                cd $path
                echo "Extracting into directory \"$path\" ..."
                infile="$fullpath" ;;
            *)
                infile="$fullpath" ;;

        esac
        case "$1" in
            *.tar.xz)    tar xvJf "$infile"    ;;
            *.tar.bz2)   tar xvjf "$infile"    ;;
            *.tar.gz)    tar xvzf "$infile"    ;;
            *.tar)       tar xvf "$infile"     ;;
            *.tbz2)      tar xvjf "$infile"    ;;
            *.tgz)       tar xvzf "$infile"    ;;
            *.bz2)       bunzip2 "$infile"     ;;
            *.rar)       unrar e "$infile"     ;;
            *.gz)        gunzip "$infile"      ;;
            *.apk)       unzip "$infile"       ;;
            *.epub)      unzip "$infile"       ;;
            *.xpi)       unzip "$infile"       ;;
            *.zip)       unzip "$infile"       ;;
            *.war)       unzip "$infile"       ;;
            *.jar)       unzip "$infile"       ;;
            *.Z)         uncompress "$infile"  ;;
            *.7z)        7z x "$infile"        ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

# easy compress - archive wrapper
compress () {
    if [ -n "$1" ] ; then
        FILE=$1
        case $FILE in
        *.tar) shift && tar cf $FILE $* ;;
        *.tar.bz2) shift && tar cjf $FILE $* ;;
        *.tar.gz) shift && tar czf $FILE $* ;;
        *.tgz) shift && tar czf $FILE $* ;;
        *.zip) shift && zip $FILE $* ;;
        *.rar) shift && rar $FILE $* ;;
        *) echo "don't know how to compress '$1'..." ;;
        esac
    else
        echo "usage: compress <foo.tar.gz> ./foo ./bar"
    fi
}



#############
# ls / navigation
#############

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias sl='ls'
alias lls='ls'
function lsh { ls -ltu $1 | head; }
# function cl() { if $(test $# -gt 0); then cd $1; fi; if $(test $(ls | wc -l) -lt 200); then ls; fi; }
# alias .="cl .."
# alias ..="cl ../.."
# alias ...="cl ../../.."



#############
# copy/paste
#############

alias copy='xclip -sel clip'
alias paste='xclip -sel clip -o'
#note: more
#https://gist.github.com/redguardtoo/01868d7a13817c9845e8#file-bashrc

############
# open / emacs
############

function o () { #open an arbitrary file
    xdg-open "$@" > /dev/null 2>&1 &
}
function img() { jp2a --colors "$1"  | less -r; } #show jpg in the terminal
function dl {
    #open last downloaded file
    f=$(ls -A1u ~/Downloads/ | head -1); o ~/Downloads/"$f";
}

alias l="less"
alias e="emacsclient --alternate-editor= -t"
alias emacs="emacsclient --alternate-editor= -t"
export EDITOR="emacsclient --alternate-editor= -t"

function eb() { emacs $(readlink -f ~/.bashrc); }
function sb() { source ~/.bashrc; }
# function ew() { emacs $(which $1); } #eg: ew inmypath.py
# function ef() { emacs $(find . -print | percol); }
# function eg() { emacs $(ag -g $1 | percol); }




########
# handle processes
########

function k() { ps aux | percol | awk '{ print $2 }' | xargs kill; }
# function k9() { ps aux | percol | awk '{ print $2 }' | xargs kill -9; }
# function psg() { ps -ef | grep "$1"; }
function kt() { pkill -9 -P $$; } #kill all children of this terminal


#######
# communication
#######
function text() {
    curl --header 'Access-Token: '$PUSHBULLET_TOKEN'' \
         --header 'Content-Type: application/json' \
         --data-binary '{ "push": { "conversation_iden": "'$1'","message": "'$2'","package_name": "com.pushbullet.android","source_user_iden": "'$PUSHBULLET_USER_ID'","target_device_iden": "'$PUSHBULLET_PHONE_ID'", "type": "messaging_extension_reply" }, "type": "push" }' \
         --request POST \
         https://api.pushbullet.com/v2/ephemerals
}
function textme() { text $PHONE_NUMBER $1; }
# function callme() { twilio_test.py -t $PHONE_NUMBER; } #NOTE: this doesn't work, twilio trial ran out
function notifyme() { noti -p -t "$1" -m "${2:- }"; } #title and message
alias emailme="gmail.py -t $EMAIL_ADDRESS "
alias skype="skypeforlinux"

#######
# wifi
#######

function wifi() { nmcli d wifi connect "$1" password "$2"; }
function wifi_ls() { nmcli c; nmcli d wifi list; }
function wifi_disconnect() { nmcli c down id "$1"; }
function wifi_reset() { sudo service network-manager restart; }



########
# lookup info
#######

alias c="cal $(date '+%Y')"
function myip() { wget http://ipinfo.io/ip -qO -; }
function geoip() { wget -qO - ipinfo.io/$1 | any2csv | plook -a; }
# function wiki() { pywiki.py "$1" | html2text -utf8 | tr '\n' ' '; echo; }
# function define() { dict $1 | head -25; }
function weather() {
    key=8b88e7d846a8fd02;
    echo NYC:
    echo ''
    data=$(curl -s "http://api.wunderground.com/api/$key/forecast/q/NY/New_York.json" | jq -r ['.forecast.txt_forecast.forecastday[] | [.title], [.fcttext], ["break"] | .[]'])
    echo $data | sed -e 's/[,]/\n/g' -e 's/[]"]/''/g' -e 's/[[]/''/g' -e 's/break/\n/g'
}



#######
# running bash / terminal commands
#######
function debug() { set -o functrace; set -x; $("$@"); set +x; set +o functrace; }
function xsh() { cat - | tr '\n' '\0' | xargs -r -0 -n1 bash -c; } #run each line of /dev/stdin in bash (-r so it doesn't crash on empty input)
function echoerr() { echo "$@" 1>&2; } #echo, but to /dev/stderr

#fzf for searching history
#https://github.com/junegunn/fzf
#(includes uninstall script if I don't like it)
#(another option is https://github.com/dvorka/hstr)
# [ -f ~/.fzf.bash ] && source ~/.fzf.bash

#qfc for autocompleting file names
#turning this off for now because its ctrl+f shortcut clashes with normal emacs ctrl+f usage at the command line
# [[ -s "$HOME/.qfc/bin/qfc.sh" ]] && source "$HOME/.qfc/bin/qfc.sh"

######
# python
######
alias python="python3"
export PYTHONPATH=$PYTHONPATH

#Note: basic pip3 command (after running sudo apt-get install python3-pip) doesn't work for me
#it still uses /usr/bin/python, which is python2 on my machine
#instead following this advice
#https://stackoverflow.com/questions/11268501/how-to-use-pip-with-python-3-x-alongside-python-2-x#comment41720502_19078295
alias pip3="python3 -m pip"

#######
# machine maintenance
#######
function bigdir() { du / | pawk -g 'int(l.split()[0]) > 1e6' -p 'write_line(l.split())' | psort -c0 -n | less; } #find big directories on the system
function dudir() { du -sc .[!.]* * |sort -h -r; }
function space_check() {
    sudo du -m --max-depth=4 / | sort -nr | head -n 20;
}

function machine() {
    sudo echo "hey" > /dev/null; #get sudo permissions in advance
    echo "-----";
    echo "Computer:";
    manufacturer=$(sudo dmidecode -s system-manufacturer)
    model=$(sudo dmidecode -s system-product-name)
    echo $manufacturer $model
    echo "-----";
    echo "CPU info:";
    cat /proc/cpuinfo | grep 'model name' | uniq; #print cpu name
    echo "-----";
    echo "Memory info:";
    grep -i memtotal /proc/meminfo | cat; #print memory quantity
    echo "-----";
    echo "Operating system:";
    lsb_release -a 2>/dev/null | grep -i description | cat #print version of ubuntu
    echo "-----";
    echo "Linux kernel info:";
    uname -a; #print version of linux kernel
    echo "-----";
    echo "Disk info:";
    df -h | grep /dev/ | cat; #print disk space
    echo "-----";
    echo "Display manager:";
    cat /etc/X11/default-display-manager
    echo "-----";
    echo "Desktop:";
    echo $DESKTOP_SESSION
    echo "-----"
}

#####
# docker
####
function dbuild() {
    echo $@;
    docker build -t ${1}:latest .;
}
alias dls="docker images -a"
function drm() {
    #https://coderwall.com/p/zguz_w/docker-remove-all-exited-containers
    sudo docker rm $(docker ps --all -q -f status=exited) #cleanup exited dockers
    sudo docker rmi "$@"
}
alias dps="docker ps"
alias dk="docker kill $@"

##################
# git
#################

#TODO: allow multiple arguments and watching multiple packages
function i() { #watch packages and auto reinstall them
    dir=$HOME/git/$1
    ls $dir/*/* | entr sh -c 'cd '$dir'; sudo python2 setup.py install --force; sudo python3 setup.py install --force; cd -;'
}

function cp_branch_master() {
    #to replace the contents of master with the contents of a branch:
    #https://ben.lobaugh.net/blog/202412/replace-one-git-branch-with-the-contents-of-another

    #git checkout master
    #git checkout -b "$1"
    git merge -s ours master
    git checkout master
    git merge "$1"
    # git push
}

alias g="git"
complete -o default -o nospace -F _git g
#working directory = 1, staged = 2, committed = 3
#these aliases copy your files from level i -> j for i,j in {1,2,3}
alias g12="git add"
alias g13="git commit"
#No easy way to commit a single staged file
#alias g23="git commit"
alias g21="confirm git checkout --"
alias g32="git reset --"
alias g31="confirm git checkout HEAD --"
alias gd="git diff"
alias gd13="git diff HEAD"
alias gd12="git diff"
alias gd23="git diff --cached"
alias gd31="git diff HEAD"
alias gd21="git diff"
alias gd32="git diff --cached"
alias gitroot='cd $(git rev-parse --show-toplevel) && echo "$_"'

#git shortcuts
# function gs() { git status; }
#gl now used for gitless
# function gl() {
#     if [ -n "$1" ]
#     then
#         git diff $1~ $1;
#     else
#         git diff HEAD~ HEAD;
#     fi
# }
function gp() { git stash && git pull --rebase && git stash apply; }
# function gg() { git log -p -S$1; }
# function sgrep() { git grep "$@" -- ':/' ':/!*thirdparty*' ':/!*data*'; }





######################
#commandline mysql
#####################

export MYSQL_HOST="localhost"
export MYSQL_DB="thingstodo"
function add_mysql_db() { mysql -u root -p -e "CREATE DATABASE $1"; }
function add_mysql_user() { mysql -u root -p -e "GRANT ALL PRIVILEGES ON *.* TO '$1'@'localhost' IDENTIFIED BY '$2';"; }
#CREATE TABLE example
# mysql -u root -p -e "CREATE TABLE pet (name VARCHAR(20), owner VARCHAR(20), species VARCHAR(20), sex CHAR(1), birth DATE, death DATE);"
function tables() { db "show tables from $MYSQL_DB" | cat; }
function dbi() { db -i "$1" | cat; }
function dbd() { db -d "$1" | cat; }
function dbc() { db --cat "$1"; }
function dbh() { db --head "$1"; }
function dbt() { db --tail "$1"; }
function dbw() { dbtp -w "$@"; }
function dbk() { db -r --key "$@" | ptr | plook -a -n; }
function dbtp() { db -r "$@" | ptr | plook -a -n; }



##############
# multiline commands
###############

#handling for multiline commands (breaks if using "history -c; history -r" in PROMPT_COMMAND above)
# shopt -s cmdhist lithist

#PS2 is the prompt for multiline commands (defaults to ">")
PS2=""



#################
## miscellaneous
################

# export GOPATH=$HOME/go
# export ANDROID_HOME=$HOME/Files/android_dev/android-sdk-linux
ANDROID_HOME=/opt/android-sdk
ANDROID_NATIVE_API_LEVEL=android-19
ANDROID_NDK=/opt/android-ndk-r14
ANDROID_PATH=/opt/android-sdk/tools:/opt/android-sdk/platform-tools:/opt/android-ndk-r14:$ANDROID_HOME/platforms:$ANDROID_HOME/tools
export PATH=$PATH:$HOME/local/bin:$HOME/misc_code/python_scripts:$GOPATH/bin:$HOME/Files/play:"$ANDROID_PATH"


# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm

function steam() { rm ~/.local/share/Steam/ubuntu12_32/steam-runtime/i386/usr/lib/i386-linux-gnu/libstdc++.so.6;
                   rm ~/.local/share/Steam/ubuntu12_32/steam-runtime/i386/usr/lib/i386-linux-gnu/libgcc_s.so.1;
                   steam;
                 }

function sshphone() { ssh -p 8022 -i ~/.ssh/jason-key-pair-useast.pem jtrigg@$1; }
function alarm() { echo "speaker-test -c2 -t sine -l1" | at $1; }
function scantopdf() {
    #use scanimage -L to find device name and then insert in -d flag below
    device_name=$(scanimage -L | pawk -g 'l and (not "queue=false" in l)' -p 'print l.split()[1][1:-1]')
    scanimage --resolution 200 --format=tiff -d "$device_name" | convert tiff:- -quality 40 -compress jpeg pdf:-;
}
function catpdfs() {
    pdftk "$@" cat output -;
}
function mic() { amixer set Capture toggle; } # mute/unmute mic

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

function serve {
    PORT_NUMBER=${1:-8000};
    python -m http.server $PORT_NUMBER;
}

# Auto-screen invocation. see: http://taint.org/wk/RemoteLoginAutoScreen
# if we're coming from a remote SSH connection, in an interactive session
# then automatically put us into a screen(1) session.   Only try once
# -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
# if screen fails for some reason.
if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" != x ]
then
  STARTED_SCREEN=1 ; export STARTED_SCREEN
  screen -RR -S main || echo "Screen failed! continuing with normal bash startup"
fi
# [end of auto-screen snippet]


#prompt the user before running a command
#http://askubuntu.com/a/22257
confirm() {
    echo -n "Are you sure you want to run '$*'? [N/y] "
    read -N 1 REPLY
    echo
    if test "$REPLY" = "y" -o "$REPLY" = "Y"; then
        "$@"
    else
        echo "Cancelled by user"
    fi
}

function watch_python() { find . -name '*.py' | entr sh -c 'sudo python '$PWD'/setup.py install > /dev/null' & }

function fix_numpy() { less /var/lib/dpkg/status | pawk -b 'x=0' -p 'if "Package:" in l: x = "python-rdkit" in l;' 'if x and l.startswith("Depends:"): write_line([x for x in r if not "numpy" in x]); else: print l' > /tmp/test.txt; }

#show previous command in tab name
#http://www.davidpashley.com/articles/xterm-titles-with-bash/
#keep this at the end of the file or else running the bashrc commands will change the starting tab title
# if [ "$SHELL" = '/bin/bash' ]
# then
#     case $TERM in
#         rxvt|xterm-256color)
#             #leaving functrace on causes the terminal to freak out on tab completion
#             # set -o functrace
#             trap 'echo -ne "\e]0;"; echo -n $BASH_COMMAND | cmd_summary.py; echo -ne "\007"; history -a; history -c; history -r;' DEBUG
#             # trap 'PREV_CMD=$CURR_CMD; CURR_CMD=$BASH_COMMAND' DEBUG
#          ;;
#     esac
# fi


# function quote() {
#     #function from:
#     #https://www.jefftk.com/p/bash-argument-parsing-and-quoting

#     #related: good comments about quoting / variable expansion:
#     #http://stackoverflow.com/a/13819996
#     #http://unix.stackexchange.com/a/109074
#     first=true
#     for arg in "$@"; do
#         if $first; then first=false; else echo -n " "; fi
#         if echo "$arg" | grep -q '[^a-zA-Z0-9./_=-]'; then
#             arg="'$(echo "$arg" | sed -e 's~\\~\\\\~g' -e "s~'~\\\\'~g")'"
#         fi
#         echo -n "$arg"
#     done
#     echo
# }

# function unquote() { echo "$1" | xargs echo; }




##########################################
#######personal stuff starts here#######
##########################################

###########
#torch setup
###########
# . /home/jtrigg/torch/install/bin/torch-activate
# export PATH=/usr/local/cuda/bin/:$PATH;
# export LD_LIBRARY_PATH=/usr/local/cuda/lib64/:$LD_LIBRARY_PATH;
# export TIEFVISION_HOME=/home/jtrigg/github/tiefvision_dresses


###########
# tensorflow setup
##########
# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64"
# export CUDA_HOME="/usr/local/cuda"


###################
# pantsme mysql
###################

# function dbwhere() { db "select * from start.product_variations where $1"; }
# VAR_SELECT="pv.*, rb.name ref_brand, rb.available brand_available, pf.id pf_family_id, pf.title family_title, ps.id source_id, ps.external_id, ps.parent_external_id, ps.features, ps.product_type_name, ps.editorial_reviews, ps.blacklist, ps.blacklist_score"
# function var() { dbtp 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.brand_lookup bl ON pv.brand = bl.name LEFT JOIN start.reference_brands rb ON bl.brand_id = rb.id LEFT JOIN start.product_family_ids pfi ON pv.id = pfi.variation_id LEFT JOIN start.product_families pf ON pfi.family_id = pf.id WHERE pv.id = "'$1'"'; }
# function plotvar() { db -r 'SELECT image_url FROM product_variations pv WHERE pv.id = "'$1'"' | $MS_DIR/start_python/start/scripts/plotting.py; }
# function src() { dbtp 'SELECT * FROM product_sources ps WHERE ps.id = '$1''; }
# function brand() { dbtp 'SELECT bl.name lookup_name, rb.* FROM reference_brands rb LEFT JOIN brand_lookup bl ON rb.id = bl.brand_id WHERE rb.name REGEXP "'"$1"'" OR bl.name REGEXP "'"$1"'"'; }
# function fam() { db 'SELECT pf.* FROM product_families pf JOIN reference_brands rb ON rb.id = pf.brand_id WHERE rb.name REGEXP "'"$1"'"'; }

# function asin_var() { dbtp 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.brand_lookup bl ON pv.brand = bl.name LEFT JOIN start.reference_brands rb ON bl.brand_id = rb.id LEFT JOIN start.product_family_ids pfi ON pv.id = pfi.variation_id LEFT JOIN start.product_families pf ON pfi.family_id = pf.id WHERE (ps.origin_id in ("2","3") and ps.external_id = "'$1'")'; }
# function sibs() { db 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.reference_brands rb ON pv.brand_id = rb.id LEFT JOIN start.product_families pf ON pf.id = pv.family_id JOIN product_sources ps2 ON ps2.parent_id = ps.parent_id WHERE (ps2.origin_id IN ("2","3") and ps2.external_id = "'$1'") OR ps2.variation_id = "'$1'"'; }
# function blacklist() { db 'UPDATE start.product_sources ps SET ps.blacklist = 1 WHERE (ps.origin_id IN ("2","3") AND ps.external_id = "'$1'") OR ps.variation_id = "'$1'"'; }
# function blacklist_sibs() { db 'UPDATE start.product_sources ps JOIN start.product_sources ps2 ON ps.parent_id = ps2.parent_id SET ps.blacklist = 1 WHERE ps2.external_id = "'$1'" OR ps2.variation_id = "'$1'"'; }
# function blacklist_merge() { db 'DELETE pv FROM start.product_variations pv INNER JOIN start.product_sources ps ON ps.variation_id = pv.id WHERE (ps.blacklist OR ps.blacklist_score > 0.5)'; }

# function asin() { google-chrome "http://www.amazon.com/dp/$1";
# }
# function offers() { db 'SELECT pso.* FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.product_source_offers pso ON pso.source_id = ps.id and pso.origin_id = ps.origin_id WHERE ps.external_id = "'$1'"'; }

# MS_DIR=$HOME/github/mysize_shopping;


# function color() { $MS_DIR/start_python/start/augment_jeans/process_image.py --asin "$1"; $MS_DIR/start_python/start/augment_jeans/main.py --sibling_variation "$1"; }

# function ms() { cd $MS_DIR; }
# function ffms() { ff $1 | grep -v build; }
# function eff() { emacs $(ff.py -p $1 -d ${2:-$MS_DIR} | head -1); }


# function not_jeans() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,0 FROM start.product_sources ps WHERE ps.variation_id = "'$1'" OR ps.external_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = 0'; }
# function not_jeans() { mark_jeans $1 0; }
# function is_jeans()  { mark_jeans $1 1; }
# function mark_jeans() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }
# function mark_jeans_asin() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.external_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }
# function mark_jeans_var() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.variation_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }

# function flag() { ${MS_DIR}/start_python/start/modelling/blacklist_jeans.py -l -p --variation "$1" -v; blacklist_sibs "$1"; not_jeans "$1"; }

# #startup ssh execute
# function sse() { ssh deploy@pants-me.com -t "$1"; }



##########
# play (framework)
##########

# alias icbm='python tools/icbm/build.py'
# alias play='thirdparty/play/play'
# function bplay() {
#   echo -e "\033];$1\007" && icbm :$1_dev && thirdparty/play/play test src/com/start/$1 ${@:2}
# }

# function bplay_prod() {
#     echo -e "\033];$1\007" && icbm :$1_deploy && thirdparty/play/play run src/com/start/$1 --%prod ${@:2}
# }

# function dep() {
#     cd ~/github/start_deploy
#     git pull
#     rm -r build
#     ./deploy.sh
#     cd -
# }



######
#start cloth simulation
#######
# function cloth() { cd /home/jtrigg/github/mysize_shopping/experimental/cloth; (python -m http.server 31014 &); while [ 1 ]; do node --max_old_space_size=4000 ./node_modules/.bin/gulp; sleep 3; done; }


######
#swift setup
#####
# export PATH=$HOME/files/swift-3.1.1-RELEASE-ubuntu16.04/usr/bin:"${PATH}"


##commands for easy upload/download to storage server
function cosaliup() {
    scp "$1" $STORAGE_SERVER_USER@$STORAGE_SERVER:/upload
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER chmod a+r /upload/$(basename "$1")
}

function cosalidown() {
    scp $STORAGE_SERVER_USER@$STORAGE_SERVER:/upload/"$1" .
}

function cosalils() {
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER ls -l /upload
}

function cosalirm() {
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER rm /upload/"$1"
}
