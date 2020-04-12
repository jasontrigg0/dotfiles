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


########
#PS1: runs to display the prompt string and optionally, to set the tab title
########

#use \[ \] around colors or else terminal multi-line commands may wrap onto the same line
#https://askubuntu.com/a/24422
#TODO: try to include the \[ and \] in the color definitions above, maybe this can help? https://stackoverflow.com/a/43462720
if [ "$color_prompt" = yes ]; then
    if [ ! -z "$SSH_CONNECTION" ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[${COLOR_RED}\]\u@\h\[${COLOR_RESTORE}\]:\[${COLOR_RED}\]\w\[${COLOR_RESTORE}\]\$ ' #show all red in SSH connection
    else
        PS1='${debian_chroot:+($debian_chroot)}\[${COLOR_LIGHT_GREEN}\]\u@\h\[${COLOR_RESTORE}\]:\[${COLOR_LIGHT_BLUE}\]\w\[${COLOR_RESTORE}\]\$ '
    fi
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

PS1_BASIC=$PS1; #BASIC_PS1 sets the prompt without setting the terminal title
PS1_SET_TITLE="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1_BASIC"; #PS1 to set the tab title as well as the prompt string

#If this is an xterm set the tab title to user@host:dir with every new prompt
case "$TERM" in
    xterm*|rxvt*)
        PS1=$PS1_SET_TITLE
        ;;
*)
    ;;
esac

function set_title() {
    echo -e "\e]0;"; echo -n "$1"; echo -ne "\007";
    PS1=$PS1_BASIC; #revert to PS1_BASIC which sets the prompt without adjusting the tab title (see above)
}

function unset_title() {
    PS1=$PS1_SET_TITLE;
}


#######
#aliases
#######

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

function h() {
    cat -A ~/.full_history | grep -a "$1" | less +G;
}


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
if [ -z "$SSH_CONNECTION" ]; then
    export IGNOREEOF="2"
fi

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
        fullpath=$(realpath "$1");
        case "$1" in
            *.tar.xz|*.tar.bz2|*.tar.gz|*.tar|*.tbz2|*.tgz|*.zip)
                dir=$(dirname "$fullpath");
                base=$(basename "$fullpath");
                root="${base%.*}" #remove extension
                path="$dir/$root"
                mkdir "$path"
                cd "$path"
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
function lshd { ls -ltu /home/jtrigg/Downloads | head; }
function cl() { if $(test $# -gt 0); then cd "$@" || return; fi; if $(test $(ls | wc -l) -lt 200); then ls; fi; }
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


#-r: relative symlinks by default
#https://superuser.com/a/837935
#-f: forces overwrite of existing files at the location
#also reminder: ln syntax is like cp
#$1 is the source of the link, $2 is the destination
alias ln='ln -snfr'

function swap()
{
    local TMPFILE=tmp.$$
    mv "$1" $TMPFILE && mv "$2" "$1" && mv $TMPFILE $2
}

############
# open / emacs
############

function o() { #open an arbitrary file
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

function eb() { $EDITOR $(readlink -f ~/.bashrc); }
function ebp() { $EDITOR $(readlink -f ~/.bash_private); }
function sb() { source ~/.bashrc; }
function eg() {
    if [ -z "$1" ]; then
        $EDITOR $(ag -g "" | percol);
    elif [ -z "$2" ]; then
        $EDITOR $(ag -g "$1" | percol);
    else
        $EDITOR $(ag "$@" | percol);
    fi;
}
alias ag="ag -W 120 --hidden" #limit ag results to 120 characters per line, also show hidden files such as .env by default
# function ew() { emacs $(which $1); } #eg: ew inmypath.py
# function ef() { emacs $(find . -print | percol); }

########
# handle processes
########

function k() { ps aux | percol | awk '{ print $2 }' | xargs kill; }
function k9() { ps aux | percol | awk '{ print $2 }' | xargs kill -9; }
# function psg() { ps -ef | grep "$1"; }
function kt() { pkill -9 -P $$; } #kill all children of this terminal
function getport() { pid=$(ss -tanp | grep "$1" | perl -n -e'/pid=(\d+)/ && print $1'); if [ -n "$pid" ]; then ps -ef | awk '$2=='"$pid"; fi }

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
function flush_dns_cache() { sudo systemd-resolve --flush-caches; } #https://askubuntu.com/a/909173
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
    echo "Disk information:";
    lsblk | grep '^sd' | cat;
    echo "-----";
    echo "Free space on mounted partitions:";
    df -h | grep /dev/ | cat; #print disk space
    echo "-----";
    echo "Display manager:";
    cat /etc/X11/default-display-manager
    echo "-----";
    echo "Resolution:"; #1920x1080 pixels
    xdpyinfo  | grep -o ' .*x.* pixels' --color=never
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
function dockercleanup() {
    docker rm $(docker ps -a -q) #cleanup stopped containers
    # docker rm $(docker ps --all -q -f status=exited); #cleanup exited dockers

    #prompt for deletion of image tags
    tags=$(docker images -a | awk '$1 != "<none>" && $2 != "<none>" && NR > 1{print $1 ":" $2}')

    echo "Checking if we can delete existing tagged images:"
    for t in ${tags}; do
        confirm docker rmi $t
    done

    docker rmi $(sudo docker images --filter "dangling=true" -q --no-trunc)
}

function drm() {
    #https://coderwall.com/p/zguz_w/docker-remove-all-exited-containers
    dockercleanup;
    sudo docker rmi "$@";
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

function gf() {
    git diff --color=always HEAD -U0 -G"$1" | less -r +/"$1";
}

function git_revert_file() {
    #https://stackoverflow.com/a/7196615
    confirm git show HEAD -- $1 | git apply -R
}

function git_old() {
    #show old version of a file
    #git_old COMMIT file
    GITPATH=$(git ls-tree --full-name --name-only HEAD ${2});
    git show ${1}:$GITPATH;
}

#common tasks
#view single commit diff: git log -p -1 $COMMIT
#view all diffs to a given file: git log -p $FILE
#grep through diffs: git log -p -G $GREP
#     OR             git log --stat -G $GREP
#grep through commit messages git lop -g --grep $GREP

#want git aliases+functions that
#apply to all files work with or without ".".
#unlike the raw git functions which sometimes need "."
#and sometimes cannot have "."
function _drop_final_dot() {
    if [ "${@: -1}" == "." ]; then
        "${@:1:$(($#-1))}" #https://stackoverflow.com/a/1215592
    else
        "$@"
    fi;
}

alias g="git"
complete -o default -o nospace -F _git g
#information on checkout, reset, revert: https://www.atlassian.com/git/tutorials/resetting-checking-out-and-reverting
#working directory = 1, staged = 2, committed = 3, (another diff = 9)
#these aliases copy your files from level i -> j for i,j in {1,2,3}
#use . to apply to all files
alias g12="git add"
alias g13="git commit"
#No easy way to commit a single staged file
#alias g23="git commit"
alias g21="confirm git checkout --"
alias g32="_drop_final_dot git reset --"
alias g31="confirm git checkout HEAD --" #doesn't work on all files (with or without .)
function g92() {
    #pull from a diff into staging location (= 2)
    #usage: g92 DIFF file1 file2...
    DIFF=$1;
    shift;
    git checkout ${DIFF} -- "$@";
}
function g91() {
    #pull from a diff into working directory (= 1)
    #usage: g92 DIFF file1 file2...
    g92 "$@";
    shift;
    g21 "$@";
}

#TODO: try https://gist.github.com/mwhite/6887990
alias diff="git diff --no-index" #git style diff from the command line
alias gd="git diff"
alias ga="git add"
alias gc="git commit"
alias gs="git status"
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

function add_mysql_db() { mysql -u root -p -e "CREATE DATABASE $1"; }
function add_mysql_user() { mysql -u root -p -e "GRANT ALL PRIVILEGES ON *.* TO '$1'@'localhost' IDENTIFIED BY '$2';"; }
#CREATE TABLE example
# mysql -u root -p -e "CREATE TABLE pet (name VARCHAR(20), owner VARCHAR(20), species VARCHAR(20), sex CHAR(1), birth DATE, death DATE);"
function tables() { db "show tables from $MYSQL_DB" | cat; }
#https://stackoverflow.com/a/5648713
function fields() { db 'select * from information_schema.columns where table_schema = "'$MYSQL_DB'" order by table_name, ordinal_position'; }
function dbp() { db "$(paste)"; }
function dbf() { db "$(cat $1)"; }
function dbi() { db -i "$1" | cat; }
function dbd() { db -d "$1" | cat; }
function dbh() { db --head "$1"; }
function dbt() { db --tail "$1"; }
function dbw() { dbtp -w "$@"; }
function dba() { db --cat "$1"; }
function dbc() { db --cascade_select "$1" "$2"; }
function dbk() { db -r --key "$@" | ptr | plook -a -n; }
function dbkc() { db -r --cascade_select "$1" "WHERE $1.id=$2" | ptr | plook -n; }
function dbkca() { db -r --cascade_select_all "$1" "WHERE $1.id=$2" | ptr | plook -n; }
function dbtp() { db -r "$@" | ptr | plook -a -n; }
function dbs() { db -r 'SHOW CREATE TABLE '"$1"''; }
function dbtree() { db -r --tree; }
function dbtreerev() { db -r --tree_rev; }

##############
# multiline commands
###############

#handling for multiline commands (breaks if using "history -c; history -r" in PROMPT_COMMAND above)
# shopt -s cmdhist lithist

#PS2 is the prompt for multiline commands (defaults to ">")
PS2=""



################
## miscellaneous
################
function bash_most_used() {
    less ~/.bash_history | tr '\|' '\n' | pawk -p 'print(l.strip())' | awk '{print $1}' | sort | uniq -c | sort -k1 -n -r | head -100
}

alias npmscripts='less package.json | any2csv --path scripts | ptr | plook -n -a'

function timestamp() {
    date '+%Y%m%d_%H%M%S';
}

function macro_record() {
    echo "Press F4 to stop (takes a second after you press it)"
    cnee --record -o $HOME/.macro --mouse --keyboard -t 0.1 -sk 'F4' --first-last > /dev/null 2>&1;
}

function macro_play() {
    #eg: to replay 10 times
    #macro_play 10
    echo "Press F4 to stop (if running multiple iterations it only stops a single iteration)"
    cnt=${1:-1}
    for i in $(seq $cnt); do
        cnee --replay -f $HOME/.macro -t 0.1 -sk 'F4' > /dev/null 2>&1;
    done
}

function csvsplit() {
    python -c 'print("'"$1"'".split(",")['"$2"'])';
}

alias lock='i3lock -n'

# export GOPATH=$HOME/go
# export ANDROID_HOME=$HOME/Files/android_dev/android-sdk-linux
ANDROID_HOME=/opt/android-sdk
ANDROID_NATIVE_API_LEVEL=android-19
ANDROID_NDK=/opt/android-ndk-r14
ANDROID_PATH=/opt/android-sdk/tools:/opt/android-sdk/platform-tools:/opt/android-ndk-r14:$ANDROID_HOME/platforms:$ANDROID_HOME/tools
export PATH=$PATH:$HOME/local/bin:$HOME/scripts:$GOPATH/bin:$HOME/Files/play:"$ANDROID_PATH"


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

function quickserve {
    PORT_NUMBER=${1:-8000};
    python -m http.server $PORT_NUMBER;
}

# Auto-screen invocation. see: http://taint.org/wk/RemoteLoginAutoScreen
# if we're coming from a remote SSH connection, in an interactive session
# then automatically put us into a screen(1) session.   Only try once
# -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
# if screen fails for some reason.
if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_CONNECTION:-x}" != x -a "${SSH_TTY:-x}" = x ]  #allow ssh -t without logging into screen, eg to run sudo from command line
then
  STARTED_SCREEN=1 ; export STARTED_SCREEN
  screen -RR -S main || echo "Screen failed! continuing with normal bash startup"
fi
# [end of auto-screen snippet]


#prompt the user before running a command
#http://askubuntu.com/a/22257
confirm() {
    echo "Are you sure you want to run '$*'? [N/y] "
    read -N 1 REPLY
    echo
    if test "$REPLY" = "y" -o "$REPLY" = "Y"; then
        "$@"
    else
        echo "Cancelled by user";
        return 1;
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


function quote() {
    #problem:
    #given some bash expression
    #eg: awk '{print $1}'
    #how to use it as an argument to another function?
    #eg new_function $my_awk_var

    #Simple case:
    #copy: awk '{print $1}'
    #THEN
    #new_function "$(paste)"
    #OR
    #arg=$(paste)
    #new_function "$arg"
    #OR
    #paste > bash_command_pasted_in_file.sh
    #new_function "$(cat bash_command_pasted_in_file.sh)"


    #But what about multiple layers?
    #Example:
    #awk '{print $1}'
    #but you want to wrap that in bash
    #bash -c 'my awk in here'
    #and maybe you want to wrap *that* in ssh:
    #ssh jtrigg@example.com 'my bash in here'

    #Works like this:
    #cmd1='{print $1}'
    #cmd2='echo "hey" | awk '"$(quote $cmd1)"
    #cmd3='bash -c '"$(quote $cmd2)"
    #ssh jtrigg@example.com "$cmd3"
    #OR all at once:
    #ssh jtrigg@example.com 'bash -c '"$(quote 'echo "hey" | awk '$(quote '{print $1}'))"


    #quote function modified from:
    #https://www.jefftk.com/p/bash-argument-parsing-and-quoting

    #related: good comments about quoting / variable expansion:
    #http://stackoverflow.com/a/13819996
    #http://unix.stackexchange.com/a/109074

    if [ -z "$1" ]; then
        arglist="$(cat - )";
    else
        arglist="$@";
    fi

    for arg in "$arglist"; do
        if echo "$arg" | grep -q '[^a-zA-Z0-9./_=-]'; then
            #fix: original function was choking on the two character string: ')
            #and single backslash: \
            arg="'$(echo "$arg" | sed -e "s~'~'\\\\''~g")'"
        fi
        echo -n "$arg"
    done

    echo
}



# function unquote() { echo "$1" | xargs echo; }


###########
#torch setup
###########
# . /home/jtrigg/torch/install/bin/torch-activate
# export PATH=/usr/local/cuda/bin/:$PATH;
# export LD_LIBRARY_PATH=/usr/local/cuda/lib64/:$LD_LIBRARY_PATH;

###########
# tensorflow setup
##########
# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64"
# export CUDA_HOME="/usr/local/cuda"

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
#swift setup
#####
# export PATH=$HOME/files/swift-3.1.1-RELEASE-ubuntu16.04/usr/bin:"${PATH}"


##commands for easy upload/download to storage server
function archup() {
    scp "$1" $STORAGE_SERVER_USER@$STORAGE_SERVER:/upload
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER chmod a+r /upload/$(basename "$1")
}

function archdown() {
    scp $STORAGE_SERVER_USER@$STORAGE_SERVER:/upload/"$1" "${2:-.}"
}

function archls() {
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER ls -lA /upload
}

function archrm() {
    ssh $STORAGE_SERVER_USER@$STORAGE_SERVER rm /upload/"$1"
}

function archmv() {
    archdown $1 $2;
    archrm $1;
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
