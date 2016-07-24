# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
# case $- in
#     *i*) ;;
#       *) return;;
# esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=250000
HISTFILESIZE=250000

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

#If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        #turn this off so tab naming based on last command words
        # PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
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


function cl() { if $(test $# -gt 0); then cd $1; fi; if $(test $(ls | wc -l) -lt 200); then ls; fi; }
# alias .="cl .."
# alias ..="cl ../.."
# alias ...="cl ../../.."
alias sudo="sudo " #without this aliases aren't available when using sudo
alias l="less"
alias e="emacsclient --alternate-editor= -t"
alias emacs="emacsclient --alternate-editor= -t"
export EDITOR="emacsclient --alternate-editor= -t"
# export GOPATH=$HOME/go
# export ANDROID_HOME=$HOME/Files/android_dev/android-sdk-linux
export PATH=$PATH:$HOME/Dropbox/misc_code/utils:$HOME/Dropbox/misc_code/utils/R:$GOPATH/bin:$ANDROID_HOME/platforms:$ANDROID_HOME/tools:$HOME/Files/play
export PYTHONPATH=$PYTHONPATH:$HOME/Dropbox/misc_code/utils:$HOME/github/mysize_shopping/start_python

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm

function myip() { wget http://ipinfo.io/ip -qO -; }
function geoip() { wget -qO - ipinfo.io/$1 | any2csv | plook -a; }
function text() { curl http://textbelt.com/text -d number=$1 -d "message=$2"; }
function textme() { text $PHONE_NUMBER $1; }
function callme() { twilio_test.py -t $PHONE_NUMBERx; }
alias emailme="gmail.py -t $EMAIL_ADDRESS "

function wiki() { pywiki.py "$1" | html2text -utf8 | tr '\n' ' '; echo; }
function define() { dict $1 | head -25; }

function debug() { set -o functrace; set -x; $("$@"); set +x; set +o functrace; }

function mic() { amixer set Capture toggle; } # mute/unmute mic
function b() { sudo bash -c "echo $1 > /sys/class/backlight/intel_backlight/brightness"; }
function h() { grep -a $1 ~/.bash_history | tail; }
function kt() { pkill -9 -P $$; } #kill all children of this terminal
function ff() { find ${2:-"."} -name '*'$1'*' | xargs -r ls -ltu | awk '{print $9}'; } #files with this name under this directory. 
function ff1() { ff $1 | head -1; } #find most recently updated file with this name under this directory
function gf() { grep -r -l -i $1 .; } #recursive grep for files containing the string in the current directory
function xsh() { cat - | tr '\n' '\0' | xargs -r -0 -n1 bash -c; } #run each line of /dev/stdin in bash (-r so it doesn't crash on empty input)
function ew() { emacs $(which $1); } #eg: ew inmypath.py
function echoerr() { echo "$@" 1>&2; } #echo, but to /dev/stderr

#git shortcuts
function gl() { git diff HEAD~$1 HEAD; }
function gp() { git stash; git pull --rebase; git stash apply; }
function gg() { git log -p -S$1; }
function sgrep() { git grep "$@" -- ':/' ':/!*thirdparty*' ':/!*data*'; }
#working directory = 1, staged = 2, committed = 3
#these aliases copy your files from level i -> j for i,j in {1,2,3}
alias g12="git add"
alias g13="git commit"
#No easy way to commit a single staged file
#alias g23="git commit"
alias g21="git checkout --"
alias g32="git reset --"
alias g31="git checkout HEAD --"
alias gd="git diff"
alias gd13="git diff HEAD"
alias gd12="git diff"
alias gd23="git diff --cached"
alias gd31="git diff HEAD"
alias gd21="git diff"
alias gd32="git diff --cached"

#startup mysql shortcuts
export MYSQL_DB="start"
export MYSQL_HOST="pants-me.com"
function tables() { db "show tables from start" | cat; }
function dbi() { db -i "$1" | cat; }
function dbd() { db -d "$1" | cat; }
function dbc() { db --cat "$1"; }
function dbh() { db --head "$1"; }
function dbt() { db --tail "$1"; }
function dbw() { dbtp -w "$@"; }
function dbk() { db -r --key "$@" | ptr | plook -a -n; }
function dbtp() { db -r "$@" | ptr | plook -a -n; }
function dbwhere() { db "select * from start.product_variations where $1"; }
VAR_SELECT="pv.*, rb.name ref_brand, rb.available brand_available, pf.id pf_family_id, pf.title family_title, ps.id source_id, ps.external_id, ps.parent_external_id, ps.features, ps.product_type_name, ps.editorial_reviews, ps.blacklist, ps.blacklist_score"
function var() { dbtp 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.brand_lookup bl ON pv.brand = bl.name LEFT JOIN start.reference_brands rb ON bl.brand_id = rb.id LEFT JOIN start.product_family_ids pfi ON pv.id = pfi.variation_id LEFT JOIN start.product_families pf ON pfi.family_id = pf.id WHERE pv.id = "'$1'"'; }
function plotvar() { db -r 'SELECT image_url FROM product_variations pv WHERE pv.id = "'$1'"' | $MS_DIR/start_python/start/scripts/plotting.py; }
function src() { dbtp 'SELECT * FROM product_sources ps WHERE ps.id = '$1''; }
function brand() { dbtp 'SELECT bl.name lookup_name, rb.* FROM reference_brands rb LEFT JOIN brand_lookup bl ON rb.id = bl.brand_id WHERE rb.name REGEXP "'"$1"'" OR bl.name REGEXP "'"$1"'"'; }
function fam() { db 'SELECT pf.* FROM product_families pf JOIN reference_brands rb ON rb.id = pf.brand_id WHERE rb.name REGEXP "'"$1"'"'; }

function asin_var() { dbtp 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.brand_lookup bl ON pv.brand = bl.name LEFT JOIN start.reference_brands rb ON bl.brand_id = rb.id LEFT JOIN start.product_family_ids pfi ON pv.id = pfi.variation_id LEFT JOIN start.product_families pf ON pfi.family_id = pf.id WHERE (ps.origin_id in ("2","3") and ps.external_id = "'$1'")'; }
function sibs() { db 'SELECT '"$VAR_SELECT"' FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.reference_brands rb ON pv.brand_id = rb.id LEFT JOIN start.product_families pf ON pf.id = pv.family_id JOIN product_sources ps2 ON ps2.parent_id = ps.parent_id WHERE (ps2.origin_id IN ("2","3") and ps2.external_id = "'$1'") OR ps2.variation_id = "'$1'"'; }
function blacklist() { db 'UPDATE start.product_sources ps SET ps.blacklist = 1 WHERE (ps.origin_id IN ("2","3") AND ps.external_id = "'$1'") OR ps.variation_id = "'$1'"'; }
function blacklist_sibs() { db 'UPDATE start.product_sources ps JOIN start.product_sources ps2 ON ps.parent_id = ps2.parent_id SET ps.blacklist = 1 WHERE ps2.external_id = "'$1'" OR ps2.variation_id = "'$1'"'; }
function blacklist_merge() { db 'DELETE pv FROM start.product_variations pv INNER JOIN start.product_sources ps ON ps.variation_id = pv.id WHERE (ps.blacklist OR ps.blacklist_score > 0.5)'; }

function asin() { google-chrome "http://www.amazon.com/dp/$1"; 
}
function offers() { db 'SELECT pso.* FROM start.product_sources ps LEFT JOIN start.product_variations pv ON ps.variation_id = pv.id LEFT JOIN start.product_source_offers pso ON pso.source_id = ps.id and pso.origin_id = ps.origin_id WHERE ps.external_id = "'$1'"'; }

MS_DIR=$HOME/github/mysize_shopping;

function color() { $MS_DIR/start_python/start/augment_jeans/process_image.py --asin "$1"; $MS_DIR/start_python/start/augment_jeans/main.py --sibling_variation "$1"; }

function ms() { cd $MS_DIR; }
function ffms() { ff $1 | grep -v build; }
function eff() { emacs $(ff.py -p $1 -d ${2:-$MS_DIR} | head -1); }


# function not_jeans() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,0 FROM start.product_sources ps WHERE ps.variation_id = "'$1'" OR ps.external_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = 0'; }
function not_jeans() { mark_jeans $1 0; }
function is_jeans()  { mark_jeans $1 1; }
function mark_jeans() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }
function mark_jeans_asin() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.external_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }
function mark_jeans_var() { db 'INSERT INTO start.image_classes (image_url, is_jeans) SELECT image_url,'$2' FROM start.product_sources ps WHERE ps.variation_id = "'$1'" ON DUPLICATE KEY UPDATE is_jeans = '$2''; }


function flag() { ${MS_DIR}/start_python/start/modelling/blacklist_jeans.py -l -p --variation "$1" -v; blacklist_sibs "$1"; not_jeans "$1"; }

function watch_python() { find . -name '*.py' | entr sh -c 'sudo python '$PWD'/setup.py install > /dev/null' & }

function fix_numpy() { less /var/lib/dpkg/status | pawk -b 'x=0' -p 'if "Package:" in l: x = "python-rdkit" in l;' 'if x and l.startswith("Depends:"): write_line([x for x in r if not "numpy" in x]); else: print l' > /tmp/test.txt; }


#use trap DEBUG below instead of PROMPT_COMMAND or else the show-last-command-in-tab-name functionality breaks
#write to bash_history after each command, also reload bash_history
# export PROMPT_COMMAND='echo -ne "\033]0;$BASH_\007"; '
# export PROMPT_COMMAND='history -a; history -c; history -r;'


#handling for multiline commands (breaks if using "history -c; history -r" in PROMPT_COMMAND above)
# shopt -s cmdhist lithist


#PS2 is the prompt for multiline commands (defaults to ">")
PS2=""

alias icbm='python tools/icbm/build.py'
alias play='thirdparty/play/play'
function bplay() {
  echo -e "\033];$1\007" && icbm :$1_dev && thirdparty/play/play test src/com/start/$1 ${@:2}
}

function bplay_prod() {
    echo -e "\033];$1\007" && icbm :$1_deploy && thirdparty/play/play run src/com/start/$1 --%prod ${@:2}
}


function space_check() {
    sudo du -m --max-depth=4 / | sort -nr | head -n 20;
}


function dep() {
    cd ~/github/start_deploy
    git pull
    rm -r build
    ./deploy.sh
    cd -
}

#show previous command in tab name
#http://www.davidpashley.com/articles/xterm-titles-with-bash/
#keep this at the end of the file or else running the bashrc commands will change the starting tab title
if [ "$SHELL" = '/bin/bash' ]
then
    case $TERM in
        rxvt|xterm-256color)
            #leaving functrace on causes the terminal to freak out on tab completion
            # set -o functrace 
            trap 'echo -ne "\e]0;"; echo -n $BASH_COMMAND | cmd_summary.py; echo -ne "\007"; history -a; history -c; history -r;' DEBUG
            # trap 'PREV_CMD=$CURR_CMD; CURR_CMD=$BASH_COMMAND' DEBUG
         ;;
    esac
fi



. /home/jtrigg/torch/install/bin/torch-activate
