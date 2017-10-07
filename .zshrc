# Check if Command is available:
# if ! type "$foobar_command_name" > /dev/null; then
    # do stuff
# fi

# Check if file exists:
# if [[ -a file ]]; then
#   do stuff
# fi

source $HOME/.profile

EMACS_BIN_DIR=/usr/local/opt/emacs-mac/bin
export BUNDLER_EDITOR="$EMACS_BIN_DIR/emacs"
export EDITOR="$EMACS_BIN_DIR/emacsclient -t"

if [[ -a ~/dotfiles/antigen.zsh ]]; then
    source ~/dotfiles/antigen.zsh
    autoload -U colors && colors
    setopt promptsubst
    antigen bundle git
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen bundle zsh-users/zsh-completions
    antigen theme ~/dotfiles worace
    antigen apply
fi

if [[ -a /usr/local/bin/virtualenvwrapper_lazy.sh ]]; then
    export WORKON_HOME=$HOME/.virtualenvs
    export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
    source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

if [[ -a $HOME/.local/bin/virtualenvwrapper_lazy.sh ]]; then
    export WORKON_HOME=$HOME/.virtualenvs
    export VIRTUALENVWRAPPER_SCRIPT=$HOME/.local/bin/virtualenvwrapper.sh
    source $HOME/.local/bin/virtualenvwrapper_lazy.sh
fi

case `uname` in
  Darwin)
    #increase keyrepeat speed beyond os x allowed maximum
    eval "defaults write NSGlobalDomain KeyRepeat -int 1"
    em () {
        if [ "$#" -ne 0 ];
        then
            $EMACS_BIN_DIR/emacsclient -c -n $*
        else
            $EMACS_BIN_DIR/emacsclient -c -n "~/Dropbox/notes/scratch.org"
        fi
    }
    ;;
  Linux)
    alias copy="xclip -selection clipboard"
    alias paste="xclip -o -selection clipboard"
    em () { emacs $* & disown }
    ;;
esac

alias racket="/Applications/Racket\ v6.8/bin/racket"

# Git
alias gs='git status '
alias ga='git add .'
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gds='git diff --staged'
alias gr='git remote -v'
alias gco="git checkout"
alias gst="git status"
alias gpum="git pull upstream master"
alias gpom="git pull origin master"
alias gpod="git pull origin develop"
alias hb="hub browse"

alias be="bundle exec"
alias bl="bundle --local"

alias la="ls -lah"
alias lf="rlwrap lein figwheel dev test" #figwheel readline mode
alias ltr="lein test-refresh"
alias ntw="nosetests --with-watch -s"
alias nt="nosetests"
alias pd="pushd $*"
alias po="popd $*"
alias hist="cat ~/.zsh_history"

# GOLANG
export GOPATH=$HOME/go
export PATH=$PATH:"$GOPATH/bin"
export CC="gcc"

alias bounce_dns="sudo killall -HUP mDNSResponder"
alias turing="cd ~/Turing"
alias code="cd ~/code"
alias clj="cd ~/code/clojure"

# hdfs aliases
alias hfs='hadoop fs'
alias hls='hadoop fs -ls'
alias htx='hadoop fs -text'
alias hc='hadoop fs -cat'

# Run spark docker devbox
alias sparkdev='docker run -v ~/code:/code -ti --name dev -e "START_SCRIPT=http://resources.prod.factual.com/services/hadoop/cdh5/scripts/get_configs.sh" spark-devbox:latest /sbin/my_init -- /sbin/setuser `whoami` /bin/bash -l'

if [[ -a ~/.secrets.sh ]]; then
  source ~/.secrets.sh
fi

if [[ -a ~/.factual.sh ]]; then
  source ~/.factual.sh
fi

function killgrep {
  kill $(ps aux | grep $1 | grep -v "grep" | awk '{print $2}')
}

function alert {
  terminal-notifier -message $1
}

function pgrep {
    ps aux | grep $1 | grep -v "grep"
}

function fetch_tile {
  TILE=$1
  TILE_VERSION=${2:-v3.0.0}
  if ! [[ -a /tmp/tiles/$TILE_VERSION/$TILE ]]; then
    aws s3 --quiet cp s3://factual-us-west-1-tiles/$TILE_VERSION/$1 /tmp/tiles/$TILE_VERSION/$TILE
  fi
  cat /tmp/tiles/$TILE_VERSION/$TILE | \
    java -cp ~/tile_builder.jar com.factual.tile.builder.mapreduce.outputFormat.TileReader
}

function ts_tile {
  TILE=$1
  curl http://marathon-services.la.prod.factual.com:31012/tiles/v3.0.1-all/$TILE | \
    java -cp ~/tile_builder.jar com.factual.tile.builder.mapreduce.outputFormat.TileReader
}

function 312tile {
  curl --compressed http://marathon-services.la.prod.factual.com:31015/tiles/$1 > /tmp/$1
  cat /tmp/$1
}

function decode_tile {
  cat $1 | java -cp ~/tile_builder.jar com.factual.tile.builder.mapreduce.outputFormat.TileReader
}

# curl -H "Content-Type: application/octet-stream" -X POST shapes-ui.prod.factual.com/api/tiles/decode --data-binary @-


function gjtile {
  curl -H "Content-Type: application/octet-stream" \
       -H "Transfer-Encoding: Chunked" \
       -H "Expect:" \
       --no-keepalive \
       -X POST shapes-ui.prod.factual.com/api/tiles/decode \
       --data-binary "@$1"
}

function fetch_uuid {
  UUID=$1
  INDEX=${2:-Iw1HPj}
  curl -s "http://constellation.prod.factual.com/$INDEX/live/summaries/query?q=factual_id:$UUID" \
    | jq -cM ".response.docs[0]"
}

function emrestart {
    if pgrep "emacs.*daemon" > /dev/null
    then
        echo "killing emacs daemon process"
        $EMACS_BIN_DIR/emacsclient -e "(kill-emacs)"
    fi
  launchctl unload "$HOME/Library/LaunchAgents/emacsserver.plist" &&
  launchctl load "$HOME/Library/LaunchAgents/emacsserver.plist"
}

function emstop {
    if pgrep "emacs.*daemon" > /dev/null
    then
        echo "killing emacs daemon process"
        $EMACS_BIN_DIR/emacsclient -e "(kill-emacs)"
    fi
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

case `uname` in
  Linux)
  export DOCKER_TLS_VERIFY="1"
  export DOCKER_HOST="tcp://192.168.99.100:2376"
  export DOCKER_CERT_PATH="/home/worace/.docker/machine/machines/default"
  export DOCKER_MACHINE_NAME="default"
    ;;
esac

# Chruby for ruby version management
function loadChruby {
    if [[ -a $1/chruby.sh ]]; then
        source $1/chruby.sh
        source $1/auto.sh
        chruby 2.3
    fi
}
loadChruby '/usr/local/share/chruby'
loadChruby '/usr/share/chruby'

if type ruby > /dev/null; then
  # For adding system-ruby gem dir to path
  PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
fi

function scrape {
	wget \
		--recursive \
		--no-clobber \
		--page-requisites \
		--html-extension \
		--convert-links \
		--restrict-file-names=windows \
		--domains website.org \
		--no-parent \
			$1
}

function countloc { find $1 -name "*" -type f | xargs wc -l | sort }
alias rake='noglob rake'

# export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh --no-use"  # This loads nvm
alias nvmu='source ~/.nvm/nvm.sh && nvm use'


HISTSIZE=100000
SAVEHIST=100000
HISTFILE="$HOME/.zsh_history"
# setopt hist_ignore_dups # ignore duplication command history list
# setopt append_history
# setopt extended_history
# setopt hist_expire_dups_first
# setopt hist_ignore_space
# setopt hist_verify
# setopt inc_append_history
setopt share_history

alias rl="source ~/.zshrc"

[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

# Custom thrift version needed for engine
PATH="/usr/local/opt/thrift@0.90/bin:$PATH"

# Hadoop + Kerberos Local Setup
export HADOOP_CONF_DIR=/etc/hadoop/conf
PATH=/usr/local/Cellar/krb5/1.14.4/bin:$PATH

export HADOOP_INSTALL=/usr/local/Cellar/hadoop/2.8.0
export HADOOP_CLASSPATH=$HADOOP_CLASSPATH:$HADOOP_INSTALL/lib/hadoop-lzo-0.4.21-SNAPSHOT.jar
export HADOOP_OPTS="$HADOOP_OPTS -Djava.library.path=$HADOOP_INSTALL/lib/lzo/Mac_OS_X-x86_64-64:$HADOOP_INSTALL/lib/native"

# 'z' directory-switching utility
# https://github.com/rupa/z
. $HOME/dotfiles/z.sh
alias j=z

export ONEPASSWORD_KEYCHAIN=$HOME/Dropbox/1Password/1Password.agilekeychain/
PATH=$PATH:$HOME/.local/bin
if [[ -a $HOME/.fastlane/bin ]]; then
  # export PATH=$PATH:"$HOME/.fastlane/bin"
fi
# tmux
alias tn="tmux new -s"
alias tls="tmux ls"
alias tl="tmux ls"
alias tr="tmux a -t"
