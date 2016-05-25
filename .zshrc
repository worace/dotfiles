# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="worace"
export BUNDLER_EDITOR="emacs"
export EDITOR="emacsclient -t"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

case `uname` in
  Darwin)
    #increase keyrepeat speed beyond os x allowed maximum
    eval "defaults write NSGlobalDomain KeyRepeat -int 0"
    em () {
        if [ "$#" -ne 0 ];
        then
            /usr/local/Cellar/emacs/24.5/bin/emacsclient -c -n $*
        else
            /usr/local/Cellar/emacs/24.5/bin/emacsclient -c -n "~/scratch"
        fi
    }
    ;;
  Linux)
    alias copy="xclip -selection c"
    em () { emacs $* & disown }
    ;;
esac

alias racket="/Applications/Racket\ v6.2/bin/racket"

# Git
alias gs='git status '
alias ga='git add .'
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gr='git remote -v'
alias gco="git checkout"
alias gst="git status"
alias gpum="git pull upstream master"
alias gpom="git pull origin master"
alias hb="hub browse"

alias be="bundle exec"
alias bl="bundle --local"

alias lf="rlwrap lein figwheel dev test" #figwheel readline mode

# GOLANG
export GOPATH=$HOME/go
export GOROOT=$HOME/go
export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"
alias gp="cd $GOPATH/src/github.com/worace"

export PATH="/usr/local/heroku/bin:$PATH"
export CC="gcc"

#add homebrew bin directory to path
export PATH="/usr/local/bin:$PATH"

alias bounce_dns="sudo killall -HUP mDNSResponder"
alias turing="cd ~/Turing"
alias code="cd ~/code"
alias clj="cd ~/code/clojure"

if [[ -a ~/.secrets.sh ]]; then
  source ~/.secrets.sh
fi

function killgrep {
  kill $(ps aux | grep $1 | grep -v "grep" | awk '{print $2}')
}

function pgrep {
    ps aux | grep $1 | grep -v "grep"
}

function emrestart {
    if pgrep "emacs.*daemon" > /dev/null
    then
        echo "killing emacs daemon process"
        emacsclient -e "(kill-emacs)"
    fi
  launchctl unload "/Users/worace/Library/LaunchAgents/emacsserver.plist" &&
  launchctl load "/Users/worace/Library/LaunchAgents/emacsserver.plist"
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

# Boot2Docker client config
# export DOCKER_HOST=tcp://192.168.59.103:2376
# export DOCKER_CERT_PATH=/Users/worace/.boot2docker/certs/boot2docker-vm
# export DOCKER_TLS_VERIFY=0

# docker-machine env default
export DOCKER_TLS_VERIFY="1"
export DOCKER_HOST="tcp://192.168.99.100:2376"
export DOCKER_CERT_PATH="/Users/worace/.docker/machine/machines/default"
export DOCKER_MACHINE_NAME="default"

# Chruby for ruby version management
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
chruby 2.2.2

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

function countloc {
	find $1 -name "*" | xargs wc -l
}
alias rake='noglob rake'
