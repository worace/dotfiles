# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="worace"
export BUNDLER_EDITOR="emacs"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

#increase keyrepeat speed beyond os x allowed maximum
case `uname` in
  Darwin)
    eval "defaults write NSGlobalDomain KeyRepeat -int 0"
    ;;
  Linux)
    alias copy="xclip -selection c"
    ;;
esac

em () { open -a /usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs $* }
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
#export GOPATH=$HOME/go
#export GOROOT=`go env GOROOT`
#export PATH="$GOPATH/bin:$PATH"
#export PATH="$PATH:/usr/local/opt/go/libexec/bin"
#alias gp="cd $GOPATH/src/github.com/worace"

### Added by the Heroku Toolbelt
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

export MANDRILL_USERNAME="horace.d.williams@gmail.com"
export MANDRILL_KEY="NyMQFrKPBDd4o-RARly3rA"

function killgrep {
  kill $(ps aux | grep $1 | grep -v "grep" | awk '{print $2}')
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

# Boot2Docker client config
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/worace/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=0

# Chruby for ruby version management
source /usr/local/share/chruby/chruby.sh
chruby 2.2.2
