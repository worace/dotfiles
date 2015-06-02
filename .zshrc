# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="worace"
export BUNDLER_EDITOR="mvim"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want disable red dots displayed while waiting for completion
# DISABLE_COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
eval "defaults write NSGlobalDomain KeyRepeat -int 0"

#alias em='/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs &'
em () { /usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs $* & }
alias c="/usr/bin/open -a '/Applications/Google Chrome.app'"

# Git
alias gs='git status '
alias ga='git add .'
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gk='gitk --all&'
alias gx='gitx --all'
alias gr='git remote -v'
alias gbr="git branch"
alias gco="git checkout"
alias gst="git status"
alias gpum="git pull upstream master"
alias gpom="git pull origin master"

alias be="bundle exec"
alias bl="bundle --local"


#servers n stuff
alias pg='postgres -D ~/postgres/ &'
alias start_redis="redis-server /usr/local/etc/redis.conf"
alias start_memcached='/usr/bin/memcached -d'
alias faye='bundle exec rackup faye.ru -s thin -E production -p 9999'

#worace at work
if [ -f ~/.lsrc.sh ]; then
  source ~/.lsrc.sh
else
  export RBENV_ROOT=/usr/local/var/rbenv
  if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
fi

export GOPATH=$HOME/go
export GOROOT=`go env GOROOT`
export PATH="$GOPATH/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"


### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
#RBENV
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
export CC="gcc"

#pick up some extra homebrew installs
export PATH="/usr/local/bin:$PATH"

##prevent scala from hanging
alias scala="scala -nocompdaemon"

alias bubbles='bundle'

if [[ -a ~/.clever.sh ]]; then
  source ~/.clever.sh
fi
if [[ -a ~/.untappd.sh ]]; then
  source ~/.untappd.sh
fi
if [[ -a ~/.sello.sh ]]; then
  source ~/.sello.sh
fi

alias bounce_dns="sudo killall -HUP mDNSResponder"
alias turing="cd ~/code/Turing"
alias code="cd ~/code"
alias so="cd ~/code/sello"
alias gp="cd $GOPATH/src/github.com/worace"

source ~/.secrets.sh

export MANDRILL_USERNAME="horace.d.williams@gmail.com"
export MANDRILL_KEY="NyMQFrKPBDd4o-RARly3rA"

function killgrep {
  kill $(ps aux | grep $1 | grep -v "grep" | awk '{print $2}')
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"
export TURING_SLACK_TOKEN="xoxp-2329094327-2746662827-3907823656-437836"
