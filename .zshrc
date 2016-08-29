# Check if Command is available:
# if ! type "$foobar_command_name" > /dev/null; then
    # do stuff
# fi

# Check if file exists:
# if [[ -a file ]]; then
#   do stuff
# fi

export BUNDLER_EDITOR="emacs"
export EDITOR="emacsclient -t"

if [[ -a ~/dotfiles/antigen.zsh ]]; then
    source ~/dotfiles/antigen.zsh
    autoload -U colors && colors
    setopt promptsubst
    antigen bundle git
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen theme ~/dotfiles worace
fi

if [[ -a /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi


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
alias gds='git diff --staged'
alias gr='git remote -v'
alias gco="git checkout"
alias gst="git status"
alias gpum="git pull upstream master"
alias gpom="git pull origin master"
alias hb="hub browse"

alias be="bundle exec"
alias bl="bundle --local"

alias la="ls -lah"
alias lf="rlwrap lein figwheel dev test" #figwheel readline mode
alias ltr="lein test-refresh"
alias nt="nosetests --with-watch -s"

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
  launchctl unload "$HOME/Library/LaunchAgents/emacsserver.plist" &&
  launchctl load "$HOME/Library/LaunchAgents/emacsserver.plist"
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

# Boot2Docker client config
# export DOCKER_HOST=tcp://192.168.59.103:2376
# export DOCKER_CERT_PATH=/Users/worace/.boot2docker/certs/boot2docker-vm
# export DOCKER_TLS_VERIFY=0

case `uname` in
  Darwin)
  # docker-machine env default
  export DOCKER_TLS_VERIFY="1"
  export DOCKER_HOST="tcp://192.168.99.100:2376"
  export DOCKER_CERT_PATH="/Users/worace/.docker/machine/machines/default"
  export DOCKER_MACHINE_NAME="default"
    ;;
  Linux)
  export DOCKER_TLS_VERIFY="1"
  export DOCKER_HOST="tcp://192.168.99.100:2376"
  export DOCKER_CERT_PATH="/home/worace/.docker/machine/machines/default"
  export DOCKER_MACHINE_NAME="default"
    ;;
esac

# Chruby for ruby version management
if [[ -a /usr/local/share/chruby/chruby.sh ]]; then
    source /usr/local/share/chruby/chruby.sh
    source /usr/local/share/chruby/auto.sh
    chruby 2.3.1
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

function countloc {
	find $1 -name "*" | xargs wc -l
}
alias rake='noglob rake'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# Add Python local (user) installs to Path:
export PATH="/home/worace/.local/bin:$PATH"

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
