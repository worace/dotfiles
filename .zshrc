# Check if Command is available:
# if ! type "$foobar_command_name" > /dev/null; then
    # do stuff
# fi

# Check if file exists:
# if [[ -a file ]]; then
#   do stuff
# fi

source $HOME/.profile

export BUNDLER_EDITOR="emacs"
export EDITOR="emacsclient -t"

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
    # eval "defaults write NSGlobalDomain KeyRepeat -int 1"
    em () {
        if [ "$#" -ne 0 ];
        then
            /usr/local/Cellar/emacs/25.1/bin/emacsclient -c -n $*
        else
            /usr/local/Cellar/emacs/25.1/bin/emacsclient -c -n "~/scratch.org"
        fi
    }
    ;;
  Linux)
    alias copy="xclip -selection clipboard"
    alias paste="xclip -o -selection clipboard"
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
alias gpod="git pull origin devel"
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

# GOLANG
export GOPATH=$HOME/go
export GOROOT=$HOME/go
alias gp="cd $GOPATH/src/github.com/worace"
export CC="gcc"


alias bounce_dns="sudo killall -HUP mDNSResponder"
alias turing="cd ~/Turing"
alias code="cd ~/code"
alias clj="cd ~/code/clojure"

if [[ -a ~/.secrets.sh ]]; then
  source ~/.secrets.sh
fi

if [[ -a ~/.factual.sh ]]; then
  source ~/.factual.sh
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

function emstop {
    if pgrep "emacs.*daemon" > /dev/null
    then
        echo "killing emacs daemon process"
        emacsclient -e "(kill-emacs)"
    fi
}

export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"
export MIT_SCHEME_EXE="/usr/local/scheme"

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
function loadChruby {
    if [[ -a $1/chruby.sh ]]; then
        source $1/chruby.sh
        source $1/auto.sh
        chruby 2.3.3
    fi
}
loadChruby '/usr/local/share/chruby'
loadChruby '/usr/share/chruby'

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

function countloc { find $1 -name "*" -type f | xargs wc -l }
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
