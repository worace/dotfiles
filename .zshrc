# zmodload zsh/zprof
# Check if Command is available:
# if ! type "$foobar_command_name" > /dev/null; then
    # do stuff
# fi

# Check if file exists:
# if [[ -a file ]]; then
#   do stuff
# fi

# Check for TTY
# if [[ "$(tty)" == '/dev/tty1' ]]; then
# exec startx; exit
# fi

# source $HOME/.profile

if [[ -d /usr/local/opt/emacs-mac/bin ]]; then
  EMACS_BIN_DIR=/usr/local/opt/emacs-mac/bin
else
  EMACS_BIN_DIR=/usr/local/bin
fi
export BUNDLER_EDITOR="$EMACS_BIN_DIR/emacs"
export EDITOR="$EMACS_BIN_DIR/emacs"

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
    export VIRTUALENVWRAPPER_PYTHON=$(which python3)
    source $HOME/.local/bin/virtualenvwrapper_lazy.sh
fi

case `uname` in
  Darwin)
    #increase keyrepeat speed beyond os x allowed maximum
    eval "defaults write NSGlobalDomain KeyRepeat -int 1"
    alias vsc=/usr/local/bin/code
    alias paste=pbpaste
    alias copy=pbcopy
    function alert {
      terminal-notifier -message $1
    }
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
    export TERM=xterm-color
    function alert {
      notify-send $1
    }
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
alias hb="gh repo view --web"

alias be="noglob bundle exec"
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
alias hfs='noglob hadoop fs'
alias hls='noglob hadoop fs -ls'
alias htx='noglob hadoop fs -text'
alias hc='noglob hadoop fs -cat'

# Run spark docker devbox
alias sparkdev='docker run -v ~/.docker_bash_history:/root/.bash_history -v ~/code:/code -ti --rm --name dev -e "START_SCRIPT=http://resources.prod.factual.com/services/hadoop/cdh5/scripts/get_configs.sh" factual/docker-cdh5-devbox /sbin/my_init -- /sbin/setuser $(whoami) /bin/bash -l'

if [[ -a ~/.secrets.sh ]]; then
  source ~/.secrets.sh
fi

if [[ -a ~/.localrc ]]; then
  source ~/.localrc
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

# Chruby for ruby version management
function loadChruby {
    if [[ -a $1/chruby.sh ]]; then
        source $1/chruby.sh
        source $1/auto.sh
        # chruby 2.6
    fi
}

loadChruby '/usr/local/share/chruby'
loadChruby '/usr/share/chruby'
loadChruby '/opt/homebrew/opt/chruby/share/chruby'

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

function countloc { find $1 -name "*" -type f | xargs wc -l | sort -n }

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# alias nvmu='source ~/.nvm/nvm.sh && nvm use'

# Elixir
export ERL_AFLAGS="-kernel shell_history enabled"
export PATH="$PATH:$HOME/.mix/escripts"
alias mc="iex -S mix"
alias mps="mix phx.server"
alias mpr="mix phx.routes"
alias mdg="mix deps.get"
alias mt="mix test"
alias mpr="mix phx.routes"

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
setopt interactivecomments

alias rl="source ~/.zshrc"

[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

# Custom thrift version needed for engine
PATH="/usr/local/opt/thrift@0.90/bin:$PATH"

# Hadoop + Kerberos Local Setup
export HADOOP_CONF_DIR=/etc/hadoop/conf
export CDH_VERSION="5.13"
PATH=/usr/local/Cellar/krb5/1.14.4/bin:$PATH

case `uname` in
  Linux)
    export HADOOP_HOME=/usr/lib/hadoop
    export SPARK_HOME=/opt/spark
    export PATH=$SPARK_HOME/bin:$PATH
    alias yrn=/usr/lib/hadoop-yarn/bin/yarn
    ;;
esac

function yarnlogs {
  /usr/lib/hadoop-yarn/bin/yarn logs -applicationId $1 > /tmp/$1.log
}

function yarnkill {
  yrn application -kill $1
}

# 'z' directory-switching utility
# https://github.com/rupa/z
. $HOME/dotfiles/z.sh
alias j=z

PATH=$PATH:$HOME/.local/bin

# tmux
alias tn="tmux new -s"
alias tls="tmux ls"
alias tl="tmux ls"
alias ta="tmux a -t"

alias i="sudo apt install"

function throughput {
  tail -f $1 | pv -lrtab > /dev/null
}

alias jq="noglob jq -cr"
alias curl="noglob curl"

export PATH="$HOME/.cargo/bin:$PATH"
alias ldappw="op get item \"LDAP Factual\" | jq -cr .details.fields[0].value | copy"
alias jqc="jq -cr ."
alias jqcp="paste | jqc | copy"
alias jqp="\jq "
alias rake="noglob rake"
alias countries="ruby -e 'require \"factual_countries\"; FactualCountries.all.keys.each { |c| puts c }'"
alias count="sort | uniq -c | sort -nr"

export SBT_OPTS="-Xmx32G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -XX:ReservedCodeCacheSize=512M"

function dockerprune {
  docker image prune -f
  docker container prune -f
}

function okiedokie {
  mpg123 $HOME/data/okiedokie.mp3 2> /dev/null
}

function csv2json {
  ruby -r csv -r json -e 'CSV.new(STDIN, headers: true).each { |r| puts r.to_h.to_json }'
}

function json2csv {
  ruby -r set -r json -r csv -e 'rows = STDIN.readlines.map { |l| JSON.parse(l) }; keys = rows.map { |r| r.keys.to_set }.reduce(:union).to_a.sort; arrs = rows.map { |r| keys.map { |k| r[k] } }; CSV(STDOUT) { |csv| csv << keys; arrs.each { |a| csv << a } }'
}

export PATH="$HOME/.npm-global/bin:$PATH"

# zprof


function jqm {
  jq "select(.$1 == \"$2\")"
}

function kubeexec {
  kubectl --namespace airflow-prod exec $1 -it /bin/bash
}

if type "frum" > /dev/null; then
  eval "$(frum init)"
fi

if [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi
