## OS X Setup

```
# Homebres
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

brew install hub
brew install openjdk
brew install zsh

ssh-keygen -t rsa -b 4096
cat ~/.ssh/id_rsa.pub | pbcopy # -> to github settings page and add ssh key

hub clone worace/dotfiles
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/emacs/ ~/.emacs.d
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/tmux-osx.conf ~/.tmux-system.conf
ln -s ~/dotfiles/basic.tmuxtheme ~/.tmux.theme


brew tap railwaycat/emacsmacport
brew install emacs-mac
brew install rg

brew tap homebrew/cask-fonts
brew cask install font-source-code-pro

# OpenJDK 8 (sbt compatible)
brew tap adoptopenjdk/openjdk
brew cask install adoptopenjdk8

brew install postgresql
brew services start postgresql
createdb
psql -c "CREATE USER postgres WITH SUPERUSER PASSWORD 'password';"

brew install ruby-install
ruby-install ruby-2.6

brew install chruby

brew install dbmate

# Metals:
# https://gist.github.com/worace/ed4b66bc6269292a1614228b7c705c3c
```

`~/.localrc`:

```
export PATH="/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home/bin:$PATH"
export JAVA_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"
source /usr/local/opt/chruby/share/chruby/auto.sh
```
