#!/usr/bin/env bash

echo "Installing Git"
sudo apt-add-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git

# echo "Generating SSH Key"
# ssh-keygen -t rsa

echo "*** Go add SSH key to github account ***"

sudo add-apt-repository ppa:cpick/hub
sudo apt-get update
sudo apt-get install -y hub

ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.lein ~/.lein
ln -s ~/dotfiles/.gemrc ~/.gemrc
ln -s ~/dotfiles/emacs ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
