dotfiles
========

## Setup Notes

### 1. Installing Git

```
sudo apt-add-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git
```

### 2. SSH Keys

Either copy them from an existing machine
or generate new ones with:

```
ssh-keygen -t rsa
```

Then need to add these to Github by logging in
and going to profile -> ssh keys

### 3. Hub Github Client

```
sudo add-apt-repository ppa:cpick/hub
sudo apt-get update sudo apt-get install hub
```

### 4. Cloning Dotfiles

```
cd ~
hub clone worace/dotfiles
```

### 5. Symlinks

```
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.lein ~/.lein
ln -s ~/dotfiles/.gemrc ~/.gemrc
ln -s ~/dotfiles/.emacs.d ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
# emacsserver launch on OS X via launchd:
ln -s ~/dotfiles/emacsserver.plist ~/Library/LaunchAgents/emacsserver.plist
```

### 6. ZSH / Oh-my-zsh

```
sudo apt-get install zsh
chsh -s /bin/zsh $(whoami) # takes a logout to take effect
```

### 7. Chruby / Ruby Install

```
cd ~/Downloads
wget -O chruby-0.3.9.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz
tar -xzvf chruby-0.3.9.tar.gz
cd chruby-0.3.9/
sudo make install

wget -O ruby-install-0.5.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.5.0.tar.gz
tar -xzvf ruby-install-0.5.0.tar.gz
cd ruby-install-0.5.0/
sudo make install

ruby-install ruby 2.2.2

gem install bundler
gem install pry
```

### 8. Emacs

```
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs25
```

### 9. JDK / Clojure

```
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer
```

__Leiningen__

```
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/local/bin/lein
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > lein
sudo mv lein /usr/local/bin
sudo chmod a+x /usr/local/bin/lein
lein repl
```

### 10. Dropbox

Install with the ubuntu installer [here](https://www.dropbox.com/install?os=lnx)

python-gpgme is also required for some verification stuff:

```
sudo apt-get install python-gpgme
```

### 11. WINE / 1password

#### Command Line Client

```
sudo apt install -y python-pip
pip install --user 1pass python-Levenshtein
```

Somewhat miraculously got this working thanks
to [this tutorial](https://discussions.agilebits.com/discussion/42126/making-1password-work-in-ubuntu-14-04)

```
sudo dpkg --add-architecture i386
wget -nc https://dl.winehq.org/wine-builds/Release.key
sudo apt-key add Release.key
sudo apt-add-repository https://dl.winehq.org/wine-builds/ubuntu/
sudo apt-get update
sudo apt-get install --install-recommends winehq-stable
sudo apt install ttf-mscorefonts-installer
sudo apt-get install -y winehq-staging
```

Use standard windows installer [here](https://agilebits.com/onepassword/windows)

### 12. Redshift

```
sudo apt-get install redshift redshift-gtk
```

## Misc Packages

```
sudo apt-get install -y redshift redshift-gtk silversearcher-ag htop pv jq caffeine tmux gksu
```

**Dev Packages**

```
sudo apt-get install -y cmake cmake-curses-gui make \
    libexpat1-dev zlib1g-dev libbz2-dev libprotozero-dev libosmium-dev rapidjson-dev \
    libpthread-stubs0-dev libsqlite3dev
```

### Font

```
cd ~/Downloads
wget https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
unzip 1.050R-it.zip
mkdir ~/.fonts
cp source-code-pro-*-it/OTF/*.otf ~/.fonts/
fc-cache -f -v
```

### Slack Client

[ScudCloud](https://github.com/raelgc/scudcloud)

### Gitignore

I have this in dotfiles as `.system_gitignore` so i can have a special gitignore for this actual repo. Thus to link this one use `ln -s ~/dotfiles/.system_gitignore ~/.gitignore`

### GTK Theme

```
sudo add-apt-repository ppa:system76/pop
sudo apt update
sudo apt install -y pop-theme gnome-tweak-tool
# Theme: Pop-dark
# Icons: Pop
```

### UI Settings

```
gsettings set com.canonical.Unity.Launcher launcher-position Bottom
```

### Nvidia

```
# restart to take effect
sudo apt-get install -y nvidia-375
```

### Postgres

```
sudo apt-get install -y postgresql libpqdev
sudo systemctl enable postgresql
sudo systemctl start postgresql
sudo -u postgres psql
# set password with \password
sudo -u postgres createuser --superuser worace
createdb
```

### Spotify

```
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update
sudo apt-get install -y spotify-client
```

## Setting up emacs-mac on OS X

This is a fork of the default emacs homebrew formula which includes some mac-specific tweaks, as well as a few patches from the emacs devel branch that puts it ahead of the default emacs formula in terms of new features.

At the moment the one I'm most interested is the patch for improved subprocess forking in emacs which makes performance much better for things like magit.

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-no-title-bars --with-modern-icon
```

https://emacs.stackexchange.com/questions/141/emacsdaemon-and-emacsclient-on-mac
