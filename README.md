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
sudo apt-get update
sudo apt-get install -y hub
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
ln -s ~/dotfiles/emacs ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
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

### 9. JDK / Clojure / Scala

```
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer scala maven
```

__Leiningen__

```
mkdir ~/.local/bin
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/.local/bin/lein
sudo chmod a+x ~/.local/bin/lein
lein repl

```

### Spark/Hadoop

```
cd ~/Downloads
wget http://mirror.jax.hugeserver.com/apache/hadoop/common/hadoop-2.8.1/hadoop-2.8.1.tar.gz
tar -xzf hadoop-2.8.1.tar.gz
sudo mv hadoop-2.8.1 /usr/local/hadoop
wget https://d3kbcqa49mib13.cloudfront.net/spark-2.2.0-bin-hadoop2.7.tgz
tar -xzf spark-2.2.0-bin-hadoop2.7
sudo mv spark-2.2.0-bin-hadoop2.7 /usr/local/spark

sudo apt-get install -y liblzo2-dev
git clone git@github.com:twitter/hadoop-lzo.git
cd hadoop-lzo
mvn clean package
sudo cp target/hadoop-lzo-0.4.21-SNAPSHOT.jar /usr/local/hadoop/lib/
sudo cp target/native/Linux-amd64-64/libgplcompression.la /usr/local/hadoop/lib/native
echo "spark.jars /usr/local/hadoop/lib/hadoop-lzo-0.4.21-SNAPSHOT.jar" >> /usr/local/spark/conf/spark-defaults.conf
```

### 10. Dropbox

Install with the ubuntu installer [here](https://www.dropbox.com/install?os=lnx)

### 11. WINE / 1password

#### Command Line Client

```
sudo apt install -y python-pip
pip install --user 1pass python-Levenshtein
```

Somewhat miraculously got this working thanks
to [this tutorial](https://discussions.agilebits.com/discussion/42126/making-1password-work-in-ubuntu-14-04)

```
sudo add-apt-repository ppa:ubuntu-wine/ppa
sudo dpkg --add-architecture i386
sudo apt install wine winetricks ttf-mscorefonts-installer
# 32-bit wine
WINEARCH=win32 WINEPREFIX=~/.wine wine wineboot
winetricks dotnet46 corefonts
winetricks mono28
```

Use standard windows installer [here](https://agilebits.com/onepassword/windows)

## Misc Packages

```
sudo apt-get install -y redshift redshift-gtk silversearcher-ag htop pv jq caffeine tmux gksu \
                        vim psensor xclip tree ttf-ancient-fonts net-tools scrot curl
```

**Dev Packages**

```
sudo apt-get install -y cmake cmake-curses-gui make \
    libexpat1-dev zlib1g-dev libbz2-dev libprotozero-dev libosmium-dev rapidjson-dev \
    libpthread-stubs0-dev libsqlite3-dev python3.6 libpython3.6-dev lzop \
    libicu-dev npm nodejs postgis mongodb-clients libgtest-dev
```

### Font

```
cd ~/Downloads
wget https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
unzip 1.050R-it.zip
mkdir ~/.fonts
cp source-code-pro-*-it/OTF/*.otf ~/.fonts/
fc-cache -f -v

sudo apt install -y fonts-font-awesome
```

### Slack Client

[Official Linux Client](https://slack.com/downloads/linux)

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

### Gnome Terminal Theme

https://github.com/metalelf0/gnome-terminal-colors

```
cd /tmp
git clone git@github.com:metalelf0/gnome-terminal-colors.git
cd gnome-terminal-colors
chmod +x install.sh
./install.sh
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

### Firefox (beta)

```
sudo add-apt-repository ppa:mozillateam/firefox-next
sudo apt-get update
sudo apt-get install -y firefox
```

### Key Repeat Speed

### Window Tiling Hotkeys (a la Divvy)

```
sudo apt install -y compizconfig-settings-manager
```

Use Grid settings to set hotkeys for maximize and maximize <bottom, top, left, right>

### Docker

https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/#docker-ee-customers

```
sudo apt-get remove docker docker-engine docker.io

sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo apt-key fingerprint 0EBFCD88

sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"

sudo apt-get update

sudo apt-get install docker-ce
```

### Drake

```
curl https://raw.githubusercontent.com/Factual/drake/master/bin/drake > ~/.local/bin/drake
chmod +x ~/.local/bin/drake
```

### Tmux

```
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/basic.tmuxtheme ~/.tmux.theme
```

## Setting up emacs-mac on OS X

This is a fork of the default emacs homebrew formula which includes some mac-specific tweaks, as well as a few patches from the emacs devel branch that puts it ahead of the default emacs formula in terms of new features.

At the moment the one I'm most interested is the patch for improved subprocess forking in emacs which makes performance much better for things like magit.

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-no-title-bars --with-modern-icon
```

https://emacs.stackexchange.com/questions/141/emacsdaemon-and-emacsclient-on-mac

## GTK Theme

```
mkdir ~/.themes
wget -O ~/Downloads/Ant.zip https://github.com/eliverlara/ant/archive/v1.1.0.zip
unzip -d ~/.themes/Ant ~/Downloads/Ant.zip
gsettings set org.gnome.desktop.interface gtk-theme "Ant"
gsettings set org.gnome.desktop.wm.preferences theme "Ant"
```

## Thrift 0.9.3

```
sudo apt-get install automake bison flex g++ git libboost-all-dev libevent-dev libssl-dev libtool make pkg-config
cd /tmp
wget https://github.com/apache/thrift/archive/0.9.3.tar.gz
tar -xzf 0.9.3.tar.gz
cd thrift-0.9.3
./bootstrap.sh
./configure
make
sudo make install
```
