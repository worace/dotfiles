dotfiles
========

## Setup Notes

### SSH Key

Either copy them from an existing machine
or generate new ones with:

```
ssh-keygen -t rsa
```

Then need to add these to Github by logging in
and going to profile -> ssh keys

###  Github CLI

https://github.com/cli/cli/blob/trunk/docs/install_linux.md

### 4. Cloning Dotfiles

```
cd ~
gh repo clone dotfiles
```

### Symlinks

```
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.lein ~/.lein
ln -s ~/dotfiles/.gemrc ~/.gemrc
ln -s ~/dotfiles/emacs ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/tmux-linux.conf ~/.tmux-system.conf
ln -s ~/dotfiles/basic.tmuxtheme ~/.tmux.theme
mkdir -p ~/.config/termite
ln -s ~/dotfiles/termite-gruvbox-dark ~/.config/termite/config
```

### Base Packages

```
sudo apt install emacs zsh tmux ripgrep xclip ffmpeg pv jq
```

### ZSH

```
chsh -s /bin/zsh $(whoami) # takes a logout to take effect
```

### Spark/Hadoop

```
cd ~/Downloads
wget http://mirror.jax.hugeserver.com/apache/hadoop/common/hadoop-2.8.1/hadoop-2.8.1.tar.gz
tar -xzf hadoop-2.8.1.tar.gz
sudo mv hadoop-2.8.1 /usr/local/hadoop

wget https://d3kbcqa49mib13.cloudfront.net/spark-2.2.0-bin-hadoop2.7.tgz
tar -xzf spark-2.2.0-bin-hadoop2.7.tgz
sudo mv spark-2.2.0-bin-hadoop2.7 /usr/local/spark

sudo apt-get install -y liblzo2-dev

git clone git@github.com:twitter/hadoop-lzo.git
cd hadoop-lzo
mvn clean package
sudo cp target/hadoop-lzo-0.4.21-SNAPSHOT.jar /usr/local/hadoop/lib/
sudo cp target/native/Linux-amd64-64/libgplcompression.la /usr/local/hadoop/lib/native
echo "spark.jars /usr/local/hadoop/lib/hadoop-lzo-0.4.21-SNAPSHOT.jar" >> /usr/local/spark/conf/spark-defaults.conf

sudo apt-get install krb5-user
```

## Misc Packages

```
sudo apt-get install -y redshift redshift-gtk silversearcher-ag htop caffeine tmux gksu \
                        vim psensor xclip tree ttf-ancient-fonts net-tools scrot curl rofi gdal-bin whois
```

### Font

https://github.com/adobe-fonts/source-code-pro/releases

```
cd ~/Downloads
wget https://github.com/adobe-fonts/source-code-pro/releases/download/2.038R-ro%2F1.058R-it%2F1.018R-VAR/OTF-source-code-pro-2.038R-ro-1.058R-it.zip
unzip *.zip
mkdir ~/.fonts
cp *.otf ~/.fonts/
fc-cache -f -v
```

### Slack Client

[Official Linux Client](https://slack.com/downloads/linux)

### Postgres

```
sudo apt-get install -y postgresql libpq-dev
sudo systemctl enable postgresql
sudo systemctl start postgresql
sudo -u postgres psql
# set password with \password
sudo -u postgres createuser --superuser $(whoami)
createdb
```
### Termite Terminal

Follow script [here](https://github.com/Corwind/termite-install/blob/master/termite-install.sh)

Theme from [here](https://github.com/morhetz/gruvbox-contrib/blob/master/termite/gruvbox-dark)

### I3 Gaps

[Source](https://github.com/Airblader/i3/wiki/Compiling-&-Installing)

```
cd /tmp
git clone https://www.github.com/Airblader/i3 i3-gaps
cd i3-gaps

# Install Deps
sudo apt-get install -y libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev \
                        libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev \
                        libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev automake

# compile & install
autoreconf --force --install
rm -rf build/
mkdir -p build && cd build/

../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers
make
sudo make install
```

Spotify UI scaling: https://community.spotify.com/t5/Desktop-Linux/Linux-client-barely-usable-on-HiDPI-displays/td-p/1067272

## Rust

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup toolchain nightly
rustup default nightly
rustup component add rls
```
