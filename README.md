dotfiles
========

## Ubuntu Basic Setup

### 1Password

[https://support.1password.com/install-linux/](https://support.1password.com/install-linux/)

```
sudo apt install -y curl

curl -sS https://downloads.1password.com/linux/keys/1password.asc | sudo gpg --dearmor --output /usr/share/keyrings/1password-archive-keyring.gpg
echo 'deb [arch=amd64 signed-by=/usr/share/keyrings/1password-archive-keyring.gpg] https://downloads.1password.com/linux/debian/amd64 stable main' | sudo tee /etc/apt/sources.list.d/1password.list
sudo mkdir -p /etc/debsig/policies/AC2D62742012EA22/
curl -sS https://downloads.1password.com/linux/debian/debsig/1password.pol | sudo tee /etc/debsig/policies/AC2D62742012EA22/1password.pol
sudo mkdir -p /usr/share/debsig/keyrings/AC2D62742012EA22
curl -sS https://downloads.1password.com/linux/keys/1password.asc | sudo gpg --dearmor --output /usr/share/debsig/keyrings/AC2D62742012EA22/debsig.gpg
sudo apt update && sudo apt install -y 1password
```

### Github CLI

```
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
sudo apt update
sudo apt install -y gh
gh auth login
```

Last step will allow to create SSH key and auth via browser

### Dotfiles

```
cd ~
gh repo clone worace/dotfiles

ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/emacs ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/tmux-linux.conf ~/.tmux-system.conf
ln -s ~/dotfiles/basic.tmuxtheme ~/.tmux.theme
```

### Basics

```
sudo apt -y install zsh tmux ripgrep xclip ffmpeg pv jq wget htop tree libssl-dev
```

### Emacs

Some packages seem to not work in emacs26 anymore so install 27 via PPA

```
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs27
```

### Shell

```
chsh -s /bin/zsh $(whoami)
```

### Rust

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup default nightly
rustup component add rls
```

[https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/](https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/)

```
cd /tmp
git clone https://github.com/rust-analyzer/rust-analyzer.git
cd rust-analyzer
cargo xtask install --server # will install rust-analyzer into $HOME/.cargo/binru
```

### Pop Shell

[https://github.com/pop-os/shell](https://github.com/pop-os/shell)

```
cd /tmp
gh repo clone pop-os/shell
cd shell
sudo apt install node-typescript gnome-tweaks
make local-install
```

Note: may need to use gnome tweaks to swap super key: [https://www.reddit.com/r/pop_os/comments/arp7qg/is_there_a_way_to_change_super_key/](https://www.reddit.com/r/pop_os/comments/arp7qg/is_there_a_way_to_change_super_key/) - Use "Alt is Swapped with Win" option

### Dropbox

- [https://www.dropbox.com/install-linux](https://www.dropbox.com/install-linux)
- Download .deb installer
- `sudo apt install python3-gpg` - to allow dropbox installer to verify signatures
- `sudo dpkg -i ~/Downloads/dropbox_*.deb`

### Fonts

```
cd ~/Downloads
wget https://github.com/adobe-fonts/source-code-pro/releases/download/2.038R-ro%2F1.058R-it%2F1.018R-VAR/OTF-source-code-pro-2.038R-ro-1.058R-it.zip
unzip *.zip
mkdir ~/.fonts
cp *.otf ~/.fonts/
fc-cache -f -v
```

### Key Repeat

```
gsettings set org.gnome.desktop.peripherals.keyboard delay 225
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15
```

### Ubuntu terminal theme

Preferences → Profiles → Uncheck "Use colors from system theme"...seems to pull gruvbox automatically from somewhere


## Extras

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

Spotify UI scaling: https://community.spotify.com/t5/Desktop-Linux/Linux-client-barely-usable-on-HiDPI-displays/td-p/1067272

## Minikube

https://minikube.sigs.k8s.io/docs/start/

```
pd ~/Downloads
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube_latest_amd64.deb
sudo dpkg -i minikube_latest_amd64.deb

$ minikube version
minikube version: v1.22.0
commit: a03fbcf166e6f74ef224d4a63be4277d017bb62e

curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
curl -LO "https://dl.k8s.io/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl.sha256"
echo "$(<kubectl.sha256) kubectl" | sha256sum --check
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

➸ kubectl version --client
Client Version: version.Info{Major:"1", Minor:"22", GitVersion:"v1.22.1", GitCommit:"632ed300f2c34f6d6d15ca4cef3d3c7073412212", GitTreeState:"clean", BuildDate:"2021-08-19T15:45:37Z", GoVersion:"go1.16.7", Compiler:"gc", Platform:"linux/amd64"}

rm kubectl.sha256
```
