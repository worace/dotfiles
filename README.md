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
sudo apt-get install hub
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
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
cp ~/dotfiles/worace.zsh-theme ~/.oh-my-zsh/themes/
chsh -s /bin/zsh 
# Note -- need to log out and log in on ubuntu for this to take effect
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

Doesn't currently seem to be a good PPA for emacs 24.4+
(lots of old discontinued ones around).

Ended up building it from source.

Thanks to [this tutorial](http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/)

```
sudo apt-get install build-essential
sudo apt-get build-dep emacs24
cd ~/Downloads
curl http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz > emacs-24.5.tar.gz
tar -xf emacs-24.5.tar.*
cd emacs-24.5
./configure
make
sudo make install
```

With luck packages will auto-install on first emacs boot based
on symlinked `.emacs.d`

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

Somewhat miraculously got this working thanks
to [this tutorial](https://discussions.agilebits.com/discussion/42126/making-1password-work-in-ubuntu-14-04)

```
sudo apt-get install wine
```

Use standard windows installer [here](https://agilebits.com/onepassword/windows)

### 12. F.lux

This supposedly had a PPA but it seems to
be defunct.

Managed to install it with these instructions
from [this post](https://gist.github.com/robertboloc/9feaa9150926efa4175a)

```
sudo apt-get install python-glade2 python-appindicator python-pexpect python-gconf
cd ~/Downloads
git clone https://github.com/Kilian/f.lux-indicator-applet.git
cd f.lux-indicator-applet
chmod +x setup.py
sudo ./setup.py install
fluxgui
```
### Slack Client

[ScudCloud](https://github.com/raelgc/scudcloud)

### Gitignore

I have this in dotfiles as `.system_gitignore` so i can have a special gitignore for this actual repo. Thus to link this one use `ln -s ~/dotfiles/.system_gitignore ~/.gitignore`

## Todo / Wishlist

* [ ] Emacs -- helm-ag within specified directory
* [ ] Emacs -- helm projectile search without pulling query term from point
* [ ] zsh -- is there a better way to deal with slow NVM startup?
* [ ] mpd -- play around with this for music playing with spotify

## Arch Setup Todo

* [ ] i3 window styling
* [ ] i3 bar - add icons to workspace (firefox, emacs, etc)
* [X] i3 bar - wifi icon, battery icon
* [ ] lemonbar or alternate
* [ ] suspend to disk config
* [ ] lockscreen on suspend
* [ ] emacs daemon on startup
* [ ] reinstalling with full disk encryption and with encrypted swap partition (LUKs?)
* [ ] launch x on system startup? - https://wiki.archlinux.org/index.php/Xinit#Autostart_X_at_login
* [ ] disable trackpad while typing
* [ ] multi swipe gesture -- 3 finger swipe for tabs (libinput?)
* [ ] i3 - alt-tab to cycle workspaces
* [ ] emacs (and vim?) - yank to system clipboard
* [X] urxvt - copy/paste with keyboard commands - https://nixmeal.wordpress.com/2012/07/24/copypaste-text-in-urxvt-rxvt-unicode-using-keyboard/ -- solved use Alt+Ctrl+C / Alt+Ctrl+V
* [ ] better wifi manager?
* [ ] style rofi (solarized? limit # of lines shown?)
* [X] Rofi launcher - bound to Alt+Space in i3; could still use some tweaks
* [ ] i3 assign programs to consistent workspaces

## Incomplete Setup Notes

* symlinks...

### Packages to install

```
yaourt ttf-font-awesome
yaourt mpstat
yaourt acpi
yaourt ttf-dejavu
yaourt wqy-zenhei
yaourt evince
```

### Local Postgres Setup

```
sudo pacman -S postgresql
sudo -i -u postgres
initdb -D '/var/lib/postgres/data'
logout
sudo systemctl start postgresql
sudo systemctl enable postgresql
# create user account for your user:
createuser -s -U postgres --interactive
createdb
psql # verify it works
```
