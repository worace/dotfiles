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
sudo apt-get install -y redshift redshift-gtk silversearcher-ag htop pv jq caffeine
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

1. Wiped existing SSD using hdparm instructions [here](https://wiki.archlinux.org/index.php/Solid_State_Drives/Memory_cell_clearing)
2. Paritioned Drive:

```
1 100MB EFI partition # Hex code ef00
2 250MB Boot partition # Hex code 8300
3 100% size partiton # (to be encrypted) Hex code 8300
```

3. Format UEFI partition: `mkfs.fat -F32 /dev/sda1`
4. Format boot partition `mkfs.ext4 /dev/sda2`
5. Setup disk encryption `cryptsetup --cipher aes-xts-plain64 --verify-passphrase --use-random luksFormat /dev/sda3`
6. open the partition `cryptsetup luksOpen /dev/sdX3 luks` - note that the final argument here "`luks`" determines the name of the device under `/dev/mapper`
7. Create physical volume for lvm `pvcreate /dev/mapper/luks`
8. create volume group (this will be container both root and swap partitions): `vgcreate vg0 /dev/mapper/luks`
9. Create swap logical volume: `lvcreate --size 8G vg0 --name swap` -- generally make this the size of RAM (8gb in this case)
10. Create root logical volume in remainder of free space `lvcreate -l +100%FREE vg0 --name root`
11. Format root filesystem: `mkfs.ext4 /dev/mapper/vg0-root`
12. make swap: `mkswap /dev/mapper/vg0-swap`
13. Mount system:

```
mount /dev/mapper/vg0-root /mnt # /mnt will be the installed system once we bootstrap it in a sec...
swapon /dev/mapper/vg0-swap # Not needed but a good thing to test
mkdir /mnt/boot
mount /dev/sdX2 /mnt/boot # the unencrypted boot partition we created before
mkdir /mnt/boot/efi
mount /dev/sdX1 /mnt/boot/efi
```

14. bootstrap the system `pacstrap /mnt base base-devel grub-efi-x86_64 zsh vim git efibootmgr` (for my desktop i left off wifi)
15. Fstab `genfstab -pU /mnt >> /mnt/etc/fstab`
16. `arch-chroot /mnt /bin/bash`
17. `ln -s /usr/share/zoneinfo/America/Los_Angeles /etc/localtime`
18. `hwclock --systohc --utc`
19. `echo Bors > /etc/hostname`
20. Uncomment `en_US.UTF-8 UTF-8 ` from /etc/locale.gen and run `locale-gen`
21. Also set LANG and LANGUAGE in `/etc/locale.conf` (may be redundant with locale-gen)

```
echo LANG=en_US.UTF-8 >> /etc/locale.conf
echo LANGUAGE=en_US >> /etc/locale.conf
echo LC_ALL=C >> /etc/locale.conf

# set root passwd
passwd

# create user acct
useradd -m -g users -G wheel -s /bin/zsh worace
passwd worace

# /etc/mkinitcpio.conf
# MODULES="ext4"
# HOOKS="base udev autodetect modconf block filesystems keyboard fsck"
# (keyboard moved up and added encrypt and lvm2 _before_ filesystems)
mkinitcpio -p linux

grub-install

# In /etc/default/grub edit the line GRUB_CMDLINE_LINUX to GRUB_CMDLINE_LINUX="cryptdevice=/dev/sda3:luks:allow-discards" then run:
# (allow discards lets TRIM instructions be passed through to the SSD supposedly)
grub-mkconfig -o /boot/grub/grub.cfg

# finish and reboot
exit
umount -R /mnt
swapoff -a
reboot

# ...

# add wheel to sudoers
su root
visudo /etc/sudoers
# uncomment:
# %wheel ALL=(ALL) ALL


# enable dhcp auto connection
sudo systemctl enable dhcpcd.service
sudo systemctl enable dhcpcd@eth0.service
# configure mirrors
# refresh list
sudo pacman -Syyu
sudo cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup
sudo rankmirrors -n 6 /etc/pacman.d/mirrorlist.backup | sudo tee /etc/pacman.d/mirrorlist

# for my desktop i have a gtx 960 hence the nvidia drivers
sudo pacman -S nvidia nvidia-libgl hub openssh xclip xorg-server xorg-xinit firefox emacs xorg-server-utils the_silver_searcher xbindkeys
sudo reboot

# install yaourt
cd /tmp
git clone https://aur.archlinux.org/package-query.git
cd package-query
makepkg -si
cd ..
git clone https://aur.archlinux.org/yaourt.git
cd yaourt
makepkg -si
cd ~

yaourt -S dropbox python3 redshift ttf-font-awesome mpstat ttf-dejavu wqy-zenhei evince i3-gaps-git rxvt-unicode rofi termite i3blocks spotify slack-desktop

# fix dropbox file watchers limit
sudo echo "fs.inotify.max_user_watches = 100000" | sudo tee --append /etc/sysctl.d/99-sysctl.conf
sudo sysctl --system

# fix timezone clock issue
sudo pacman -S ntp
sudo ntpd -qg


git clone https://github.com/worace/dotfiles.git

ln -s ~/dotfiles/arch/xinitrc ~/.xinitrc
ln -s ~/dotfiles/arch/Xresources.light ~/.Xresources
mkdir ~/.config/i3
ln -s ~/dotfiles/arch/i3.config ~/.config/i3/config
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/emacs ~/.emacs.d
ln -s ~/dotfiles/.system_gitignore ~/.gitignore
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
mkdir ~/.config/termite
ln -s ~/dotfiles/arch/termite-dark ~/.config/termite/config
ln -s ~/dotfiles/arch/.i3blocks.conf ~/.i3blocks.conf
mkdir ~/.local/share/applications
ln -s ~/dotfiles/arch/1Password.desktop ~/.local/share/applications/1Password.desktop

# Github SSH setup

# multilib and wine/1password
# uncomment the multilib section from /etc/pacman.conf:
# [multilib]
# Include = /etc/pacman.d/mirrorlist

sudo pacman -Syy
sudo pacman -S wine
```

* symlinks...

### Packages to install

```
yaourt ttf-font-awesome
yaourt mpstat
yaourt acpi
yaourt ttf-dejavu
yaourt wqy-zenhei
yaourt evince
sudo pacman -S wget
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

### Password Manager

#### Keepass

* keepassx2 (1password alternative) `sudo pacman -S keepassx2`
* Using minikeepass on ios: https://itunes.apple.com/us/app/minikeepass-secure-password/id451661808?mt=8
* Writeup on ios<->dropbox syncing (semi-manual): https://groups.google.com/forum/#!topic/viphone/ke4NVQ8QfC8

#### 1Password

* mostly following https://discussions.agilebits.com/discussion/42126/making-1password-work-in-ubuntu-14-04
* also needed to go to Preferences > Browsers > Check "Unlock on secure desktop" and reopen firefox

### Lockscreen

* i3lock-color (fork of i3lock): `sudo pacman -S i3lock-color`
* use script in scripts/lock.sh
* set binding in i3 config:

`bindsym $mod+shift+l exec "$home/dotfiles/arch/scripts/lock.sh"`

### Dropbox

Setup to allow dropbox to work properly:

```
echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p
```

### Node JS

```
sudo pacman -S nodejs
sudo pacman -S npm
```

## Setting up emacs-mac on OS X

This is a fork of the default emacs homebrew formula which includes some mac-specific tweaks, as well as a few patches from the emacs devel branch that puts it ahead of the default emacs formula in terms of new features.

At the moment the one I'm most interested is the patch for improved subprocess forking in emacs which makes performance much better for things like magit.

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-no-title-bars --with-modern-icon
```

https://emacs.stackexchange.com/questions/141/emacsdaemon-and-emacsclient-on-mac
