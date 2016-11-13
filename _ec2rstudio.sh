# quick install script for setting up RStudio Server on Amazon EC2 instances

# clone several useful repositories
git clone https://github.com/nhejazi/ubuntu-fresh.git
git clone https://github.com/nhejazi/mydotfiles.git ~/.dotfiles
git clone https://github.com/nhejazi/myPkgLib.git

# add user
sudo adduser nimaRStudio
sudo passwd nimaRStudio

# update/upgrade
sudo apt-get update -y
sudo apt-get upgrade -y

# run only select Ubuntu scripts on EC2
sudo sh ~/ubuntu-fresh/_aptCore.sh
sudo sh ~/ubuntu-fresh/_aptLangs-basic.sh
sudo sh ~/ubuntu-fresh/_aptTools-extra.sh
sudo sh ~/ubuntu-fresh/_aptVim.sh

# fix installation issues arising from dependencies
sudo apt-get install -f -y

# test that RStudio Server is working...
sudo rstudio-server verify-installation
