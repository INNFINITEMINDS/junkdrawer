# quick install script for setting up RStudio Server on Amazon EC2 instances

if [ "$EUID" -ne 0 ]; then
  echo "This script must be run as root"
  exit
fi

# clone several useful repositories
git clone https://github.com/nhejazi/ubuntu-fresh.git
git clone https://github.com/nhejazi/mydotfiles.git ~/.dotfiles
git clone https://github.com/nhejazi/myPkgLib.git


# add user
sudo adduser nima


# update/upgrade
sudo apt-get update
sudo apt-get upgrade


# run only select Ubuntu scripts on EC2
sudo sh ~/ubuntu-fresh/_aptCore.sh
sudo sh ~/ubuntu-fresh/_aptLangs-basic.sh
sudo sh ~/ubuntu-fresh/_aptTools-extra.sh
sudo sh ~/ubuntu-fresh/_aptVim.sh


# install R package library
sudo Rscript -e ~/myPkgLib/install_rpkgs.R


# test that RStudio Server is working...
sudo rstudio-server verify-installation
