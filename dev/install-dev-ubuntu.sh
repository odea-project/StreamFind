#!/bin/bash

## Install R ##
# update indices
sudo apt update -qq -y
# install two helper packages we need
sudo apt install -y --no-install-recommends software-properties-common dirmngr
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt install -y --no-install-recommends r-base
# Get 5000+ CRAN Packages
sudo add-apt-repository -y ppa:c2d4u.team/c2d4u4.0+


## Install streamFind and dependencies ##
R < ./dev/000_install_streamFind_and_dependencies.R --no-save