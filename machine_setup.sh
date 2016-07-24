#!/bin/bash

sudo apt-get install emacs
sudo apt-get install scid
sudo apt-get install pgn-extract
cd ~ && wget -O - "http://www.dropbox.com/download?plat=lnx.x86_64" | tar xz
sudo apt-get install git
sudo apt-get install python-dev
sudo apt-get install libfreetype6-dev
sudo apt-get install python-pip
sudo apt-get install python-bs4
sudo easy_install pandas
sudo easy_install matplotlib
sudo easy_install sklearn
sudo easy_install csvkit


sudo apt-get install libblas-dev liblapack-dev libatlas-base-dev gfortran
sudo pip install numpy
sudo pip install scipy
sudo pip install pandas

sudo pip install xlrd
sudo apt-get install libmysqlclient-dev
sudo pip install MySQL-python

sudo easy_install sqlalchemy
sudo easy_install pymysql

#phantomjs
sudo apt-get install build-essential chrpath libssl-dev libxft-dev
sudo apt-get install libfreetype6 libfreetype6-dev
sudo apt-get install libfontconfig1 libfontconfig1-dev
cd ~
export PHANTOM_JS="phantomjs-2.1.1-linux-x86_64"
wget https://bitbucket.org/ariya/phantomjs/downloads/$PHANTOM_JS.tar.bz2
sudo tar xvjf $PHANTOM_JS.tar.bz2
sudo mv $PHANTOM_JS /usr/local/share
sudo ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/bin
