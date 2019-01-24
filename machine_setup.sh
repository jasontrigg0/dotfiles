#!/bin/bash

sudo apt install emacs
sudo apt install git
sudo apt install scid
sudo apt install curl
sudo apt install pgn-extract
# cd ~ && wget -O - "http://www.dropbox.com/download?plat=lnx.x86_64" | tar xz
# sudo apt install python-dev
# sudo apt install libfreetype6-dev

#python
sudo apt install python-pip
sudo apt install python3-pip
sudo apt install python-bs4
sudo apt install python3-bs4
sudo pip install pandas
sudo pip3 install pandas
sudo pip install matplotlib
sudo pip3 install matplotlib
sudo apt install python-tk
sudo apt install python3-tk
sudo pip install sklearn
sudo pip3 install sklearn
# sudo easy_install csvkit

#nvm (installs in user directory)
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
#upgrade node
nvm install node --reinstall-packages-from=node || nvm install node #reinstall-packages-from won't work on the initial installation
sudo chown -R jtrigg /usr/local/lib/node_modules #setup permissions for npm install -g with https://github.com/npm/npm/issues/8165#issuecomment-98382865
ln -snf $(npm root -g) ~/.node_modules #load modules from global folders. this isn't recommended. https://nodejs.org/api/modules.html#modules_loading_from_the_global_folders

#nvm install node should install npm, so the below command is unnecessary, right?
# sudo apt install npm

#numpy scipy pandas
# sudo apt install libblas-dev liblapack-dev libatlas-base-dev gfortran
# sudo pip install numpy
sudo pip install scipy
sudo pip3 install scipy

sudo pip install xlrd

sudo apt install mysql-client
sudo apt install libmysqlclient-dev #needed for python's mysqlclient
sudo pip3 install mysqlclient

# sudo easy_install sqlalchemy
# sudo easy_install pymysql

#https://www.viget.com/articles/dress-up-your-git-diffs-with-word-level-highlights
#prettier git diffs
mkdir -p $HOME/scripts
curl https://raw.githubusercontent.com/git/git/fd99e2bda0ca6a361ef03c04d6d7fdc7a9c40b78/contrib/diff-highlight/diff-highlight > $HOME/scripts/diff-highlight && chmod +x $HOME/scripts/diff-highlight

#ag
sudo apt install silversearcher-ag

#easy screenshot sending
curl -s https://packagecloud.io/install/repositories/gyazo/gyazo-for-linux/script.deb.sh | sudo bash
sudo apt install gyazo

sudo apt install i3
sudo apt install jp2a

sudo apt install screen

#phantomjs
# sudo apt install build-essential chrpath libssl-dev libxft-dev
# sudo apt install libfreetype6 libfreetype6-dev
# sudo apt install libfontconfig1 libfontconfig1-dev
# cd ~
# export PHANTOM_JS="phantomjs-2.1.1-linux-x86_64"
# wget https://bitbucket.org/ariya/phantomjs/downloads/$PHANTOM_JS.tar.bz2
# sudo tar xvjf $PHANTOM_JS.tar.bz2
# sudo mv $PHANTOM_JS /usr/local/share
# sudo ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/bin

#earlyoom (kill processes before they take up all your memory)
#https://github.com/rfjakob/earlyoom
cd ~/Downloads
git clone https://github.com/rfjakob/earlyoom.git
cd earlyoom
make
sudo make install

#noti for desktop and mobile (pushbullet) notifications
# https://github.com/variadico/noti
# using tee to redirect to a protected folder
curl -L https://github.com/variadico/noti/releases/download/v2.5.0/noti2.5.0.linux-amd64.tar.gz | tar -xOz | sudo tee /usr/bin/noti > /dev/null
sudo chmod +x /usr/bin/noti

#dex for autostart with i3
#https://faq.i3wm.org/question/2155/how-can-i-use-autostart-desktop-files-in-i3.1.html
sudo apt install dex

#concat pdfs
sudo apt install pdftk

#https://github.com/mbostock/gistup
npm install -g gistup

#monitoring
sudo apt install htop
sudo apt install iotop
sudo apt install powertop
sudo apt install nethogs

#percol
sudo pip install percol

#watch files and run command when they change
#https://github.com/clibs/entr
sudo apt install entr #ls file_to_watch.txt | entr sh -c 'echo "File changed"'

sudo apt install jq #used by weather() command in bashrc

#TODO: gitless install, if it ends up useful http://gitless.com/

#docker
sudo apt install docker
sudo usermod -aG docker $USER
sudo curl -L https://github.com/docker/compose/releases/download/1.19.0/docker-compose-`uname -s`-`uname -m` -o /usr/local/bin/docker-compose

#random utility to show file progress
sudo apt install pv

#for i3 screenshot shortcuts
sudo apt install scrot

#for storing secret files on git
sudo apt install git-crypt

#chromedriver
sudo apt-get install chromium-chromedriver
sudo cp /usr/lib/chromium-browser/chromedriver /usr/bin/chromedriver
