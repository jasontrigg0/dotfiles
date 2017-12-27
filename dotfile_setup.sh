#!/bin/bash
############################
# dotfile_setup.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )" #this script's directory
olddir=$(dirname $dir)/dotfiles_old #old dotfiles backup directory
files="bashrc bash_profile emacs gitignore gitconfig agignore startup screenrc inputrc" #list of files/folders to symlink in homedir


# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv $HOME/.$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file $HOME/.$file
done


#setup .ssh config
if [ -f "$dir/sshconfig" ]
then
    mv ~/.ssh/config $olddir
    ln -s $dir/sshconfig ~/.ssh/config
fi

#setup .i3 config
if [ -f "$dir/i3config" ]
then
    mv ~/.i3/config $olddir
    ln -s $dir/i3config ~/.config/i3/config
fi

#setup autostart
if [ -f "$dir/jtrigg.desktop" ]
then
    mv ~/.config/autostart/jtrigg.desktop $olddir
    ln -s $dir/jtrigg.desktop ~/.config/autostart/jtrigg.desktop
fi


#setup .emacs.d directory
#http://www.lingotrek.com/2010/12/integrating-emacs-with-x11-clipboard-in.html
{
    sudo apt-get install xclip &&
    ln -snf $dir/xclip.el $HOME/.emacs.d/xclip.el;
} || {
    echo "ERROR: unable to install xclip";
}


#setup global .gitignore
git config --global core.excludesfile $HOME/.gitignore

#setup crontab
#using ssmtp to get MAILTO to work with crontab
#original ssmtp recommendation: https://askubuntu.com/a/536934
#gmail setup: https://help.ubuntu.com/community/EmailAlerts
#in case of gmail permissions problems: https://serverfault.com/questions/635139/how-to-fix-send-mail-authorization-failed-534-5-7-14/672182#672182
sudo apt install ssmtp
sudo cp $dir/ssmtp /etc/ssmtp/ssmtp.conf
sudo sed -i 's/AuthUser=/AuthUser='"$MAILER_ADDRESS"'/g' /etc/ssmtp/ssmtp.conf
sudo sed -i 's/AuthPass=/AuthPass='"$MAILER_PASSWORD"'/g' /etc/ssmtp/ssmtp.conf

#install the actual crontab file
sudo chown root $dir/crontab
sudo ln -snf $dir/crontab /etc/cron.d/jtrigg
#check the crontab logs here:
#grep -i cron /var/log/syslog
