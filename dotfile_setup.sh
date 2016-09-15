#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )" #this script's directory
olddir=$(dirname $dir)/dotfiles_old #old dotfiles backup directory
files="bashrc bash_profile emacs gitignore gitconfig" #list of files/folders to symlink in homedir


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

