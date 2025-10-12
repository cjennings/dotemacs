#!/bin/sh
# Craig Jennings
# post archsetup step to reset remote upstream repositories on emacs
# configuration and doftiles.

cd ~/emacs.d/
git remote remove origin
git remote add github git@github.com:cjennings/dotemacs.git
git remote add origin git@cjennings.net:dotemacs.git
git branch -M main
git push -u origin main


cd ~/.dotfiles/
git remote remove origin
git remote add github git@github.com:cjennings/dotfiles.git
git remote add origin git@cjennings.net:dotfiles.git
git branch -M main
git push -u origin main
