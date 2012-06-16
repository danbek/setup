Unix Setup
==========

The unix setup that I like to use.

Organization
------------

* Everything goes into the directoy "setup" - dotfiles, bin, whatever
  * dotfiles are stored with names like dot-emacs.d, in root directory
  * directory `bin` contains any useful scripts
* The install_sh script installs everything by creating appropriate symlinks
* setup is kept under version control (git / guthub).

Download & Installation
-----------------------

    cd
    # download from github
    cd setup && ./installer

Login again to pick up all the changes.

The installer has one special options as well:

    installer --slime # download slime and build sbcl core, then exit

To-do
-----
* automatically download emacs stuff?
* git-completion apparently not where I thought it was on ubuntu 12.04. Need to fix this.
* installer should probably create any required empty directory (i.e. emacs backups).
* add .gitconfig
