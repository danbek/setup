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
If I don't already have a key pair for the host, do:

    $ ssh-keygen -t rsa -C "user@hostname"

and then put the public key on github. On windows you will want to do

    git config --global core.autocrlf=false
    
or else cygwin will be unhappy about line endings on the installer. The last
time I did this I found that git complained about setting this in --global,
so you may need to use --system instead.  Then the following:

    cd
    git clone git@github.com:danbek/setup.git
    cd setup/bin && ./installer

Login again to pick up all the changes.

The installer has special options as well:

    installer --slime # download slime and build sbcl core, then exit
    installer --vim-pandoc # download and install vim-pandoc, then exit
    installer --vim-netrw # download and install vim-netrw 142, which
                          # fixes an annoying bug

To-do
-----
* automatically download emacs stuff?
* git-completion apparently not where I thought it was on ubuntu 12.04. Need to fix this.
* installer should probably create any required empty directory (i.e. emacs backups).

Other Setup Notes
=================

ssh config
----------
`.ssh/config` has the form:

    Host <nickname>
         HostName <full hostname>
         User <username>

Make capslock a control under Windows
-------------------------------------

An [MSDN article][msdn] covers this. You have to edit the registry.

* Go to the `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout` registry entry.
* Add a `REG_BINARY` value named `Scancode Map`
* The value should be `00000000 00000000 02000000 3A001D00 00000000`
* reboot.

[msdn]: http://msdn.microsoft.com/en-us/library/windows/hardware/gg463447.aspx
