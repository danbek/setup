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

and then put the public key on github. On windows I will want to do

    git config --global core.autocrlf false
    
or else cygwin will be unhappy about line endings on the installer. The last
time I did this I found that git complained about setting this in --global,
so I may need to use --system instead.  Then the following:

    cd
    git clone git@github.com:danbek/setup.git
    cd setup/bin && ./installer

The installer has special options as well:

    installer --slime # download slime and build sbcl core, then exit
    installer --vim-pandoc # download and install vim-pandoc, then exit
    installer --vim-netrw # download and install vim-netrw 142, which
                          # fixes an annoying bug

To install Adobe Source Code Pro and Hack fonts do this:

   cd setup/bin && ./install_fonts.sh

Login again to pick up all the changes.

To-do
-----
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
* The value should be `00000000 00000000 02000000 1D003A00 00000000`
* reboot.

[msdn]: http://msdn.microsoft.com/en-us/library/windows/hardware/gg463447.aspx

Make capslock a control under Xubuntu
-------------------------------------

    $ sudo /etc/default/keyboard
    $ # change XKBOPTIONS to "ctrl:nocaps"
    $ sudo dpkg-reconfigure keyboard-configuration

Make Alt-Tab work correctly on Ubuntu 18.04
-------------------------------------------

Follow instructions here: https://askubuntu.com/a/1154780/510762 and here: https://askubuntu.com/questions/464946/force-alt-tab-to-switch-only-on-current-workspace-in-gnome-shell

Installing linux on Hyper-V
----------------------------

You can get this setup so that you access the linux GUI through an RDP
session, which allows clipboard sharing to work. This is the best
approach that I have found yet.

Here are instructions for doing this for Ubuntu 20.04:

https://www.nakivo.com/blog/install-ubuntu-20-04-on-hyper-v-with-enhanced-session/

This allows you to connect from the hyper-v GUI, but not from the
windows Remote Desktop application itself. To do that, follow these
instructions:

https://stackoverflow.com/questions/57236407/cannot-rdp-to-ubuntu18-hyper-v-quick-start

Keeping this link in case it ever prooves useful:

https://www.nakivo.com/blog/how-to-use-remote-desktop-connection-ubuntu-linux-walkthrough/

This setup apparently uses xrdp to start the session, and it was not sourcing
.profile. To fix this I added this line right before the test and exec
in `/etc/xrdp/startwm.sh`:

    . $HOME/.profile

Placing matplotlib windows on second monitor
--------------------------------------------

Trying to do this using devilspie2. The installer places the config file in the right place. But you need to 
run devilspie2 from the command line in a terminal:

$ devilspie2 # just run it somewhere

Should really make this run on startup or something.

Zenburn for xfce terminal
-------------------------
See https://pastebin.com/6xQhZd84

    # Zenburn colours scheme for Xfce Terminal updated for Xfce4-terminal 0.6.3. Copy and paste the following in ${HOME}/.config/xfce4/Terminal/terminalrc:

    ColorBackground=#404040
    ColorForeground=#F6F3E8
    ColorCursor=#8f8fafaf9f9f
    ColorPalette=#3f3f3f3f3f3f;#e8e893939393;#9e9ecece9e9e;#f0f0dfdfafaf;#8c8cd0d0d3d3;#c0c0bebed1d1;#dfdfafaf8f8f;#efefefefefef;#3f3f3f3f3f3f;#e8e893939393;#9e9ecece9e9e;#f0f0dfdfafaf;#8c8cd0d0d3d3;#c0c0bebed1d1;#dfdfafaf8f8f;#efefefefefef

AUCTex Under Cygwin/Windows
---------------------------

Assumes TeXLive installed outside cygwin

    $ git clone git://git.savannah.gnu.org/auctex.git
    $ cd auctex
    $ ./autogen.sh
    $ ./configure --with-texmf-dir=/cygdrive/c/texlive/2013/texmf-dist
    $ make
    $ make install

Also need line

    (load "auctex.el" nil t t)

in init.el, but this should already be there.

For ubuntu, I needed to install autoconf and texinfo, and run
configure like

    $ ./configure --with-texmf-dir=/usr/local/texlive/2013/texmf-dist
    
    
References:

* [AUCTeX From Git] (http://permalink.gmane.org/gmane.emacs.auctex.general/5019)
* [AUCTeX On Cygwin] (http://emacsworld.blogspot.com/2013/11/updated-instructions-for-installng.html)
* The AUCTeX manual also has useful info
 
