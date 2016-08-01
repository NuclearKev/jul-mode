# jul-mode

jul-mode is a package for Emacs that allows you to easily manipulate
packages found in the user repositories for Dragora. It also lets you
easily install, upgrade, and remove packages.

## jul-mode.el

This is the main source file. All the code is based on code from the
`list-packages.el' file.

## notes.org

Very messy notes for figuring out how `list-packages' works.

## Goals

The main goal of the mode is to bring an easy-to-use package manager
for the user repos. However, I wish for it to be a tool that you can
use to do all package management on your Dragora system.

## How to use jul-mode

Using jul-mode is very easy. Since it isn't found in melpa, you must clone it
from this repo and add '(add-to-list 'load-path "/path/to/jul-mode")' to your
.emacs file. This will allow you to use the 'load' function to load in jul-mode.
To load it place '(load "jul-mode.el")' in your .emacs file.

To run jul-mode, simple run the command 'M-x jul-list-package' and a it will
sync with the user repo and probe your installed directory, then display all
the available and installed packages.

At this point you can search for the package you wish to install/remove by using
Emacs' default search function (C-s). Once you find a package you wish to
install/remove, you can press 'i' or 'd' to mark it for installation or
deletion, respectively. If you wish to check you system for updates, press 'U'
(notice that this is upper case) and jul-mode will find any packages that are
in need of updating. You will notice the packages will be marked with a 'U' if
they are in need of upgrading. To execute the installation, deletion, and/or
upgrade press 'x' and an asynchronous shell task will be opened. Enter in your
sudo password and you are golden. If you marked a package you don't wish to
install/remove/upgrade, you can press 'u' (lowercase) while the cursor is on
that package and it will be unmarked.

After you do some installs and some updates you may wish to clean out all the
.tlz files found in the temporary directory. You can do this by running the
`jul-package-clean-temp-dir' function. To execute it, just hit 'C' (uppercase).

If you wish to see the output of the install, remove, or upgrade command, you
can switch to the `pkg $(command) Output' buffer; where command is add, remove,
or upgrade.
