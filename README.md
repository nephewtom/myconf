# myconf
This repo stores my configuration settings for bash & GNU Emacs under Linux and Mac OS X.

In order to set bash environment and aliases, it is necessary to source the corresponding script.
For example, on Ubuntu 14.04:

    user@ubuntu:~$ cat .bash_aliases
    . ~/myconf/linux_alias

    user@ubuntu:~$ tail -3 .bashrc
    # --- my conf
    . ~/myconf/linux_env
    . ~/myconf/persistent_history

For example, on Mac OS X:

    iMac:~ tomas$ cat .bash_profile
    export PATH=${PATH}:~/bin
    . ~/myconf/common_env
    . ~/myconf/mac_env
    . ~/myconf/persistent_history
    . ~/myconf/common_alias
    . ~/myconf/mac_alias

To load emacs `init.el` file, create a symbolic link like:

    user@hostname:~/.emacs.d$ ln -s ~/myconf/init.el init.el

TODO: I might change its name to **onmyset**, as it has a [funny meaning.](http://es.urbandictionary.com/define.php?term=On+My+Set)
