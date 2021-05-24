
# Table of Contents

1.  [Description](#org8ed9452)
2.  [Prerequisites](#org478c272)
3.  [Quick start](#orgf62cfac)
    1.  [Initial setup](#orga6777d7)
    2.  [Uploading and downloading files](#org436992c)
4.  [Commands](#orgc7778f9)
5.  [Limitations](#orgba11a1c)
6.  [License](#org6d35307)

Intended for linux users who have [emacs](https://www.gnu.org/software/emacs/) always open.


<a id="org8ed9452"></a>

# Description

Synchronizing important files on two or more computers using

1.  `emacs` + GNU `make`,
2.  some sort of a cloud that can be mounted to a directory, for example, an
    a. ssh-server, or
    b. some public cloud service (e.g., Russian [Yandex Disk](https://disk.yandex.com/) or Swiss [pCloud](https://www.pcloud.com)) that can be mounted in linux
       using standard open-source utilities,
    and
3.  symmetric encryption
    a. using [ImageMagick](https://imagemagick.org/) for `JPEG` and `PNG` images or
    b. using [gpg](https://www.gnupg.org/) for other (presumably text) files.

Encrypted files saved in the cloud have **random names** to minimize the amount of information cloud owners can extract by monitoring our cloud directory.


<a id="org478c272"></a>

# Prerequisites

We need

1.  `emacs`, GNU `make`, `ImageMagick`, `gpg`, `sed`, and `gawk`; in Debian these can be installed as follows:  
    `aptitude install emacs make imagemagick gpg sed gawk`
2.  [lisp-goodies](https://github.com/chalaev/lisp-goodies): [start.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/start.el) (used by [Makefile](Makefile)), and [shalaev.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/shalaev.el)
3.  (for testing only) [el-debug.el](https://github.com/chalaev/el-debug/blob/master/packaged/el-debug.el)


<a id="orgf62cfac"></a>

# Quick start

I am running `emacs` in daemon mode (in text console) using the following line in [~/.login](https://github.com/chalaev/lisp-goodies/blob/master/.login):

    emacs --daemon

Once `emacs` was started in the daemon mode, I can use `emacsclient -c` to open a new (gui) emacs window.


<a id="orga6777d7"></a>

## Initial setup

1.  `mkdir ~/.emacs.d/local-packages/`.
2.  Place [shalaev.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/shalaev.el) and [cloud.el](packaged/cloud.el) to `~/.emacs.d/local-packages/`
3.  Load [start.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/start.el) in your [~/.emacs](.emacs)
4.  Evaluate `cloud.el` in `emacs` at start by placing the following lines
    
        (require 'cloud)
        (cloud-start)
    
    into your [~/.emacs](.emacs) file. 
    In emacs, update some of the files , then `M-x cloud-sync`.
    During the very first `(cloud-start)` run, configuration file [~/.emacs.d/cloud/\`hostname\`/config](config) will be created.
5.  Mount remote directory. The mounting point may be arbitrary (specified as `cloud-directory` in [~/.emacs.d/cloud/\`hostname\`/config](config)), the default one is `/mnt/cloud/`.


<a id="org436992c"></a>

## Uploading and downloading files

There are several log files; the least informative one is ``~/.emacs.d/cloud/`hostname`/log`` described in [files.org](files.org).

1.  Edit a text file in emacs. Unless it is blacklisted it will automatically be clouded when you save it. (Blacklisted files can be manually clouded using `M-x cloud-add`.)
2.  Run `M-x cloud-sync` to upload the file and ``tail -f ~/.emacs.d/cloud/`hostname`/log`` to see the log.
3.  Now you can move to another host (e.g. from your office desktop to your laptop).
4.  Using some secure way, copy [~/.emacs.d/cloud/\`hostname\`/config](config) to another host; launch `cloud.el` there and check the log files.
    Ensure that updated on your previous host files were downloaded to your current host.
5.  [Let me know](https://github.com/chalaev/cloud/issues/new/choose) if something is unclear or does not work.

Every time we `M-x cloud-sync`, local files get synchronized with the cloud.
(The same happens when we start/quit emacs.)
For this purpose I have a line in my `crontab`:  
`43 9-21 * * * emacsclient -e "(cloud-sync)" &> /dev/null`


<a id="orgc7778f9"></a>

# Commands

Except for `M-x cloud-sync`, commands are barely used:

-   `M-x cloud-add` adds one or several files to the list of "clouded" files.
    This means that `M-x cloud-sync` command will upload these "clouded" files to the remote server if they are updated. Supposed to be used in dired buffer for several
    (marked) files, or (when no files are marked) for a single file. **Files edited in emacs are clouded automatically,** but there are exceptions – see
    [sample configuration file](config) and the [source code](cloud.org).
    Works both on files and directories.
-   `M-x cloud-forget` is the opposite of `M-x cloud-add`. 
    It is also called automatically when files are removed in dired buffer. Currently works on files only, not on directories.
-   `M-x cloud-sync` syncronizes local files with the cloud. Could be regularly called with a `crontab` line, e.g.,  
    `43 9-21 * * * emacsclient -e "(cloud-sync)" &> /dev/null`


<a id="orgba11a1c"></a>

# Limitations

1.  I use [GNU make](https://www.gnu.org/software/make/) together with its `--jobs` option to enjoy [(unsupported in emacs)](https://www.emacswiki.org/emacs/EmacsLispLimitations) multi-threading, and thus
    I have to suffer from the [make](https://www.gnu.org/software/make/) restriction: only nicely named files will work.  
    In particular, **no spaces in file names** are allowed.
    (This limitation can probably be circumvented by creating soft links to badly named files.)
2.  Encrypting images is just a toy feature for now; it has to be better developed to become really useful.
    After encrypting an image file and then decrypting it back, we get the same, but not identical picture (file size is changed).


<a id="org6d35307"></a>

# License

This code is released under [MIT license](https://mit-license.org/).

