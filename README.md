<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Description</a></li>
<li><a href="#sec-2">2. Prerequisites</a></li>
<li><a href="#sec-3">3. Quick start</a></li>
<li><a href="#sec-4">4. Source code files</a></li>
<li><a href="#sec-5">5. Motivation</a></li>
<li><a href="#sec-6">6. Limitations</a></li>
<li><a href="#sec-7">7. Support</a></li>
</ul>
</div>
</div>

Intended for linux users who have [emacs](https://www.gnu.org/software/emacs/) always open.

I did not expect this project to grow that much;
some of the desired functions are still not implemented or half-implemented.

# Description<a id="sec-1" name="sec-1"></a>

Synchronizing important files on two or more computers using
1.  `emacs` + GNU `make`,
2.  some sort of a cloud that can be mounted to a directory, for example, an
    1.  ssh-server, or
    2.  some public cloud service (e.g., Russian [Yandex Disk](https://disk.yandex.com/) or Swiss [pCloud](https://www.pcloud.com)) that can be mounted in linux
        using standard open-source utilities,
    
    and
3.  symmetric encryption
    1.  using [ImageMagick](https://imagemagick.org/) for `JPEG` and `PNG` images or
    2.  using [gpg](https://www.gnupg.org/) for other (presumably text) files.
    
    `JPEGs` and `PNGs` are encrypted with AES-algorithm that [may become vulnerable](https://imagemagick.org/script/cipher.php) if the same password is used for multiple images; this is why every image gets an individual password.
    As the encrypted image is about 13 times larger than the original one, I find it attractive switching to [selective image encryption](https://duckduckgo.com/?q=selective+image+encryption&t=ffsb&ia=web) when it becomes available in linux.
    (Or just pay for more cloud space.)

Encrypted files saved in the cloud have **random names** to minimize the amount of information Evil Corporations can extract by monitoring our cloud directory.

# Prerequisites<a id="sec-2" name="sec-2"></a>

We need `emacs`, GNU `make`, `ImageMagick`, `gpg` and `gawk`; in Debian these can be installed with `aptitude` as follows:

    aptitude install emacs make imagemagick gpg gawk

# Quick start<a id="sec-3" name="sec-3"></a>

1.  Mount remote directory. The mounting point may be arbitrary (specified as `cloud-directory` in `~/.emacs.d/cloud/config`), the default one is `/mnt/cloud/`.
2.  You can create the file `~/.emacs.d/cloud/config` yourself, or it will be generated. Mine looks as follows:
    
        delete-contents=yes
        contents-name=XYZ
        password=*********
        number-of-CPU-cores=8
        cloud-directory=/mnt/lws/cloud/
3.  Evaluate `cloud.el` in `emacs` at start by placing the following lines
    
        (server-start)
        (mapcar #'require '(cl dired-aux timezone diary-lib subr-x))
        (load-file "/path-to/cloud.el")
        (switch-to-buffer "*Messages*")
        (cloud-start)
    
    into your `~/.emacs` file.
    Then open a directory `C-x d`, mark several files, and cloud them with `M-x cloud-add`. Then `M-x cloud-sync`.
4.  Check log files described in [files.org](files.org): they should let you know that the encrypted copies of your clouded files have been copied to the remote directory
5.  Using some secure way, copy `~/.emacs.d/cloud/config` to another host; launch `cloud.el` there and check the log files.
6.  [Let me know](https://github.com/chalaev/cloud/issues/new/choose) if something does not work.

Every time we `M-x cloud-sync`, local files get synchronized with the cloud. For this purpose I have a line in my `crontab`:
`43 9-21 * * * emacsclient -e "(cloud-sync)" &> /dev/null`

# Source code files<a id="sec-4" name="sec-4"></a>

(Dynamically created/updated logs and data files are described in [files.org](files.org).)
1.  [README.org](README.org) generates `README.md` for [notabug](https://notabug.org/shalaev/emacs-cloud) and [github](https://github.com/chalaev/cloud).
2.  [cloud.org](cloud.org) contains the code from [generated/main.el](generated/main.el) together with explanations.
3.  [0.el](0.el), [1.el](1.el), and [2.el](2.el) are kind of "Appendix" containing some pieces of code which not interesting enough to be included in `cloud.org`.
4.  goodies/{[macros](goodies/macros.el),[functions](goodies/functions.el),[logging](goodies/logging.el)}.el are copied from the [elisp-goodies](https://notabug.org/shalaev/elisp-goodies) project.
5.  [Makefile](Makefile) merges all the code into [generated/cloud.el](generated/cloud.el) which is the main file to be launched when `emacs` starts.
6.  [shell/cloud-git](shell/cloud-git) synchronizes file operations in `git` with this code, for example:
    `cloud-git rm files.org` and `cloud-git mv log-files.org files.org`
7.  [bugs.org](bugs.org) contains
    1.  error and problem list,
    2.  tests to be done, and
    3.  ideas on further development.

# Motivation<a id="sec-5" name="sec-5"></a>

I like cloud file storages: they are cheap (or even free) and reliable.

However, Evil Corporations and governments are trying to spy on people using the information
that they extract from private data stored in the cloud.

Most of my important files (for example, emails or document scans) are not regularly changed;
for those I use [backup2l](https://github.com/gkiefer/backup2l) with gpg encryption, and store encrypted archives
using one of cloud services that allow `WebDav` access to the storage directory (Russian [Yandex Disk](https://disk.yandex.com/) or Swiss [pCloud](https://www.pcloud.com)).

This does not work so well for regularly changed files.
Since emacs is my only text editor, it is enough to write eLisp code that
1.  Saves unencrypted file locally and its encrypted copy in the cloud.
2.  Every file in `dired` buffer can be "clouded" (`M-x cloud-add`).
3.  Encrypted files are stored under randomly generated names and modification dates/times.
4.  Remotely stored files are periodically syncronized with the local ones.
5.  Dired-compatible: whatever I do with a file in dired (delete, rename), will be automatically done on other computers.

# Limitations<a id="sec-6" name="sec-6"></a>

1.  I use [GNU make](https://www.gnu.org/software/make/) together with its `--jobs` option to enjoy [(unsupported in emacs)](https://www.emacswiki.org/emacs/EmacsLispLimitations) multi-threading, and thus
    I have to suffer from the [make](https://www.gnu.org/software/make/) restriction: only nicely named files will work.  
       In particular, **no spaces in file names** are allowed.  
       In the future I hope to circumvent this limitation by creating soft links to badly named files.
2.  After encrypting an image and then decrypting it back, we get the same, but not identical picture (file size is changed).

# Support<a id="sec-7" name="sec-7"></a>

You can support this project by sending
1.  comments/questions to [oleg@chalaev.com](oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).