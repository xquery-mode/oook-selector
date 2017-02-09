## Installation

If use Cider and Leiningen already, just jump to the section
[Install Oook selector and its dependencies](#install-oook-selector-and-its-dependencies).
Otherwise, follow the steps outlined in the next section
[Install prerequisites](#install-prerequisites).

### Install prerequisites

#### Install Cider

Start Emacs and run  `M-x package-install <Return> cider <Return>`.

If it complains that `cider` is not available ("[No match]"),
put this in your `~/.emacs` or `~/.emacs.d/init.el`:

```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
```
call  `M-x package-refresh-contents`  and try again.

#### Install Leiningen

To get [Leiningen](https://leiningen.org):
* download the
  [`lein script`](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)
  (or on Windows
  [`lein.bat`](https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein.bat))
* place it on your `$PATH` where your shell can find it (e.g., `~/bin`)
* set it to be executable:
```
  mkdir ~/bin
  cd ~/bin
  wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
  chmod a+x ~/bin/lein
```

### Install Oook selector and its dependencies

Get Oook selector and put it into a directory, e.g., `~/src/oook-selector`:
```
  cd ~/src
  git clone --recursive https://github.com/xquery-mode/oook-selector.git
```

Put this in your `~/.emacs` or `~/.emacs.d/init.el`:

```
(require 'oook-setup  "~/src/oook-selector/oook-setup")

;; default server configuration
(setq xdmp-servers
  '(:rest-server (:host "localhost" :port "8000" :user "admin" :password "admin")
    :xdbc-server (:host "localhost" :port "9000" :user "admin" :password "admin")))
;; make sure that oook has our current XDBC server configuration
(xdmp-propagate-server-to-oook)
```

Note: `oook-setup` does some initialization in addition to just loading the
  Oook selector. If you want to do this step yourself, and just load the
  bare Oook selector, have a look at `oook-setup.el` to see how the loading
  is done, and replace the `require 'oook-setup` line by something you prefer.

Warning: Oook selector contains its own versions of Oook, XQuery Mode,
  and Page Break Lines. Make sure it does not conflict with local files
  in your Emacs configuration. If in doubt, remove any other version.
