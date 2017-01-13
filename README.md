# Oook selector

This is the beginning of convenience tools for interacting with MarkLogic.

> Oook -- is all the [Librarian](https://en.wikipedia.org/wiki/Unseen_University#Librarian)
> at the university of the Discworld ever utters. He is the sole staff
> member of the greatest database of knowledge, and as such offers a
> very versatile and helpful interface to it.

### Warning

It is not even alpha but just some very early work and might change a lot.

### Successor of XDBC selector

The Oook selector replaces XDBC selector. Oook selector now
uses [Oook](https://github.com/xquery-mode/Oook) instead of Cider Any;
we decided to get rid of the backend design of cider-any and have a
simpler interface that just lets you evaluate XQuery documents.

## Installation

### Prerequisites

#### Install Uruk

Get [Uruk](https://github.com/daveliepmann/uruk) and put it into your
code directory, e.g., `~/src/`
```
  mkdir ~/src
  cd ~/src
  git clone https://github.com/daveliepmann/uruk.git
```

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

Get Oook selector and put it into a directory, e.g., `~/src/oook-selector`
```
  cd ~/src
  git clone --recursive https://github.com/xquery-mode/oook-selector.git
```

Put this in your `~/.emacs` or `~/.emacs.d/init.el`:

```
;; Uncomment the following line if your Clojure project uses lambdawerk.marklogic:
;; (provide 'lambdawerk.marklogic)

(require 'oook-setup  "~/src/oook-selector/oook-setup")

;; default server configuration
;; Note: You don't need this section if you use an LW configuration service.
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

## Set up a Leiningen project

In order to use Oook selector, you have to start Cider REPL in a
Leiningen project. The Uruk library must be pinned in the `project.clj`
in the dependencies section.

You can use just a stub project that acts as a gateway like:
```
cd ~/src

lein new app uruk-gw

cat > uruk-gw/project.clj <<__EOL__
(defproject uruk-gw "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [uruk "0.3.3"]]
  :main ^:skip-aot uruk-gw.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
__EOL__
```
## Usage

Cider-jack-in to an Uruk project by opening the `project.clj` file in
Emacs and starting a Cider REPL by entering  `C-c m j`. You can try
Oook selector by entering  `C-c m`  There is help if you press `?`
afterwards.

Note: If you use an LW configuration service, first enter  `C-c m g`  to fetch the
      configuration for the MarkLogic connection from the LW configuration service.

With  `C-c m x`  you can just evaluate an XQuery from a minibuffer.

All other commands have usually a lowercase version for the current database, and
a UPPERCASE one for the modules database of the current session/connection.

### Complete list of available selector methods:

- general methods:
  - `C-c m ?` - Selector help buffer
  - `C-c m q` - Quit / abort
- Cider convenience functions:
  - `C-c m j` - (cider) Start an nREPL server for the current project and connect to it.
  - `C-c m r` - (cider) Select the REPL buffer, when possible in an existing window.
  - `C-c m R` - (cider) Switch to the last Clojure buffer.
- simple XQuery evaluation:
  - `C-c m x` - Evaluate an XQuery from minibuffer
  - `C-c m X` - Evaluate an XQuery from minibuffer in the modules database
- document management:
  - `C-c m l` - List documents †
  - `C-c m L` - List documents in the modules database †
  - `C-c m s` - Show document ‡
  - `C-c m S` - Show document in the modules database ‡
  - `C-c m t` - Show this document at point ‡
  - `C-c m T` - Show this document at point in the modules database ‡
  - `C-c m u` - Upload document ‡
  - `C-c m U` - Upload document into the modules database ‡
  - `C-c m d` - Delete document ‡
  - `C-c m D` - Delete document in the modules database
  - `C-c m b` - Set server path for current buffer ‡
  - `C-c m B` - Set database for current buffer ‡
- database selection:
  - `C-c m c` - Choose/select database within current session/connection
  - `C-c m .` - Select default database of the server
  - `C-c m ,` - Select modules database of the server
  - `C-c m -` - Show which database is currently used
  - `C-c m /` - Show which database is currently used
- LW configuration service:
  (Just ignore this section you don't have such a service or don't know what it is.)
  - `C-c m g` - Get connection settings for ML connection from LW configuration service

#### † Notes on the Document list
* Press  `u`  to update the document list
* Press  `<Return>`  on a document's URI to show the document at point
  (This is the same as entering  `C-c m t`  outside of a Document list.)
* For paged output, set page limit with xdmp-set-page-limit.
  Use numerical prefix to switch to a different page.
* Currently, it cannot list documents with an URL not beginning with a `/`.

#### ‡ Notes server path and database of the current buffer

When a file is directly opened from MarkLogic using `Show document`
(and friends) or uploaded at a new location, its server path and
database are stored in buffer local variables.

"Upload document" and "Delete document" obey these variables, so that
you can just update the document in MarkLogic or delete it from the
database by entering `C-c m u` and `C-c m d`, respectively, from
within the opened file buffer.

You can set those variables explicitly by entering  `C-c m b` (set
server path) and `C-c m B` (set database). To upload to a new path,
you can also just enter a different path than the suggested when
uploading by entering  `C-c m u`.

#### Note on upload and delete document methods

To use upload and delete document you should have the file that is to
be transfered or delete open in a buffer. If you fire the command the
filename will be taken from the buffer and you will be interactively
queried to enter a directory path.  The file will be uploaded with a
URI of `<directory>/<filename>`.  Delete works analogously.
