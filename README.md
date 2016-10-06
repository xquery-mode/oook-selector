# xdbc selector

This is the beginning of convenience tools for interacting with MarkLogic.

## Warning

It is not even alpha but just some very early work and might change a lot.

## Installation

Get xdbc-selector, xquery-mode, cider-any, and page-break-lines and put them
into a directory ~/src/emacs/
```
  git clone https://github.com/xquery-mode/xdbc-selector.git
  git clone https://github.com/xquery-mode/cider-any.git
  git clone https://github.com/xquery-mode/xquery-mode.git
  git clone https://github.com/purcell/page-break-lines.git
```

Right now you have to use some special branches:
```
  cd ~/src/emacs/cider-any/
  git checkout replace-URI-by-host-port
```

Put this in your ~/.emacs or ~/.emacs.d/init.el:

```
(let ((default-directory  "~/src/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-to-load-path '("page-break-lines"))
  (normal-top-level-add-to-load-path '("xquery-mode"))
  (normal-top-level-add-to-load-path '("cider-any"))
  (normal-top-level-add-to-load-path '("cider-any/examples"))
  (normal-top-level-add-to-load-path '("xdbc-selector"))
  ;;(normal-top-level-add-subdirs-to-load-path)
  )

;; Uncomment the following line if your Clojure project uses ml-file-loading:
;; (provide 'ml-file-loading)

(require 'xdbc-setup)

;; default server configuration
;; Note: You don't need this section if you use an LW configuration service.
(setq xdmp-servers
  '(:rest-server (:host "localhost" :port "8000" :user "admin" :password "admin")
    :xdbc-server (:host "localhost" :port "9000" :user "admin" :password "admin")))
;; make sure that cider-any-uruk has our current XDBC server configuration
(xdmp-propagate-server-to-cider-any-uruk)
```
xdbc-setup does some initialization in addition to just loading the
xdbc-selector.  If you want to do this step yourself and just load the
bare xdbc-selector replace the require line by:
```
(require 'xdbc-selector)
```

Get uruk and put it into your code directory, e.g. ~/src/
```
  git clone https://github.com/daveliepmann/uruk.git
```

## Set up leiningen project

Start cider repl in a leiningen project. The Uruk library must be
pinned in the project.clj in the dependencies section.

You can use just a stub project that acts as a gateway like:
```
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

Cider-jack-in to an uruk project and try the xdbc-selecter by entering  C-c m
There is help if you press '?' afterwards.

Note: If you use an LW configuration service, first enter  C-c m g  to fetch the
      configuration for the XDBC and REST server of the application setting
      :app/ml-file-loading  from the LW configuration service.

With  C-c m x  you can just evaluate an xquery from a minibuffer.

All other commands have usually a lowercase version for the current database, and
a UPPERCASE one for the modules database of the current session/connection.

### Complete list of available selector methods:

- general methods:
  - C-c m ? - Selector help buffer
  - C-c m q - Quit / abort
- simple xquery evaluation:
  - C-c m x - Evaluate an xquery from minibuffer
  - C-c m X - Evaluate an xquery from minibuffer in the modules database
- document management:
  - C-c m l - List documents †
  - C-c m L - List documents in the modules database †
  - C-c m s - Show document
  - C-c m S - Show document in the modules database
  - C-c m u - Upload a document
  - C-c m U - Upload a document in the modules database
  - C-c m d - Delete a document
  - C-c m D - Delete a document in the modules database  
- database selection:
  - C-c m c - Choose/select database within current session/connection
  - C-c m . - Select default database of the server
  - C-c m , - Select modules database of the server
  - C-c m - - Show which database is currently used
  - C-c m / - Show which database is currently used
- LW configuration service:  
  (Just ignore this section you don't have such a service or don't know what it is.)
  - C-c m g - Get connection settings for ML connection from LW configuration service

† - For paged output, set page limit with xdmp-set-page-limit.
    Use numerical prefix to switch to a different page.  
    (Cannot list documents with an URL not beginning with a '/'.)

#### Note on upload and delete document methods
To use upload and delete document you should have the file that
is to be transfered or delete open in a buffer. If you fire
the command the filename will be taken from the buffer and
you will be interactively queried to enter a directory path.
The file will be uploaded with a uri of <directory>/<filename>.
Delete works analogously.
