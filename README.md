# Rationale

This is the beginning of convenience tools for interacting with MarkLogic.

## Installation

Get xquery-mode, cider-any, and page-break-lines and put them in ~/src/emacs/
```
  git clone https://github.com/xquery-mode/cider-any.git
  git clone https://github.com/xquery-mode/xquery-mode.git
  git clone https://github.com/purcell/page-break-lines.git
```

Put this in your ~/.emacs or ~/.emacs.d/init.el:

```
(let ((default-directory  "~/src/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory  "~/src/lambdawerk-dev/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'mark-logic)
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
                 [uruk "0.3.0"]]
  :main ^:skip-aot uruk-gw.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
__EOL__
```

## Usage

Currently the database/xdbc connections are to be configured by changing
the function xdmp-select-db in the beginning of mark-logic/xdmp-methods.el.
You configure a primary connection and a seconderay one (the "other" db).

Than cider-jack-in to a uruk project and try the xdbc-selecter by entering C-cm
There is help if you press '?' afterwards.

With  C-cm q  you can just evaluate a query from a minibuffer.

All other commands have usually a lowercase version for the primary db, and
a UPPERCASE one for the other db.
 C-cm l / L  -  list documents
 C-cm u / U  -  upload a documents (by xdmp:document-load)
 C-cm d / D  -  delete a documents (by xdmp:document-delete)

For upload and delete document you should have the file that
is to be transfered or delete open in a buffer. If you fire
the command the filename will be taken from the buffer and
you will be interactively queried to enter a directory path.
The file will be uploaded with a uri of <directory>/<filename>.
Delete works analogously.