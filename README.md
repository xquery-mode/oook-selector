# Rationale

This is the beginning of convenience tools for interacting with MarkLogic.

## Installation

Get xquery-mode, cider-any, and page-break-lines and put them in ~/src/emacs/
```
  git clone https://github.com/xquery-mode/cider-any.git
  git clone https://github.com/xquery-mode/xquery-mode.git
  git clonehttps://github.com/purcell/page-break-lines.git
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

## Usage

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
Than cider-jack-in to that project and try the xdbc-selecter by entering C-cm;
there is help if you press '?' afterwards
