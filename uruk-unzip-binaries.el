;;; Unzip gzipped binaries on cider-any-uruk-eval-form

;;; Commentary:

;;; Code:

(require 'cider-eval-form)

(cider-eval-form "
(defn gunzip
  ([string]
   (gunzip string \"UTF-8\"))
  ([string encoding]
   (let [input-stream (clojure.java.io/input-stream string)]
     (with-open [out (java.io.ByteArrayOutputStream.)]
       (org.apache.commons.io.IOUtils/copy (java.util.zip.GZIPInputStream. input-stream) out)
       (.close input-stream)
       (.toString out encoding)))))

(defn maybe-unzip [data]
  (or (and (= (type data) (Class/forName \"[B\"))
           ;; is byte-array => assume gzipped data and try to unzip
           (try
            (gunzip data)
            ;; if not successfull => fail and let the default case handle it
            (catch Exception e nil)))
      ;; something else (string, number, ...) => convert to String using str
      (str data)))
  ")


(defun cider-any-uruk-eval-form ()
  "Clojure form for XQuery document revaluation."
  (format "(do
             (require '[uruk.core :as uruk])
             (set! *print-length* nil)
             (set! *print-level* nil)
             (let [host \"%s\"
                   port %s
                   db %s]
               (with-open [session (uruk/create-default-session (uruk/make-hosted-content-source host port db))]
                 (doall (map maybe-unzip (uruk/execute-xquery session \"%%s\"))))))"
          (plist-get cider-any-uruk-connection :host)
          (plist-get cider-any-uruk-connection :port)
          (cider-any-uruk-plist-to-map cider-any-uruk-connection)))


(provide 'cider-any-uruk-unzip-binaries)
