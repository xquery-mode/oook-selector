;;; Unzip gzipped binaries on cider-any-uruk-eval-form

;;; Commentary:
;;; - if binary is zipped, unzip it and just output the contents
;;; - on other binaries: show informational string "<ginary node of <n> bytes {0x<8 bytes>...}"
;;; - if neither is true, just try to convert via Clojure's "str" function (old behaviour)

;;; Code:

(require 'cider-eval-form)

(cider-eval-form "
(defn unzip
  ([string]
   (unzip string \"UTF-8\"))
  ([string encoding]
   (let [input-stream (clojure.java.io/input-stream string)]
     (with-open [out (java.io.ByteArrayOutputStream.)]
       (org.apache.commons.io.IOUtils/copy (java.util.zip.GZIPInputStream. input-stream) out)
       (.close input-stream)
       (.toString out encoding)))))

(defn hexify \"Convert byte sequence to hex string\" [coll]
  (let [hex [\\0 \\1 \\2 \\3 \\4 \\5 \\6 \\7 \\8 \\9 \\a \\b \\c \\d \\e \\f]]
      (letfn [(hexify-byte [b]
        (let [v (bit-and b 0xFF)]
          [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
        (apply str (mapcat hexify-byte coll)))))

(defn maybe-unzip [data]
  (if (= (type data) (Class/forName \"[B\"))
    ;; is byte-array => assume gzipped data and try to unzip
    (try
      (unzip data)
      ;; if not successfull => returnt info string
      (catch Exception e
        (format \"<binary node of %s bytes> {0x%s%s}\" (alength data) (hexify (take 8 data)) (if (> (alength data) 8) \"...\" \"\"))))
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
