* [x] idea: maybe use a separate history when temporarily switched to modules database (20160921 mgr)
**    => Not done but not necessary anymore as of xdmp-buffer-path and xdmp-buffer-database.
* [x] C-c m t takes region instead of current line if region is marked, otherwise the string at point
* [x] Return key in file list opens document at point
* [ ] use an Emacs rest interface to implement xdmp-document-load/rest-from-file without lambdawerk.marklogic.
* [ ] Add a xdmp-delete-document that asks for an URI of a document (and not for a directory like the current version; rename that wann to xdmp-delete-this-document); see Issue 3
* [ ] show collections of document:
(->  (lambdawerk.marklogic.calls/get-document
            "/requests/0:0:0:0:0:0:0:1/2017-03-07T10:08:01.945Z/request.txt"
            (lambdawerk.marklogic.db-query/get-connection-spec {:connection-name :finch-server :server-type :rest})
            {:category "collections"})
           print)
* [ ] show documents in collection:
for $d in cts:search(/, cts:collection-query("request-f66b585e-fb54-4441-81d8-ab97264939c2")) return xdmp:node-uri($d)
