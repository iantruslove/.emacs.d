;; Phantom REPL for e.g. inf-clojure mode
;;
;; M-x cljs-start-phantom-repl

(require 'cemerick.piggieback
         'cemerick.austin
         'cljs.repl)
(cljs.repl/repl (cemerick.austin/exec-env :phantom-cmd "phantomjs"))
