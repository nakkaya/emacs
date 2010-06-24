;; enable skeleton-pair insert globally
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq abbrev-mode t)

(add-hook 'clojure-mode-hook 
	  (lambda ()
	    (setq local-abbrev-table clojure-mode-abbrev-table)))

(define-abbrev-table 'java-mode-abbrev-table '())
(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())
(define-abbrev-table 'clojure-mode-abbrev-table '())
(define-abbrev-table 'c++-mode-abbrev-table '())

;;clojure
(define-skeleton skel-clojure-println
  ""
  nil
  "(println "_")")
(define-abbrev clojure-mode-abbrev-table "prt" "" 'skel-clojure-println)

(define-skeleton skel-clojure-defn
  ""
  nil
  "(defn "_" [])")
(define-abbrev clojure-mode-abbrev-table "defn" "" 'skel-clojure-defn)

(define-skeleton skel-clojure-defn-
  ""
  nil
  "(defn- "_" [])")
(define-abbrev clojure-mode-abbrev-table "def-" "" 'skel-clojure-defn- )

(define-skeleton skel-clojure-if
  ""
  nil
  "(if ("_"))")
(define-abbrev clojure-mode-abbrev-table "if" "" 'skel-clojure-if )

(define-skeleton skel-clojure-let
  ""
  nil
  "(let ["_"] )")
(define-abbrev clojure-mode-abbrev-table "let" "" 'skel-clojure-let)

(define-skeleton skel-clojure-ref-set
  ""
  nil
  "(dosync (ref-set "_" ))")
(define-abbrev clojure-mode-abbrev-table "refs" "" 'skel-clojure-ref-set)

(define-skeleton skel-clojure-proxy
  ""
  nil
  "(proxy ["_"] [] "
    \n > ")")
(define-abbrev clojure-mode-abbrev-table "proxy" "" 'skel-clojure-proxy)

(define-skeleton skel-clojure-doseq
  ""
  nil
  "(doseq ["_"] "
    \n > ")")
(define-abbrev clojure-mode-abbrev-table "doseq" "" 'skel-clojure-doseq)

(define-skeleton skel-clojure-do
  ""
  nil
  "(do "_" "
    \n > ")")
(define-abbrev clojure-mode-abbrev-table "do" "" 'skel-clojure-do)

(define-skeleton skel-clojure-reduce
  ""
  nil
  "(reduce (fn[h v] ) "_" ) ")

(define-abbrev clojure-mode-abbrev-table "reduce" "" 'skel-clojure-reduce)

(define-skeleton skel-clojure-try
  ""
  nil
  "(try "_" (catch Exception e (println e)))")

(define-abbrev clojure-mode-abbrev-table "try" "" 'skel-clojure-try)

(define-skeleton skel-clojure-map
  ""
  nil
  "(map #() "_")")

(define-abbrev clojure-mode-abbrev-table "map" "" 'skel-clojure-map)

(define-skeleton skel-cpp-prt
  ""
  nil
  \n >
  "cout<< " _ " <<endl;"
  \n >)
(define-abbrev c++-mode-abbrev-table "cout"  "" 'skel-cpp-prt )


(define-skeleton skel-list-insert
  ""
  nil
  "(insert "_" )")
(define-abbrev lisp-mode-abbrev-table "ins" "" 'skel-list-insert )

(define-skeleton skel-list-setq
  ""
  nil
  "(setq "_" )")
(define-abbrev lisp-mode-abbrev-table "set" "" 'skel-list-setq )

(define-skeleton skel-list-deffun
  ""
  nil
  "(defun "_" () "
  \n >
  ")")
(define-abbrev lisp-mode-abbrev-table "deff" "" 'skel-list-deffun )

(define-skeleton skel-list-defvar
  ""
  nil
  "(defvar "_" )")
(define-abbrev lisp-mode-abbrev-table "defv" "" 'skel-list-defvar )

(define-skeleton skel-list-if
  ""
  nil
  "(if "
  _
  \n >
  " )")
(define-abbrev lisp-mode-abbrev-table "if" "" 'skel-list-if )

(define-skeleton skel-list-progn
  ""
  nil
  "(progn "
  _
  \n >
  " )")
(define-abbrev lisp-mode-abbrev-table "progn" "" 'skel-list-progn )

(define-skeleton skel-java-println
  "Insert a Java println Statement"
  nil
  "System.out.println(" _ " );")
(define-abbrev java-mode-abbrev-table "prt" "" 'skel-java-println )

(define-skeleton skel-java-alert
  "insert alert statement for javascript"
  nil
  "alert( " _ " );")
(define-abbrev java-mode-abbrev-table "alt" "" 'skel-java-alert )

(define-skeleton skel-java-ife
  "Insert a Common If else Statement"
  nil
  \n >
  "if (" _ " ){"
  \n >
  "} else {"
  \n >
  "}")
(define-abbrev java-mode-abbrev-table "ife" "" 'skel-java-ife )

(define-skeleton skel-java-try
  "Insert a try catch block"
  nil
  \n >
  "try{"
  \n >
  _ \n
  "}catch( Exception e ) {" >
  " "
  \n > \n
  "}" >)
(define-abbrev java-mode-abbrev-table "try" "" 'skel-java-try )

(define-skeleton skel-java-if
  "Insert a Common If Statement"
  nil
  \n >
  "if (" _ " ){"
  \n >
  "}")

(define-abbrev java-mode-abbrev-table "if" "" 'skel-java-if )
(define-abbrev c++-mode-abbrev-table "if"  "" 'skel-java-if )

(define-skeleton skel-java-for1
  "Insert a Common If Statement"
  nil
  \n >
  "for( int i=0 ; i<" _ " ;i++){"
  \n >
  "}")
(define-abbrev java-mode-abbrev-table "for1" "" 'skel-java-for1 )


(define-skeleton skel-java-timer
  "creates timing statements"
  nil
  \n >
  "final long start = System.currentTimeMillis();"
  \n >
  "System.out.println( Long.toString( System.currentTimeMillis() - start ) ) ;")
(define-abbrev java-mode-abbrev-table "jtimer" "" 'skel-java-timer )

(define-skeleton skel-java-comment
  "creates javadoc comment"
  nil
  \n >
  "/*"
  \n >
  "*"
  \n >
  "*"
  \n >
  "*"
  \n >
  "* @param"
  \n >
  "* @return"
  \n >
  "* @exception"
  \n >
  "*"
  \n >
  "*/"
  \n >)
(define-abbrev java-mode-abbrev-table "jdcomment" "" 'skel-java-comment )
