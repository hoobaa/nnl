(in-package :cl-user)
(asdf:defsystem :nnl
  :version 1.0
  :description "naka naka language. basic utility. on lisp, lol."
  :author "d.n. <strobolights@gmail.com>"
  ;; :depends-on (:cl-ppcre :series :trees :drakma)
  :depends-on (:cl-ppcre :series :trees)
  :components
  ((:file "pkg")
   (:file "nnl" :depends-on ("pkg"))
   ))
  
