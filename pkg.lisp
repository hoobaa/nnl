(in-package :cl-user)

(defpackage #:nnl
    (:use #:common-lisp)
  (:export
   #:eval-always
   #:mkstr #:symb #:flatten #:group
   #:ef #:aif #:awhen #:awhile #:aand #:acond #:alambda #:it
   #:aif2

   #:defmacro!

   #:cat
   
   #:dlambda #:with-pandoric #:plambda #:pprogn #:defpand
   #:with-gensyms
   #:get-pandoric 
   #:pget #:w/pand
   #:self #:this

   #:add-nickname-to-package

   #:=lambda #:=defun #:=bind #:=values #:=funcall #:=apply

   #:sec2internal-time-span

   #:make-tlist 
   #:tlist-left #:tlist-right #:tlist-empty-p #:tlist-add-left #:tlist-add-right #:tlist-rem-left

   #:last1 #:single #:append1 #:conc1 #:mklist #:allf #:concf #:conc1f #:concnew #:mkatom
   #:_f #:pull #:pull-if

   #:filter
   #:split-if #:split

   #:makerb #:getrb #:remrb #:maxrb #:pop-maxrb #:rb-lower-bound #:minrb #:pop-minrb #:rb-upper-bound
   ))
