(in-package :nnl)

;; common util.

(defmacro eval-always ( &body body )
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-always
 (defun mkstr (&rest args)
   (with-output-to-string (s)
     (dolist (a args) (princ a s))))
 (defun symb (&rest args)
   (values (intern (apply #'mkstr args))))
 (defun group (source n)
   (if (zerop n) (error "zero length"))
   (labels ((rec (source acc)
              (let ((rest (nthcdr n source)))
                (if (consp rest)
                    (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
     (if source (rec source nil) nil)))
 (defun flatten (x)
   (labels ((rec (x acc)
              (cond ((null x) acc)
                    ((atom x) (cons x acc))
                    (t (rec (car x) (rec (cdr x) acc))))))
     (rec x nil)))
 )

(defmacro ef (test-form then-form &body else-form)
  "emacs like if. else-form is wrapped progn."
  `(if ,test-form
       ,then-form
       (progn
         ,@else-form)))
(defmacro aif (test-form then-form &body else-form)
  "based on ef"
  `(let ((it ,test-form))
     (if it 
         ,then-form 
         (progn ,@else-form))))

(defmacro aif2 (test then &body else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
      (if (or it ,win) ,then 
        (progn
          ,@else)))))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(eval-always
  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2)))
  )

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
               syms)
         ,@body))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (if os ;; if o! not exist, same as defmacro/g!
        `(defmacro/g! ,name ,args
           `(let ,(mapcar #'list (list ,@gs) (list ,@os))
              ,(progn ,@body)))
        `(defmacro/g! ,name ,args
           (progn ,@body)))))

(eval-always
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))
  (set-dispatch-macro-character
   #\# #\` #'|#`-reader|)
  )

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                           g!args
                           `(cdr ,g!args)))))
          ds))))

(eval-always
 (declaim (inline get-pandoric pget))
 (defun get-pandoric (box sym)
   (funcall box :pandoric-get sym))
 (defsetf get-pandoric (box sym) (val)
   `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

 ;; short name
 (defun pget (box sym)
   (funcall box :pandoric-get sym))
 (defsetf pget (box sym) (val)
   `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

 (defun pandoriclet-get (letargs)
   `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t 
      ;; do nothing
      (error "Unknown pandoric get: ~a" sym)
      )
     ))
 (defun pandoriclet-set (letargs)
   `(case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t 
      ;; do nothing
      (error "Unknown pandoric set: ~a" sym)
      )
     ))
 )

(defmacro w/pand (syms box &body body)
  (let ((g!box (gensym "box")))
    `(let ((,g!box ,box))
       (declare (ignorable ,g!box))
       (symbol-macrolet
         (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                    syms))
         ,@body))))

(defmacro with-pandoric (syms box &body body)
  `(w/pand ,syms ,box ,@body))

;;(setf (symbol-function 'w-pand) (symbol-function 'with-pandoric))

(defmacro pprogn (pargs &body body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
        this (progn
               ,@body)
        self (dlambda
              (:pandoric-get (sym)
                             ,(pandoriclet-get pargs))
              (:pandoric-set (sym val)
                             ,(pandoriclet-set pargs))
              (t (&rest args)
                 (apply this args)))))))

(defmacro plambda (largs pargs &body body)
  `(pprogn ,pargs (lambda ,largs ,@body)))

(defmacro defpan (name largs pargs &body body)
  "like fuckin OO's method definition or something. modify from LoL. I added largs."
  `(defun ,name (self ,@largs)
     ,(if pargs
          `(with-pandoric ,pargs self
                          ,@body)
          `(progn ,@body))))

;; (defpan test (a b) (_x _y)
;;         (incf _x a)
;;         (decf _y b)
;;         (list _x _y))

(eval-always
 (set-dispatch-macro-character 
  #\# #\f
  (lambda (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "Bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg))))))
 )

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #`(,a1 (gensym ,(mkstr a1 "-")))
                 syms)
     ,@body))

(defmacro add-nickname-to-package (nick pkg)
  `(rename-package 
    ,pkg ,pkg 
    (adjoin ,nick (package-nicknames ,pkg) :test #'string-equal)))

(eval-always
 (add-nickname-to-package :r :cl-ppcre)
 (add-nickname-to-package :s :series)
 )

;;;; ppcre patch.


;;; short name 
(eval-always
 ;; global short name
 (setf (symbol-function 'cat) (symbol-function 'concatenate))

 ;; ppcre short name

 (setf (symbol-function 'r::c) (symbol-function 'r:create-scanner))
 (export 'r::c :r)
 (setf (symbol-function 'r::s) (symbol-function 'r:scan))
 (export 'r::s :r)
 (setf (symbol-function 'r::r) (symbol-function 'r:regex-replace))
 (export 'r::r :r)
 (setf (symbol-function 'r::r-all) (symbol-function 'r:regex-replace-all))
 (export 'r::r-all :r)
 (setf (symbol-function 'r::split) (symbol-function 'r:split))
 (export 'r::split :r)

 (defmacro r::$ (&optional flg)
   (error "dummy macro"))
 (export 'r::$ :r)

 (defmacro! r::do-scans2 ( (rex o!tgt &key result-form (start 0) end) &body body)
   "easy to use with macrolet."
   `(macrolet ((r::$ (&optional flg)
                (check-type flg (or integer (member nil :tgt :mf :mt :rf :rt)))
                (cond
                  ((integerp flg)
                     `(aand
                       ,',g!rf
                       (aref it ,flg) 
                       (subseq ,',g!tgt it (aref ,',g!rt ,flg))))
                  ((eq :tgt flg) ',g!tgt)
                  ((eq :mf flg) ',g!mf)
                  ((eq :mt flg) ',g!mt)
                  ((eq :rf flg) ',g!rf)
                  ((eq :rt flg) ',g!rt)
                  ((null flg)
                     `(subseq ,',g!tgt ,',g!mf ,',g!mt))
                  )))
     (ppcre:do-scans (,g!mf ,g!mt ,g!rf ,g!rt ,rex ,g!tgt ,(if result-form result-form) :start ,start ,@(if end `(:end ,end)))
       ,@body
       )))
 (export 'r::do-scans2 :r)

 (defmacro! r::rif ( (rex o!tgt &key (start 0) end) ex-t &body ex-f)
   `(macrolet ((r::$ (&optional flg)
                (check-type flg (or integer (member nil :tgt :mf :mt :rf :rt)))
                (cond
                  ((integerp flg)
                     `(aand
                       ,',g!rf
                       (aref it ,flg) 
                       (subseq ,',g!tgt it (aref ,',g!rt ,flg))))
                  ((eq :tgt flg) ',g!tgt)
                  ((eq :mf flg) ',g!mf)
                  ((eq :mt flg) ',g!mt)
                  ((eq :rf flg) ',g!rf)
                  ((eq :rt flg) ',g!rt)
                  ((null flg)
                     `(subseq ,',g!tgt ,',g!mf ,',g!mt))
                  )))
     (multiple-value-bind (,g!mf ,g!mt ,g!rf ,g!rt) (r::s ,rex ,g!tgt :start ,start ,@(if end `(:end ,end)))
       (ef ,g!mf
           ,ex-t
         ,@ex-f))))
 (export 'r::rif :r)

 (defmacro r::rwhen ( (rex tgt &key (start 0) (end (length tgt))) &body body )
   `(r::rif (,rex ,tgt :start ,start :end ,end)
     (progn
       ,@body)
     ))
 (export 'r::rwhen :r)

 (defmacro r::rif-not ( (rex tgt &key (start 0) (end (length tgt))) ex-f &body ex-t)
   `(r::rif (,rex ,tgt :start ,start :end ,end)
     (progn
       ,@ex-t)
     ,ex-f))
 (export 'r::rif-not :r)

 (defun r::join (glue &rest args )
   (let (res)
     (loop 
       for elm in args
       do (progn
            (push elm res)
            (push glue res)))
     (pop res)
     (apply #'concatenate 'string (nreverse res) )))
 (export 'r::join :r)

 )


(eval-always
 (defun |#"-reader| (stream sub-c num &aux flags string splitter)
   (declare (ignore sub-c num))
   "easy to use like perl. from LOL."
   (setq flags
         (s:collect 'list
           (s:until-if (lambda (x) 
                         (if (alpha-char-p x)
                             nil
                           (setf splitter x)))
             (s:scan-stream stream #'read-char))))
     
   (let ((ppre #\")
         (pre (read-char stream)))
     (setq string
           (s:collect 'string
             (s:until-if #'null
               (s:mapping ((in (s:scan-stream stream #'read-char)))
                 (setq ppre pre
                       pre in)
                 (cond
                   ((and (char= pre #\#) (char= ppre #\"))
                      nil)
                   (t
                      ppre)))))))

   (cond
     ((member #\q flags)
        string)
     ((member #\c flags) ;; create scanner
        `(ppcre:create-scanner ,string))
     ((member #\s flags) ;; scan
        (with-gensyms (tgt)
          `(lambda (,tgt)
            #f
            (ppcre:scan ,string ,tgt)))
        )
     ((member #\r flags) ;; replace
        (with-gensyms (tgt)
          (destructuring-bind (from to) (r:split (format nil "~c" splitter) string)
            `(lambda (,tgt )
              #f
              (r:r ,from ,tgt ,to)))))
     ((member #\R flags) ;; replace-all
        (with-gensyms (tgt)
          (destructuring-bind (from to) (r:split (format nil "~c" splitter) string)
            `(lambda (,tgt )
              #f
              (r:r-all ,from ,tgt ,to)))))
          
          ))

 (set-dispatch-macro-character
  #\# #\" #'|#"-reader|)
 )


;;;; on lisp continuation. continuation is useful when imprementing somekinda messaging.
(eval-always
 (defvar ==cont #'values) ;; because sbcl, closure top setq is weird. to let =cont to be lexical, I use symbol-macro.
 (define-symbol-macro =cont ==cont)
 )

(defmacro =lambda (parms &body body) 
  `(lambda (=cont ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f =cont ,,@parms))
       (defun ,f (=cont ,@parms) ,@body))))
 
(defmacro =bind (parms expr &body body)
  `(let ((=cont (lambda ,parms ,@body))) 
     ,expr))
    
(defmacro =values (&rest retvals)
  `(funcall =cont ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn =cont ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn =cont ,@args))


(defmacro comment (&body body) ;; comment
  )

;;;; time
(defun sec2internal-time-span (sec)
  (* sec internal-time-units-per-second))


;;;; queue from LOL
(eval-always
 (declaim (inline tlist-left tlist-right tlist-empty-p tlist-add-left tlist-add-right tlist-rem-left))

 (defun make-tlist () 
   (cons nil nil))
 (defun tlist-left (tl) (caar tl))
 (defun tlist-right (tl) (cadr tl))
 (defun tlist-empty-p (tl) (null (car tl)))

 (defun tlist-add-left (tl it)
   (let ((x (cons it (car tl))))
     (if (tlist-empty-p tl)
         (setf (cdr tl) x))
     (setf (car tl) x)))

 (defun tlist-add-right (tl it)
   (let ((x (cons it nil)))
     (if (tlist-empty-p tl)
         (setf (car tl) x)
       (setf (cddr tl) x))
     (setf (cdr tl) x)))

 (defun tlist-rem-left (tl)
   (if (tlist-empty-p tl)
       (error "Remove from empty tlist")
     (let ((x (car tl)))
       (setf (car tl) (cdar tl))
       (if (tlist-empty-p tl)
           (setf (cdr tl) nil)) ;; For gc
       (car x))))
 )

;; on lisp around setf
(proclaim '(inline last1 single append1 conc1 mklist mkatom))
(defun last1 (lst &optional (n 1)) ;; calast like cadr
  (car (last lst n)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (lst obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defun mkatom (obj)
  (if (listp obj) (car obj) obj))

(defun filter (fn lst &aux acm)
  (dolist (x lst)
    (awhen (funcall fn x)
      (push it acm)))
  (nreverse acm))

(defun split-if (fn lst &aux acm)
  (loop for src on lst
        for tgt = (car src)
        until (funcall fn tgt)
        do (push tgt acm)
        finally (return (values (nreverse acm) src))))

(defun split (obj lst &key (test #'eql))
  (split-if (lambda (x) (funcall test obj x))
   lst))
  

(defmacro! allf (o!val &rest args)
  `(setf ,@(mapcan #`(,a1 ,g!val) args)))

(define-modify-macro concf (obj) nconc)
(define-modify-macro 
    conc1f (obj) (lambda (place obj)
                   (nconc place (list obj))))
(define-modify-macro 
    concnew (obj &rest args)
    (lambda (place obj &rest args)
      (if (apply #'member obj place args)
          place
        (nconc place (list obj))
        )))
;;
(defmacro _f (op place &rest args)
  ;; (incf (aref ar 3) 200) === (_f + (aref ar 3) 200)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            ( ,(car var) (,op ,access ,@args)) )
      ,set)))

(defmacro! pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    `(let* ((,g!obj ,obj)
            ,@(mapcar #'list vars forms)
            ( ,(car var) (delete ,g!obj ,access ,@args)))
      ,set)))

(defmacro! pull-if (fn place &rest args)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    `(let* ((,g!fn ,fn)
            ,@(mapcar #'list vars forms)
            ( ,(car var) (delete-if ,g!fn ,access ,@args)))
      ,set)))

;; popn




;;;;; red-black tree
(defmacro makerb (&key (test #'<))
  `(trees:make-binary-tree :red-black ,test :key #'car))

(defun getrb (key rb)
  (multiple-value-bind (val valp) (trees:find key rb)
    (values (cdr val) valp)))

;;(import 'nnl:aif2)
(defun (setf getrb) (obj key rb)
  (aif2 (trees:find key rb)
   (setf (cdr it) obj)
   (trees:insert (cons key obj) rb)))

(defmacro remrb (key rb)
  `(aif2 (trees:delete ,key ,rb)
    (values (cdr it) t)
    (values nil nil)))

(defun maxrb (rb &optional reversep)
  (cond
    ((trees:emptyp rb)
     (values nil nil))
    (t (let ((val (if reversep (trees:minimum rb) (trees:maximum rb))))
         (values (car val) (cdr val))))))

(defun pop-maxrb (rb &optional reversep)
  (multiple-value-bind (k v) (maxrb rb reversep)
    (ef k
        (progn
          (remrb k rb)
          (values k v))
      (values nil nil))))

(defun rb-lower-bound (key rb &optional reversep)
  (cond
    ((trees:emptyp rb)
     (values nil nil))
    (t (let ((val (if reversep (trees:upper-bound key rb) (trees:lower-bound key rb))))
         (values (car val) (cdr val))))))

(defun minrb (rb)
  (maxrb rb t))
(defun pop-minrb (rb)
  (pop-maxrb rb t))
(defun rb-upper-bound (key rb)
  (rb-lower-bound key rb t))

;;;;;; directory
;;(defun walk-dir (dir fn 
;;                 &aux
;;                 (tlist-of-dirs (make-tlist))
;;                 )
;;  (tlist-add-right tlist-of-dirs dir)
;;
;;  (loop
;;    (let ((tgt (tlist-left tlist-of-dirs))
;;          tgt-path)
;;      (ef (not tgt)
;;          (return)
;;
;;        ;; dequeue
;;        (tlist-rem-left tlist-of-dirs)
;;
;;        (dolist (tgtfile (directory (merge-pathnames "*.*" tgt) :directories nil))
;;          (funcall fn tgtfile))
;;        (dolist (tgtfile (directory (merge-pathnames "*/" tgt)))
;;          (funcall fn tgtfile)
;;          (tlist-add-right tlist-of-dirs tgtfile))
;;        ))))

(defun walk-directory (dirname fn)
  (dolist (file (directory (merge-pathnames "*.*" dirname) :directories nil))
    (funcall fn file))
  (dolist (file (directory (merge-pathnames "*/" dirname)))
    (funcall fn file)
    (walk-directory file fn)))

;;;; log
(defparameter file-log-path "/tmp/file-log.lisp")
(defun file-log (&rest rest)
  (with-open-file (os file-log-path :direction :output :if-does-not-exist :create :if-exists :append)
    (multiple-value-bind (sec min hour day month year) (get-decoded-time)
      (format os "(~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d " year month day hour min sec)
      #+ccl(format os "(~a ~a) " (#_getpid) (ccl:process-serial-number ccl:*current-process*))
      (apply #'format os rest)
      (format os ")~%"))))



