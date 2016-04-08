
(in-package :vdom.paren)

(defpssyslib "vdom" :vdom.paren
  :runtime *runtime*)

(defparameter *vdom-node* '(virtual-dom h))
(defparameter *vdom-dom* '*vdom-d-o-m)

(defparameter *with-self* t
  "Define self variable inside each function in component")

(defvar *supported-vdom-tags*
  '(a abbr address area article aside audio b base bdi bdo big blockquote body br
    button canvas caption cite code col colgroup data datalist dd del details dfn
    div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6
    head header hr html i iframe img input ins kbd keygen label legend li link
    main map mark menu menuitem meta meter nav noscript object ol optgroup option
    output p param pre progress q rp rt ruby s samp script section select small
    source span strong style sub summary sup table tbody td textarea tfoot th
    thead time title tr track u ul var video wbr)
  "Tags that are converted to vdom classes")

(defun keyword-to-sym (kwd)
  (intern (symbol-name kwd) :vdom.paren))

(defun supported-tag-p (tag)
  "Check if tag is supported by looking through *SUPPORTED-VDOM-TAGS*"
  (member (keyword-to-sym tag) *supported-vdom-tags* :test #'equal))

(defun fixup-attributes (attrs)
  "this does some name fixing for who vs paren vs js object props
   class -> className
   some-prop -> someProp"
  (mapcar #'(lambda (attr)
              (if (keywordp attr)
                  (cond ((eq attr :class) 'class-name)
                        ((find #\- (symbol-name attr)) (keyword-to-sym attr))
                        (t attr))
                  attr))
          attrs))

(defun process-body (sexp body-fn)
  (mapcan #'(lambda (def)
              (cond ((stringp def) (list def))
                    ((and (listp def) (keywordp (car def))) (list (funcall body-fn def)))
                    ((listp def) (list (funcall body-fn def)))
                    (t (list def))))
          sexp))

(defun tree-to-vdom (sexp)
  "Convert sexp tree to vdom classes"
  (let (comp-init tag attr-list body)
    (when (eql (car sexp) '%)
      (setf comp-init t
            sexp (cdr sexp)))
    (cond
      ((atom (first sexp))
       (setf tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (list (first rest) (second rest)) into attr
             else
               do (progn (setf attr-list (apply #'nconc attr))
                         (setf body rest)
                         (return))
             finally (setf attr-list (apply #'nconc attr))))
      ((listp (first sexp))
       (setf tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
          if (keywordp (first rest))
            collect (list (first rest) (second rest)) into attr
          finally (setf attr-list (apply #'nconc attr)))
       (setf body (cdr sexp))))
    (when attr-list
      (setf attr-list (fixup-attributes attr-list)))

    (cond ;; (comp-init
          ;;  `((,*vdom-name*)
          ;;    ,(keyword-to-sym tag)
          ;;    (create ,@attr-list)
          ;;    (array ,@(process-body body #'tree-to-vdom))))
          ((supported-tag-p tag)
           `(chain virtual-dom (h ,(string-downcase (symbol-name tag))
                                  (create ,@attr-list)
                                  ,(if (or (> (length body) 1)
                                           (and (listp (car body))
                                                (supported-tag-p (caar body))))
                                       `(array ,@(process-body body #'tree-to-vdom))
                                       `(progn ,@(process-body body #'tree-to-vdom))))))
          (t `(,@sexp)))))
