
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

(defun parse-defun (def)
  (let ((defun-form (car def))
        (name (cadr def))
        (lambda-list (caddr def))
        (body (cdddr def)))
    (values defun-form name lambda-list body)))

(defun transform-defun-form (defun-form)
  ;'lambda/contract)
  (case defun-form
    (defun 'lambda)
    (defun/contract 'lambda/contract)))

(defun process-props (component-name props-contracts)
  ""
  (let ((res '()))
    (do ((i 0 (+ i 2)))
        ((>= i (length props-contracts)))
      (progn
        (let ((expected (with-output-to-string (out)
                          (format out "~A" (nth (+ i 1) props-contracts)))))
          (push (nth i props-contracts) res)
          (push `(lambda (props prop-name component)
                   (when (not (,(nth (+ i 1) props-contracts)
                               (getprop props prop-name))) ;; prop-name component-name))
                     (blame (create :type "vdom-component"
                                    :function ,(symbol-name component-name)
                                    :variable prop-name
                                    :given (getprop props prop-name)
                                    :expected ,expected))
                     (return (new (*error (+ "Prop " prop-name " validation failed"))))))
              res))))
    (nreverse res)))
  ;; (list (car props)
  ;;       (cadr props)))
  ;; customProp: function(props, propName, componentName) {
  ;;     if (!/matchme/.test(props[propName])) {
  ;;       return new Error('Validation failed!');
  ;;     }
  ;;   }

;; (defvcomponent hello ()
;;   ())

;; (daefjsclass ,name (*vdom-class*
;;                    ,super)
;;   (defun render ()
;;     ))

(defpsmacro defvcomponent (name super-classes &rest body)
  (let ((sc (or (and super-classes (append (list *vdom-superclass-name*) super-classes))
                (list *vdom-superclass-name*))))
    `(defjsclass ,name (,@sc)
       ,@body)))

;; (defpsmacro defvcomponent (name &rest body)
;;   "Define vdom component"
;;   (flet ((process-form (def)
;;            (cond ((equalp (car def) :>>)
;;                   ;; this adds support to define parenscript contracts
;;                   ;; inside the component, which is transformed into
;;                   ;; propTypes
;;                   `(prop-types (create ,@(process-props name (cdr def))))) ; mapcan #'process-props (cdr def)))))
                 
;;                   ((= (length def) 2)
;;                    `(,(car def) ,@(cdr def)))
                 
;;                  (t (multiple-value-bind (defun-form name lambda-list body)
;;                         (parse-defun def)
;;                       (case defun-form
;;                         (defun `(,name (lambda ,lambda-list
;;                                          ,(when *with-self*
;;                                                 `(var self this))
;;                                          (progn ,@body))))
;;                         (defun/contract `(,name (lambda/contract ,lambda-list
;;                                                                  ,@body)))))))))
;;     `(chain ,*vdom-name* (create-class (create 'display-name ,(symbol-name name)
;;                                                 ,@(mapcan #'process-form body))))))

(defpsmacro defcomponent (name &rest body)
  "Bind component to a variable of the same name as component"
  `(var ,name (component ,name ,@body)))

(defpsmacro render-component (name dom)
  "Render component inside dom node"
  `(chain ,*vdom-dom* (render ,name ,dom)))

(defpsmacro render (name dom)
  "Render component inside dom node"
  `(chain ,*vdom-dom* (render ,name ,dom)))

(defpsmacro vwho (&rest body)
  "Transform cl-who like forms into vdom nodes"
  (when (and (listp body) (listp (car body)))
    (tree-to-vdom (car body))))

(defpsmacro set-state% (&rest body)
  "Helper macro to set state"
  (if *with-self*
      `(chain self (set-state (create ,@body)))
      `(chain this (set-state (create ,@body)))))

(defun keyword-to-sym (kwd)
  (intern (symbol-name kwd) :vdom.paren))

;; (defun keyword-to-str (kwd)
;;   (keyword-to-sym

(defun supported-tag-p (tag)
  (member (keyword-to-sym tag) *supported-vdom-tags* :test #'equal))

(defun process-body (sexp body-fn)
  (mapcan #'(lambda (def)
              (cond ((stringp def) (list def))
                    ((and (listp def) (keywordp (car def))) (list (funcall body-fn def)))
                    ((listp def) (list (funcall body-fn def)))
                    (t (list def))))
          sexp))

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

(defun tree-to-vdom (sexp)
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

    ;; (format t "~A~%" sexp)
    ;; (format t "~A~%~%" (caar body))
    
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
