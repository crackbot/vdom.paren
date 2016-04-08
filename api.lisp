
(in-package :vdom.paren)

(defsection @main-tutorial (:title "Main")
  "This is a binding to virtual-dom library with few extensions."
  (@library-tutorial section)
  (@api-tutorial section))

(defsection @library-tutorial (:title "Tutorial")
  "Here is an implementation of simple TODO app.

   Single todo item:

```lisp
(defcomponent *list-item ()
  (defun initialize (data)
    (setf% data data))
  
  (defun render ()
    (who (:li (@ this data title)))))
```

   List:

```lisp
(defcomponent *list ()
  (defun initialize (items)
    (setf% items items))
  
  (defun create-item (data)
    (-> (new (*list-item data)) (render)))

  (defun render ()
    (let ((items (mapcar (@ this create-item)
                         (@ this items))))
      (who (:ul items)))))
```

   Let's render the app into DOM

```lisp
(defvar list (new (*list (array (create :title \"Item 1\")
                                (create :title \"Item 1\")))))
(defvar el (-> document (get-elements-by-tag-name \"body\")))

(render-component list (aref el 0))
```
")

(defsection @api-tutorial (:title "API")
  (defcomponent psmacro)
  (render-component psmacro)
  (who psmacro))

(defpsmacro defcomponent (name super-classes &rest body)
  "Define new virtual-dom component.

   Each component is transformed into DEFJSCLASS from from PLUS.PAREN
   package. Which means all BODY definitions supported by DEFJSCLASS
   form are also supported by DEFCOMPONENT."
  (let ((sc (or (and super-classes (append (list *vdom-superclass-name*) super-classes))
                (list *vdom-superclass-name*))))
    `(defjsclass ,name (,@sc)
       ,@body)))

(defpsmacro render-component (name dom)
  "Render component into dom node"
  `(chain ,*vdom-dom* (render ,name ,dom)))

(defpsmacro who (&rest body)
  "Transform cl-who like forms into vdom nodes."
  (when (and (listp body) (listp (car body)))
    (tree-to-vdom (car body))))
