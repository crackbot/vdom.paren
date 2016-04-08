<a id='x-28VDOM-2EPAREN-3A-40MAIN-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

# Main

## Table of Contents

- [1 Tutorial][05e2]
- [2 API][61af]

###### \[in package VDOM.PAREN\]
This is a binding to virtual-dom library with few extensions.

<a id='x-28VDOM-2EPAREN-3A-40LIBRARY-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 1 Tutorial

Here is an implementation of simple TODO app.

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

Let's render the app into `DOM`

```lisp
(defvar list (new (*list (array (create :title "Item 1")
                                (create :title "Item 1")))))
(defvar el (-> document (get-elements-by-tag-name "body")))

(render-component list (aref el 0))
```


<a id='x-28VDOM-2EPAREN-3A-40API-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 2 API

<a id='x-28VDOM-2EPAREN-3ADEFCOMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **DEFCOMPONENT** *NAME SUPER-CLASSES &REST BODY* 

    Define new virtual-dom component.

<a id='x-28VDOM-2EPAREN-3ARENDER-COMPONENT-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **RENDER-COMPONENT** *NAME DOM* 

    Render component inside dom node

<a id='x-28VDOM-2EPAREN-3AWHO-20-28MGL-PAX-EXT-3APSMACRO-29-29'></a>

- [psmacro] **WHO** *&REST BODY* 

    Transform cl-who like forms into vdom nodes

  [05e2]: #x-28VDOM-2EPAREN-3A-40LIBRARY-TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [61af]: #x-28VDOM-2EPAREN-3A-40API-TUTORIAL-20MGL-PAX-3ASECTION-29 "API"
