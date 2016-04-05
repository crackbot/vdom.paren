
(in-package :vdom.paren)

(defparameter *vdom-superclass-name* '*vdom-class*)

(defparameter *runtime*
  `(progn
     (defjsclass ,*vdom-superclass-name* ()
       (defun _vdom_init (dom-el)
         (let* ((tree (-> this (render)))
                (root-node (-> virtual-dom (create tree))))
                
                ;(container (-> document (get-element-by-id dom-el))))
               
               (setf (@ this _vdom-tree) tree
                     (@ this _root-node) root-node)
                     
               (setf (@ dom-el inner-h-t-m-l) "")
               (-> dom-el (append-child root-node))))

       (defun _update ()
         (let* ((new-tree (-> this (render)))
                (patches (-> virtual-dom (diff (@ this _vdom-tree)
                                               new-tree)))
                (root-node (-> virtual-dom (patch (@ this _root-node)
                                                  patches))))
           (setf% _vdom-tree new-tree
                  _root-node root-node)))
         
       (defun update (dom-el)
         (if (and (@ this _vdom-tree)
                  (@ this _root-node))
             (-> this (_update))
             (-> this (_vdom_init dom-el)))))))
