
(defpackage :vdom.paren
  (:use :cl
        :parenscript :plus.paren :contracts.paren :serve.paren
        :mgl-pax :mgl-pax-ext)
  (:export :defcomponent
           :render-component
           :who))

(defpackage :vdom.paren-tests
  (:use :cl :vdom.paren :parenscript :lisp-unit))
