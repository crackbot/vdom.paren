
(asdf:defsystem :vdom.paren
  :name "vdom.paren"
  :description "Bindings for virtual-dom with several extensions."
  :version "0.0.1"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "vdom.paren.asd")
               (:file "package")
               (:file "runtime")
               (:file "vdom")
               (:file "api")

               (:module "bower-components"
                        :components ((:javascript-file "bower_components/virtual-dom/dist/virtual-dom.js"))))
  
  :depends-on (:parenscript :contracts.paren :serve.paren :plus.paren
                            :mgl-pax :mgl-pax-ext :lisp-unit))
