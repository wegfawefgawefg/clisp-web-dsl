;; Run with: sbcl --script src/demo.lisp
(load (merge-pathnames "dsl.lisp" *load-truename*))

(defun render-big-bang-demo ()
    (let ((intro
           (html
               (:h1 "clisp-web-dsl Big Bang Demo" '(:id "top" :class "hero"))
               (:h2 "Minimal HTML subset, one page")
               (:div "Container block sample." '(:class "container"))
               (:p "This output is generated from Lisp forms." '(:class "lead"))
               (:p "Escaping check: 5 < 7 & \"quoted\" text." '(:title "x < y & z"))))
          (text-block
           (html
               (:h3 "Text Semantics")
               (:p "Strong label:" nil)
               (:strong "IMPORTANT")
               (:span " + ")
               (:em "emphasized")
               (:span " + ")
               (:code "(+ 1 2)")))
          (links-and-lists
           (html
               (:h3 "Links and Lists")
               (:a "Visit example.com" '(:href "https://example.com" :target "_blank" :rel "noopener"))
               (:br)
               (:ul "First item" '(:class "items"))
               (:li "Second item")
               (:li "Third item")
               (:ol "Step one")
               (:li "Step two")))
          (media
           (html
               (:h3 "Void Elements")
               (:hr nil '(:class "divider"))
               (:img nil '(:src "/static/demo.png" :alt "demo image" :width "320" :height "180"))
               (:br)
               (:p "End of demo."))))
      (concatenate 'string intro text-block links-and-lists media)))

(format t "~a~%" (render-big-bang-demo))
