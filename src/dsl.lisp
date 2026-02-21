(defparameter *global-attributes* '(:class :id :style :title))

(defvar *valid-tags-and-attributes*
    (let ((table (make-hash-table)))
        (labels ((register-tag (tag &optional specific-attributes)
                   (setf (gethash tag table)
                         (append specific-attributes *global-attributes*))))
            (register-tag :h1)
            (register-tag :h2)
            (register-tag :h3)
            (register-tag :p)
            (register-tag :div)
            (register-tag :span)
            (register-tag :strong)
            (register-tag :em)
            (register-tag :code)
            (register-tag :a '(:href :target :rel))
            (register-tag :ul)
            (register-tag :ol)
            (register-tag :li)
            (register-tag :br)
            (register-tag :hr)
            (register-tag :img '(:src :alt :width :height)))
        table))

(defparameter *void-tags* '(:br :hr :img))

(defun plist-even-p (attributes)
    (evenp (length attributes)))

(defun validate-attributes (tag attributes)
    (let ((valid-attributes (gethash tag *valid-tags-and-attributes*)))
        (and valid-attributes
             (plist-even-p attributes)
             (loop for (attribute value) on attributes by #'cddr
                   always (member attribute valid-attributes)))))

(defun validate-tag (tag)
    (gethash tag *valid-tags-and-attributes*))

(defun html-name (symbol)
    (string-downcase (string symbol)))

(defun escape-html (value)
    (let ((text (princ-to-string (or value ""))))
        (with-output-to-string (out)
            (loop for ch across text
                  do (case ch
                         (#\& (write-string "&amp;" out))
                         (#\< (write-string "&lt;" out))
                         (#\> (write-string "&gt;" out))
                         (#\" (write-string "&quot;" out))
                         (#\' (write-string "&#39;" out))
                         (otherwise (write-char ch out)))))))

(defun attributes->string (attributes)
    (with-output-to-string (out)
        (loop for (attr value) on attributes by #'cddr
              do (format out " ~a='~a'" (html-name attr) (escape-html value)))))

(defun render-node (tag content attributes)
    (when (and (validate-tag tag)
               (validate-attributes tag attributes))
        (let ((tag-name (html-name tag)))
            (if (member tag *void-tags*)
                (format nil
                        "<~a~a>"
                        tag-name
                        (attributes->string attributes))
                (format nil
                        "<~a~a>~a</~a>"
                        tag-name
                        (attributes->string attributes)
                        (escape-html content)
                        tag-name)))))

(defmacro html (&body body)
    `(apply #'concatenate
            'string
            (remove nil
                    (list
                     ,@(loop for form in body
                             collect
                             (let ((tag (first form))
                                   (parts (rest form)))
                               (cond
                                   ((= (length parts) 0) `(render-node ,tag nil nil))
                                   ((= (length parts) 1) `(render-node ,tag ,(first parts) nil))
                                   ((= (length parts) 2) `(render-node ,tag ,(first parts) ,(second parts)))
                                   (t (error "Invalid node form: ~s" form)))))))))

(defun render-demo-page ()
    (html
        (:h1 "Hello, world!" '(:class "title" :id "main-title"))
        (:p "This is a paragraph." '(:class "text" :id "main-text"))))

(defun render-numbered-paragraphs ()
    (apply #'concatenate
           'string
           (loop for i from 1 to 10
                 collect (html (:p (format nil "~d" i) '(:class "counter"))))))

(defun main ()
    (format t "~a~%~%~a~%"
            (render-demo-page)
            (render-numbered-paragraphs)))
