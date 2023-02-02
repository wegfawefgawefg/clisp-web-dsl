(defvar *valid-tags-and-attributes*
        (let ((table (make-hash-table)))
          (setf (gethash :h1 table) '(:class :id :style))
          (setf (gethash :p table) '(:class :id :style))
          table))

(defun validate-attributes (tag attributes)
  (let ((valid-attributes (gethash tag *valid-tags-and-attributes*)))
    (if valid-attributes
        (loop for (attribute value) on attributes by #'cddr
                unless (member attribute valid-attributes)
              do (return nil))
        t)))

(defun validate-tag (tag)
  (gethash tag *valid-tags-and-attributes*))

(defun evaluate (form)
  (if (listp form)
      (eval form)
      form))

(defmacro html (&body body)
  (let ((result '()))
    (dolist (form body)
      (let ((tag (evaluate (first form)))
            (content (evaluate (second form)))
            (attributes (evaluate (third form))))
        (when (validate-tag tag)
              (when (validate-attributes tag attributes)
                    (push (format "<~a~{ ~a='~a'~}>~a</~a>" tag
                            (if attributes
                                (loop for (attr value) on attributes by #'cddr
                                      collect attr collect value))
                            content tag)
                          result)))))
    (apply #'concatenate 'string (nreverse result))))

(html
  (h1 "Hello, world!" :class "title" :id "main-title")
  (p "This is a paragraph." :class "text" :id "main-text"))

(html
  (loop for i from 1 to 10
        do (html :p (format "~d" i))))
