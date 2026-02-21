(defparameter *void-tags* '(:br :hr :img))

(defun plist-even-p (attributes)
    (evenp (length attributes)))

(defun renderable-tag-p (tag)
    (or (symbolp tag)
        (stringp tag)))

(defun renderable-attribute-name-p (attribute)
    (or (symbolp attribute)
        (stringp attribute)))

(defun validate-attributes (tag attributes)
    (declare (ignore tag))
    (and (plist-even-p attributes)
         (loop for (attribute value) on attributes by #'cddr
               always (renderable-attribute-name-p attribute))))

(defun validate-tag (tag)
    (renderable-tag-p tag))

(defun html-name (symbol)
    (string-downcase (string symbol)))

(defun join-strings (parts separator)
    (if (null parts)
        ""
        (reduce (lambda (a b) (concatenate 'string a separator b)) parts)))

(defun raw (value)
    (list :raw (princ-to-string (or value ""))))

(defun raw-content-p (value)
    (and (listp value)
         (= (length value) 2)
         (eq (first value) :raw)))

(defun raw-content-value (value)
    (second value))

(defun tag-name->keyword (name)
    (intern (string-upcase name) "KEYWORD"))

(defun whitespace-char-p (ch)
    (or (char= ch #\Space)
        (char= ch #\Tab)
        (char= ch #\Newline)
        (char= ch #\Return)))

(defun split-on-char (text delimiter)
    (let ((parts '())
          (start 0)
          (len (length text)))
        (loop for i from 0 below len
              do (when (char= (char text i) delimiter)
                     (push (subseq text start i) parts)
                     (setf start (1+ i))))
        (push (subseq text start len) parts)
        (nreverse parts)))

(defun insert-tag-newlines (html-text)
    (with-output-to-string (out)
        (loop for i from 0 below (length html-text)
              for ch = (char html-text i)
              do (write-char ch out)
                 (when (and (char= ch #\>)
                            (< (1+ i) (length html-text))
                            (char= (char html-text (1+ i)) #\<))
                     (write-char #\Newline out)))))

(defun extract-tag-name (line)
    (let* ((len (length line))
           (start (if (and (> len 1) (char= (char line 1) #\/)) 2 1))
           (end start))
        (loop while (< end len)
              for ch = (char line end)
              while (and (not (char= ch #\>))
                         (not (char= ch #\/))
                         (not (whitespace-char-p ch)))
              do (incf end))
        (if (> end start)
            (string-downcase (subseq line start end))
            "")))

(defun opening-tag-line-p (line)
    (and (> (length line) 2)
         (char= (char line 0) #\<)
         (not (char= (char line 1) #\/))
         (not (char= (char line 1) #\!))
         (not (char= (char line 1) #\?))))

(defun closing-tag-line-p (line)
    (and (> (length line) 3)
         (char= (char line 0) #\<)
         (char= (char line 1) #\/)))

(defun self-closing-tag-line-p (line)
    (let ((len (length line)))
        (and (> len 3)
             (char= (char line (- len 2)) #\/)
             (char= (char line (1- len)) #\>))))

(defun inline-complete-element-line-p (line)
    (and (opening-tag-line-p line)
         (not (self-closing-tag-line-p line))
         (search "</" line)))

(defun void-tag-line-p (line)
    (let ((tag-name (extract-tag-name line)))
        (member (tag-name->keyword tag-name) *void-tags*)))

(defun indent-string (level indent-size)
    (make-string (* level indent-size) :initial-element #\Space))

(defun pretty-html (html-text &key (indent-size 4))
    (let ((depth 0)
          (lines (split-on-char (insert-tag-newlines html-text) #\Newline)))
        (with-output-to-string (out)
            (loop for raw-line in lines
                  for line = (string-trim '(#\Space #\Tab #\Newline #\Return) raw-line)
                  unless (string= line "")
                  do
                     (cond
                         ((closing-tag-line-p line)
                          (setf depth (max 0 (1- depth)))
                          (format out "~a~a~%" (indent-string depth indent-size) line))
                         ((opening-tag-line-p line)
                          (format out "~a~a~%" (indent-string depth indent-size) line)
                          (unless (or (inline-complete-element-line-p line)
                                      (self-closing-tag-line-p line)
                                      (void-tag-line-p line))
                              (incf depth)))
                         (t
                          (format out "~a~a~%" (indent-string depth indent-size) line)))))))

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

(defun render-content-part (value)
    (cond
        ((raw-content-p value) (raw-content-value value))
        (value (escape-html value))
        (t "")))

(defun normalize-attribute-value (attribute value)
    (cond
        ((null value) nil)
        ((eq value t) t)
        ((and (member attribute '(:class "class") :test #'equal) (listp value))
         (join-strings (mapcar #'princ-to-string value) " "))
        (t value)))

(defun attributes->string (attributes)
    (with-output-to-string (out)
        (loop for (attr value) on attributes by #'cddr
              for normalized = (normalize-attribute-value attr value)
              do (cond
                     ((null normalized) nil)
                     ((eq normalized t)
                      (format out " ~a" (html-name attr)))
                     (t
                      (format out " ~a='~a'" (html-name attr) (escape-html normalized)))))))

(defun css-name (x)
    (substitute #\- #\_ (string-downcase (string x))))

(defun css-value (x)
    (princ-to-string x))

(defun css-declarations (plist)
    (unless (plist-even-p plist)
        (error "Invalid CSS declaration plist: ~s" plist))
    (with-output-to-string (out)
        (loop for (prop value) on plist by #'cddr
              unless (null value)
              do (format out "~a: ~a; " (css-name prop) (css-value value)))))

(defun css-rule (selector declarations)
    (format nil "~a { ~a}" selector (css-declarations declarations)))

(defun css (&rest rules)
    (format nil "~{~a~%~}" rules))

(defun render-node (tag content attributes &optional raw-content-p)
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
                        (if raw-content-p
                            (or content "")
                            (render-content-part content))
                        tag-name)))))

(defmacro html (&body body)
    (labels ((node-form-p (x)
               (and (listp x)
                    (symbolp (first x))
                    (keywordp (first x))))
             (quoted-plist-p (x)
               (and (listp x)
                    (= (length x) 2)
                    (eq (first x) 'quote)
                    (listp (second x))))
             (compile-content-parts (parts)
               (if (null parts)
                   nil
                   `(apply #'concatenate
                           'string
                           (list
                            ,@(loop for part in parts
                                    collect
                                    (if (node-form-p part)
                                        (compile-node part)
                                        `(render-content-part ,part)))))))
             (compile-node (form)
               (let* ((tag (first form))
                      (parts (rest form))
                      (part-count (length parts)))
                 (cond
                     ((= part-count 0)
                      `(render-node ,tag nil nil))
                     ((= part-count 1)
                      `(render-node ,tag ,(first parts) nil))
                     ((= part-count 2)
                      (if (quoted-plist-p (second parts))
                          `(render-node ,tag ,(first parts) ,(second parts))
                          `(render-node ,tag ,(compile-content-parts parts) nil t)))
                     (t
                      (let* ((last-part (car (last parts)))
                             (has-attrs (quoted-plist-p last-part))
                             (attrs (if has-attrs last-part nil))
                             (content-parts (if has-attrs (butlast parts) parts)))
                        `(render-node ,tag ,(compile-content-parts content-parts) ,attrs t)))))))
      `(apply #'concatenate
              'string
              (list
               ,@(loop for form in body
                       collect (compile-node form))))))

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
