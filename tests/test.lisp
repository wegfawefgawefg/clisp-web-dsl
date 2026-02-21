;; Run with: sbcl --script tests/test.lisp
(load (merge-pathnames "../src/dsl.lisp" *load-truename*))

(defparameter *failed* 0)

(defun assert-equal (name expected actual)
    (if (equal expected actual)
        (format t "PASS: ~a~%" name)
        (progn
            (incf *failed*)
            (format t "FAIL: ~a~%  expected: ~s~%  actual:   ~s~%" name expected actual))))

(assert-equal
    "render-demo-page"
    "<h1 class='title' id='main-title'>Hello, world!</h1><p class='text' id='main-text'>This is a paragraph.</p>"
    (render-demo-page))

(assert-equal
    "invalid-attribute-is-rejected"
    ""
    (html (:h1 "Hello" '(:onclick "evil()"))))

(assert-equal
    "invalid-tag-is-rejected"
    ""
    (html (:table "Nope" '(:class "x"))))

(assert-equal
    "runtime-content-expression"
    "<p class='x'>42</p>"
    (let ((n 42))
        (html (:p (format nil "~d" n) '(:class "x")))))

(assert-equal
    "numbered-paragraphs"
    "<p class='counter'>1</p><p class='counter'>2</p><p class='counter'>3</p><p class='counter'>4</p><p class='counter'>5</p><p class='counter'>6</p><p class='counter'>7</p><p class='counter'>8</p><p class='counter'>9</p><p class='counter'>10</p>"
    (render-numbered-paragraphs))

(assert-equal
    "link-tag-with-link-attributes"
    "<a href='https://example.com' target='_blank' rel='noopener'>Go</a>"
    (html (:a "Go" '(:href "https://example.com" :target "_blank" :rel "noopener"))))

(assert-equal
    "void-tag-rendering"
    "<br><hr class='line'><img src='/x.png' alt='x'>"
    (html
        (:br)
        (:hr nil '(:class "line"))
        (:img nil '(:src "/x.png" :alt "x"))))

(assert-equal
    "escape-content-and-attributes"
    "<p title='&quot;q&quot; &amp;'>5 &lt; 7 &amp; 9 &gt; 2</p>"
    (html (:p "5 < 7 & 9 > 2" '(:title "\"q\" &"))))

(when (> *failed* 0)
    (format t "~%~d test(s) failed.~%" *failed*)
    (sb-ext:exit :code 1))

(format t "~%All tests passed.~%")
