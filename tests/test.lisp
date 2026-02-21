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
    "arbitrary-attribute-is-allowed"
    "<h1 onclick='evil()'>Hello</h1>"
    (html (:h1 "Hello" '(:onclick "evil()"))))

(assert-equal
    "arbitrary-tag-is-allowed"
    "<table class='x'>Nope</table>"
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

(assert-equal
    "class-list-joins-with-spaces"
    "<div class='card primary wide'>x</div>"
    (html (:div "x" '(:class ("card" "primary" "wide")))))

(assert-equal
    "boolean-and-nil-attributes"
    "<input disabled type='checkbox'></input>"
    (html (:input "" '(:disabled t :checked nil :type "checkbox"))))

(assert-equal
    "nested-list-items"
    "<ul><li>One</li><li>Two</li><li>Three</li></ul>"
    (html
        (:ul
            (:li "One")
            (:li "Two")
            (:li "Three"))))

(assert-equal
    "nested-with-parent-attributes"
    "<div class='card'><h3>Title</h3><p>Body</p></div>"
    (html
        (:div
            (:h3 "Title")
            (:p "Body")
            '(:class "card"))))

(assert-equal
    "pretty-html-basic"
    "<div>
    <p>Hi</p>
    <br>
</div>
"
    (pretty-html "<div><p>Hi</p><br></div>"))

(assert-equal
    "css-rule-basic"
    ".hero { font-size: 48px; line-height: 1.1; }"
    (css-rule ".hero" '(:font-size "48px" :line-height "1.1")))

(assert-equal
    "css-multi-rule"
    ".hero { font-size: 48px; }
.cta { background-color: #111; }
"
    (css
     (css-rule ".hero" '(:font-size "48px"))
     (css-rule ".cta" '(:background-color "#111"))))

(assert-equal
    "raw-unescaped-content"
    "<p><strong>trusted</strong></p>"
    (html (:p (raw "<strong>trusted</strong>"))))

(assert-equal
    "raw-mixed-with-escaped-content"
    "<div><em>x</em> &amp; y</div>"
    (html (:div (raw "<em>x</em>") " & y")))

(when (> *failed* 0)
    (format t "~%~d test(s) failed.~%" *failed*)
    (sb-ext:exit :code 1))

(format t "~%All tests passed.~%")
