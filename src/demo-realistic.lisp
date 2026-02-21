;; Run with: sbcl --script src/demo-realistic.lisp
(load (merge-pathnames "dsl.lisp" *load-truename*))

(defun header ()
    (html
        (:div "Acme Notes" '(:id "brand" :class "brand"))
        (:a "Docs" '(:href "/docs"))
        (:span " | ")
        (:a "Pricing" '(:href "/pricing"))
        (:span " | ")
        (:a "Sign in" '(:href "/login"))))

(defun hero ()
    (html
        (:h1 "Write. Ship. Search." '(:id "hero-title"))
        (:p "A fast internal knowledge base for small teams." '(:class "subtitle"))
        (:a "Start free" '(:href "/signup" :class "cta"))
        (:span " ")
        (:a "See live demo" '(:href "/demo" :rel "noopener"))
        (:br)
        (:p "No credit card required." '(:class "fine-print"))))

(defun features ()
    (html
        (:h2 "Why teams switch")
        (:ul "Full-text search in milliseconds")
        (:li "Versioned notes with edit history")
        (:li "Simple role-based access")
        (:li "Public links when you need them")
        (:li "Works with plain Markdown")
        (:p "Tech stack: " nil)
        (:code "Common Lisp + minimal HTML DSL")))

(defun social-proof ()
    (html
        (:h3 "Trusted by early users")
        (:p "\"We replaced three tools with one.\"" '(:title "Customer quote"))
        (:strong "Sam, Eng Manager")
        (:br)
        (:em "Saved us 6+ hours/week in onboarding")))

(defun pricing ()
    (html
        (:h2 "Pricing")
        (:div "Starter — $9/user" '(:class "tier"))
        (:p "Includes search, history, and team spaces.")
        (:div "Pro — $19/user" '(:class "tier"))
        (:p "Adds SSO, audit logs, and advanced permissions.")))

(defun footer ()
    (html
        (:hr nil '(:class "divider"))
        (:p "Questions? Email support@acme-notes.test")
        (:img nil '(:src "/static/team-shot.png" :alt "Team photo" :width "560" :height "240"))))

(defun realistic-demo-page ()
    (apply #'concatenate
           'string
           (list
            (header)
            (hero)
            (features)
            (social-proof)
            (pricing)
            (footer))))

(format t "~a~%" (realistic-demo-page))
