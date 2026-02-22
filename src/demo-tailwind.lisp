;; Run with: sbcl --script src/demo-tailwind.lisp
(load (merge-pathnames "dsl.lisp" *load-truename*))

(defun shell ()
    (html
        (:script "" '(:src "https://cdn.tailwindcss.com"))
        (:div
            (:div
                (:p "clisp-web-dsl x Tailwind" '(:class "text-xs font-semibold uppercase tracking-[0.18em] text-cyan-300"))
                (:h1 "Tailwind utility classes rendered from Lisp forms"
                     '(:class "mt-3 text-3xl font-black leading-tight text-white sm:text-5xl"))
                (:p "Same permissive HTML DSL, now with utility-first styling."
                    '(:class "mt-4 max-w-2xl text-sm text-slate-300 sm:text-base"))
                (:div
                    (:a "Read docs"
                        '(:href "/docs"
                          :class "inline-flex items-center rounded-xl bg-cyan-300 px-4 py-2 text-sm font-semibold text-slate-950 transition hover:bg-cyan-200"))
                    (:span " ")
                    (:a "Open pricing"
                        '(:href "/pricing"
                          :class "inline-flex items-center rounded-xl border border-slate-600 px-4 py-2 text-sm font-semibold text-slate-100 transition hover:border-slate-400 hover:bg-slate-800"))
                    '(:class "mt-6 flex flex-wrap gap-3"))
                '(:class "rounded-3xl border border-slate-700/60 bg-slate-900/70 p-8 shadow-2xl shadow-slate-950/60 backdrop-blur"))
                (:div
                    (:div
                        (:p "57ms" '(:class "text-2xl font-black text-cyan-300"))
                        (:p "Search latency" '(:class "mt-1 text-xs uppercase tracking-wide text-slate-400"))
                        '(:class "rounded-2xl border border-slate-700/70 bg-slate-900/80 p-5"))
                    (:div
                        (:p "99.9%" '(:class "text-2xl font-black text-emerald-300"))
                        (:p "Uptime target" '(:class "mt-1 text-xs uppercase tracking-wide text-slate-400"))
                        '(:class "rounded-2xl border border-slate-700/70 bg-slate-900/80 p-5"))
                    (:div
                        (:p "0 setup" '(:class "text-2xl font-black text-fuchsia-300"))
                        (:p "Pure HTML output" '(:class "mt-1 text-xs uppercase tracking-wide text-slate-400"))
                        '(:class "rounded-2xl border border-slate-700/70 bg-slate-900/80 p-5"))
                    '(:class "mt-6 grid gap-4 sm:grid-cols-3"))
                (:p "Built with SBCL and rendered via `html` macro."
                    '(:class "mt-6 text-xs text-slate-400"))
                '(:class "mx-auto max-w-5xl px-4 py-10 sm:px-6 lg:px-8 min-h-screen bg-gradient-to-b from-slate-950 via-slate-900 to-slate-800"))))

(format t "~a~%" (pretty-html (shell)))
