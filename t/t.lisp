(ql:quickload :uiop)
(ql:quickload :rt)
(ql:quickload :xmls)

(load "t/package.lisp")

(load "src/main.lisp")
(load "t/main.lisp")

(rsss-testing:do-all)
