(defpackage :rsss-testing
  (:use :cl)
  (:export :do-all))

(in-package :rsss-testing)

;; NIL → VARYING
(defun do-all ()
  "Execute all tests."
  (rt:do-tests))

;; -----------------

;; To run these tests, just load up :rsss-testing and run #'do-all.
;; Senproblem'!

;; -----------------

(defvar *atom-xml*   (alexandria:read-file-into-string #p"t/atom.xml"))
(defvar *atom-node*  (xmls:parse *atom-xml*))
(defvar *atom-entry* (eighth (xmls:node-children *atom-node*)))
(defvar *rss1-xml*   (alexandria:read-file-into-string #p"t/rss1.xml"))
(defvar *rss1-node*  (xmls:parse *rss1-xml*))
(defvar *rss1-entry* (second (xmls:node-children *rss1-node*)))
(defvar *rss2-xml*   (alexandria:read-file-into-string #p"t/rss2.xml"))
(defvar *rss2-node*  (xmls:parse *rss2-xml*))
(defvar *rss2-entry* (sixth (xmls:node-children
			    (car (xmls:node-children *rss2-node*)))))

;; —————————————————————————————————————
;; MISC
;; —————————————————————————————————————
(rt:deftest append-or-replace-i
	    (let ((test-list nil))
	      (rsss::append-or-replace test-list 1))
	    (1))
(rt:deftest append-or-replace-ii
	    (let ((test-list '("never again!" 2312 "well, maybe…")))
	      (rsss::append-or-replace test-list "again. MAYBE."))
	    ("never again!" 2312 "well, maybe…" "again. MAYBE."))

(rt:deftest equ-assoc-i
	    (let ((test-assoc '(("mami" 2) ("madoka" 3) ("homura" 4))))
	      (rsss::equ-assoc "madoka" test-assoc))
	    ("madoka" 3))
(rt:deftest equ-assoc-ii
	    (let ((test-assoc '(("homura" "madoka"))))
	      (rsss::equ-assoc "homura" test-assoc))
	    ("homura" "madoka"))

(rt:deftest nilp-i
	    (rsss::nilp nil)
	    T)
(rt:deftest nilp-ii
	    (rsss::nilp 1)
	    nil)

(rt:deftest ie-car-i
	    (multiple-value-bind (result error)
	      (rsss::ie-car 1)
	      (list result (not (rsss::nilp error))))
	    (nil T))
(rt:deftest ie-car-ii
	    (multiple-value-bind (result error)
	      (rsss::ie-car '((1 2) 2 3))
	      (list result (not (rsss::nilp error))))
	    ((1 2) nil))


;; —————————————————————————————————————
;; PARSING
;; —————————————————————————————————————
;; ATOM PARSING
;; —————————————————
(rt:deftest parse-atom-i
	    (rsss:name (rsss::parse-atom *atom-node*))
	    "Planet GNU")
(rt:deftest parse-atom-ii
	    (rsss:date (rsss::parse-atom *atom-node*))
	    "2019-07-09T17:55:32+00:00")
(rt:deftest parse-atom-iii
	    (rsss:uri (rsss::parse-atom *atom-node*))
	    "https://planet.gnu.org/")
(rt:deftest parse-atom-iv
	    (length (rsss:entries (rsss::parse-atom *atom-node*)))
	    60)
(rt:deftest parse-atom-v
	    (rsss:name (third (rsss:entries (rsss::parse-atom *atom-node*))))
	    "DW5821e firmware update integration in ModemManager and fwupd")

(rt:deftest parse-atom-entry-i
	    (length (rsss:text (rsss::parse-atom-entry *atom-entry*)))
	    7923)
(rt:deftest parse-atom-entry-ii
	    (rsss:author (rsss::parse-atom-entry *atom-entry*))
	    "Christopher Lemmer Webber")
(rt:deftest parse-atom-entry-iii
	    (rsss:date (rsss::parse-atom-entry *atom-entry*))
	    "2019-07-09T14:27:00+00:00")
(rt:deftest parse-atom-entry-iv
	    (rsss:uri (rsss::parse-atom-entry *atom-entry*))
	    "http://dustycloud.org/blog/racket-is-an-acceptable-python/")

;; —————————————————
;; RSS1 PARSING
;; —————————————————
(rt:deftest parse-rss1-i
	    (rsss:name (rsss::parse-rss *rss1-node*))
	    "Planet GNU")
(rt:deftest parse-rss1-ii
	    (rsss:date (rsss::parse-rss *rss1-node*))
	    nil)
(rt:deftest parse-rss1-iii
	    (rsss:uri (rsss::parse-rss *rss1-node*))
	    "https://planet.gnu.org/")
(rt:deftest parse-rss1-iv
	    (length (rsss:entries (rsss::parse-rss *rss1-node*)))
	    60)
(rt:deftest parse-rss1-v
	    (rsss:name (third (rsss:entries (rsss::parse-rss *rss1-node*))))
	    "FSF Blogs: Thank you for advancing free software: Read FSF spring news in the latest Bulletin")

(rt:deftest parse-rss1-entry-i
	    (length (rsss:text (rsss::parse-rss-item *rss1-entry*)))
	    14616)
(rt:deftest parse-rss1-entry-ii
	    (rsss:author (rsss::parse-rss-item *rss1-entry*))
	    "FSF Blogs")
(rt:deftest parse-rss1-entry-iii
	    (rsss:date (rsss::parse-rss-item *rss1-entry*))
	    "2019-07-09T15:39:21+00:00")
(rt:deftest parse-rss1-entry-iv
	    (rsss:uri (rsss::parse-rss-item *rss1-entry*))
	    "http://www.fsf.org/blogs/rms/photo-blog-2019-june-brno")

;; —————————————————
;; RSS2 PARSING
;; —————————————————
(rt:deftest parse-rss2-i
	    (rsss:name (rsss::parse-rss *rss2-node*))
	    "Esperanto-USA member blogs")
(rt:deftest parse-rss2-ii
	    (rsss:date (rsss::parse-rss *rss2-node*))
	    "2019-07-09T00:00:39-07:00")
(rt:deftest parse-rss2-iii
	    (rsss:uri (rsss::parse-rss *rss2-node*))
	    "http://esperanto-usa.org/eusa/blogs/member-blogs.rss")
(rt:deftest parse-rss2-iv
	    (length (rsss:entries (rsss::parse-rss *rss2-node*)))
	    21)
(rt:deftest parse-rss2-v
	    (rsss:name (third (rsss:entries (rsss::parse-rss *rss2-node*))))
	    "Maja bulteno")

(rt:deftest parse-rss2-entry-i
	    (length (rsss:text (rsss::parse-rss-item *rss2-entry*)))
	    0)
(rt:deftest parse-rss2-entry-ii
	    (rsss:author (rsss::parse-rss-item *rss2-entry*))
	    nil)
(rt:deftest parse-rss2-entry-iii
	    (rsss:date (rsss::parse-rss-item *rss2-entry*))
	    "2019-06-29T00:47:00+00:00")
(rt:deftest parse-rss2-entry-iv
	    (rsss:uri (rsss::parse-rss-item *rss2-entry*))
	    "http://eo1a.blogspot.com/2019/06/cirkau-la-mondon-post-okdek-tagoj-26.html")

;; —————————————————
;; GENERAL PARSING
;; —————————————————
(rt:deftest parse-i
	    (type-of (rsss:parse *atom-xml*))
	    rsss::feed)
(rt:deftest parse-ii
	    (type-of (rsss:parse *rss1-xml*))
	    rsss::feed)
(rt:deftest parse-iii
	    (type-of (rsss:parse *rss2-xml*))
	    rsss::feed)
