;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.

;; See the GNU General Public License for more details.

;; -----------------

(defpackage :rsss
  (:use :cl)
  (:export 
    :parse
    :entries :name :uri :date :desc :author :text :media))

(in-package :rsss)


;; —————————————————————————————————————
;; CLASSES
;; —————————————————————————————————————
(defclass feed ()
  ((uri     :initarg :uri     :accessor uri     :initform  nil)
   (name    :initarg :name    :accessor name    :initform  nil)
   (date    :initarg :date    :accessor date    :initform  nil)
   (desc    :initarg :desc    :accessor desc    :initform  nil)
   (entries :initarg :entries :accessor entries :initform  nil)))

(defclass entry ()
  ((uri     :initarg :uri     :accessor uri     :initform  nil)
   (name    :initarg :name    :accessor name    :initform  nil)
   (date    :initarg :date    :accessor date    :initform  nil)
   (author  :initarg :author  :accessor author  :initform  nil)
   (desc    :initarg :desc    :accessor desc    :initform  nil)
   (media   :initarg :media   :accessor media   :initform  nil)
   (text    :initarg :text    :accessor text    :initform  nil)))



;; —————————————————————————————————————
;; MISC
;; —————————————————————————————————————
(defmacro append-or-replace (list item)
  "If a list is empty (nil), then replace it with a new list containing item.
  Otherwise, append item to the pre-existing list.
  Side-effectively, with nconc et. al."
  `(if (nilp ,list)
    (setf ,list  (list ,item))
    (nconc ,list (list ,item))))

;; VARYING LIST → LIST
(defun equ-assoc (item list)
  "Run #'assoc, but with #'equal as the test function."
  (assoc item list :test #'equal))

;; VARYING → BOOLEAN
(defun nilp (item)
  "Return whether or note an item is eq to NIL."
  (eq nil item))

;; LIST → VARYING
(defun ie-car (item)
  "Try car'ing something… but don't sweat it if, y'know, it fucks."
  (ignore-errors (car item)))



;; —————————————————————————————————————
;; PARSING
;; —————————————————————————————————————
;; STRING → RSSS:FEED
(defun parse (xml)
  "Parse a given XML string (atom/rss[12]) into a FEED object."
  (let* ((node (xmls:parse xml))
	 (type (feed-type node)))
    (cond ((or (eq type :rss2) (eq type :rss1))
	   (parse-rss node))
	  ((eq type :atom)
	   (parse-atom node)))))

;; -----------------

(defmacro common-let (node extra-let form &optional extra-form)
  "A let-statement used by basically every parsing-function/macro."
  `(let ((name    (xmls:node-name     ,node))
	 (chchild (xmls:node-children ,node))
	 (attrs   (xmls:node-attrs    ,node))
	 ,@extra-let)
     ,form))



;; —————————————————
;; ATOM PARSING
;; —————————————————
(defmacro parse-atom-children (rsss parent-node child-node extra-cond)
  "Code common to parsing both overarching Atom XML and individual entries."
   `(mapcar
      (lambda (,child-node)
	(common-let ,child-node nil
	  (cond ((equal "link"  name)
		 (setf (uri ,rsss)       (cadr (equ-assoc "href" attrs))))
		((equal "title" name)
		 (setf (name ,rsss)      (car chchild)))
		((equal "updated" name)
		 (setf (date ,rsss)      (car chchild)))
		((equal "summary" name)
		 (setf (desc ,rsss)      (car chchild)))
		,extra-cond)))
;;	  nil))
      (xmls:node-children ,parent-node)))

;; -----------------

;; XMLS:NODE → RSSS:FEED
(defun parse-atom (atom-node)
  "Parse Atom XMLS node into an rsss FEED object."
  (let ((feed (make-instance 'feed)))
    (parse-atom-children
      feed atom-node atom-child
      ((equal "entry" name)
       (append-or-replace (entries feed) (parse-atom-entry atom-child))))
    feed))

;; XMLS:NODE → RSSS:ENTRY
(defun parse-atom-entry (entry-node)
  "Parse an Atom <entry>'s XMLS:NODE into an RSSS:ENTRY object."
  (let ((entry (make-instance 'entry)))
    (parse-atom-children
      entry entry-node entry-child
      ((equal "content" name)
       (setf (text entry) (car chchild))))
    entry))



;; —————————————————
;; RSS1/RSS2 PARSING
;; —————————————————
(defmacro parse-rss-children (rsss parent-node child-node
				    &optional (cond-1 `(T nil))
					      (cond-2 `(T nil))
					      (cond-3 `(T nil))
					      (cond-4 `(T nil)))
  "Some code common to parsing the children of rss nodes."
  `(mapcar
     (lambda (,child-node)
       (common-let ,child-node nil
         (cond ((equal "title" name)
		(setf (name ,rsss) (ie-car chchild)))
	       ((equal "pubDate" name)
		(setf (date ,rsss) (ie-car chchild)))
	       ((equal "date" name)
		(setf (date ,rsss) (ie-car chchild)))
	       ((equal "link" name)
		(setf (uri ,rsss)  (ie-car chchild)))
	       ,cond-1 ,cond-2 ,cond-3 ,cond-4)))
     (xmls:node-children ,parent-node)))

;; -----------------

;; XMLS:NODE → RSSS:FEED
(defun parse-rss (rss-node)
  "Parse an RSS XMLS node into an rsss:FEED object."
  (let ((feed (make-instance 'feed)))
    (mapcar 
      (lambda (rss-child)
	(let ((name (xmls:node-name rss-child)))
	  (cond ((equal "channel" name)
		 (parse-rss-channel feed rss-child))
		((equal "item"    name)
		 (append-or-replace
		   (entries feed) (parse-rss-item rss-child))))))
      (xmls:node-children rss-node))
    feed))

;; RSSS:FEED XMLS:NODE → NIL
(defun parse-rss-channel (feed channel-node)
  "Parse a channel node of an RSS feed; modifies the FEED object."
  (parse-rss-children
    feed channel-node channel-child
    ((equal "description" name)
     (setf (desc feed) (ie-car chchild)))
    ((equal "item" name)
     (append-or-replace (entries feed) (parse-rss-item channel-child))))
  feed)

;; XMLS:NODE → RSSS:ENTRY
(defun parse-rss-item (entry-node)
  "Parse an item (XMLS:NODE) of an RSS feed."
  (let ((entry (make-instance 'entry)))
    (parse-rss-children
      entry entry-node entry-child
      ((or (equal "content" name) (equal "encoded" name))
       (setf (text entry)   (ie-car chchild)))
      ;; about the following: people use <description> tags for both summaries
      ;; and for actual post-bodies. (wtf :/)
      ;; so, if the text is longer than 250 characters, it's *probably* not
      ;; a summary, but an actual post. then again, not all posts are *that*
      ;; long…
      ;; this is a hack that won't always be helpful or effective, it's the
      ;; best trade-off I could think of. sorry ♥
      ((equal "description" name)
       (if (and (< 250 (length (ie-car chchild))) (not (text entry)))
	 (setf (text entry) (ie-car chchild))
	 (setf (desc entry) (ie-car chchild))))
      ((equal "enclosure" name)
       (setf (media entry)  (cadr (assoc "url" attrs :test #'equal))))
      ((or (equal "author" name) (equal "creator" name))
       (setf (author entry) (ie-car chchild))))
    entry))



;; —————————————————————————————————————
;; PRE-PARSING
;; —————————————————————————————————————
;; STRING → SYMBOL
(defun feed-type (node)
  "Return the type of the feed-- :rss2 for RSS 2.0, :rss1 for RSS 1.0/other,
  and :atom for (obviously!) Atom."
  (let ((name    (xmls:node-name node))
	(attrs   (xmls:node-attrs node)))
    (cond ((and (equal "rss" name)
		(equal "2.0" (cadr (assoc "version" attrs :test #'equal))))
	   :rss2)
	  ((equal "rss" name)
	   :rss1)
	  ((equal "feed" name)
	   :atom))))
