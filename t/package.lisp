(defpackage :rsss
  (:use :cl)
  (:export 

    ;; PUBLIC FUNCTIONS
    :feed-value
    :feed-values
    :feed-value-listless

    :feed-items

    :title
    :description
    :pubdate
    :link


    ;; PRIVATE FUNCTIONS
    :getf-string
    :getf-strings))
    

(in-package :rsss)
