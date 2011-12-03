;; Copyright 2011 Ravi Desai
;; Distributed under the GNU General Public License version 3 or later.

;;;; This is to define the package that I have all my code a part off.

(defpackage :pleasance
  (:use :cl :cl-ppcre)
  (:export :score :create-scores :*array* :atrb :mso :adx :momentum :roc :+5close-diff :nn :ga :refresh-data))
