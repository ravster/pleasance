;; Copyright 2011-2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

;;;; This is to define the package that I have all my code a part off.

(defpackage :pleasance
  (:use :cl :cl-ppcre)
  (:export :score :create-scores :*array* :atrb :mso :adx :momentum :roc :+5close-diff :nn :ga :refresh-data))
