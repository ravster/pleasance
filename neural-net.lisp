;;;; The goal is to make a net that can predict when a big move is about to happen (As defined by a higher-than-normal TR).

(defparameter weight-00 nil
  "From atr-node to first hidden node.")
(defparameter weight-01 nil
  "From atr to second hidden node.")
(defparameter weight-10 nil
  "From ma-diff-close to first hidden node.")
(defparameter weight-11 nil
  "From ma-diff-close to second hidden node.")

(defparameter h-weight-0 nil
  "From hidden node 1 to output node.")
(defparameter h-weight-1 nil
  "From hidden node 2 to output node.")