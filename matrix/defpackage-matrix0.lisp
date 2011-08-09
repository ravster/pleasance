;;;; Define the package for the YuWangLai matrix BPNN implementation

(defpackage :ravi.matrix0
  (:use :cl :cl-user :gsll :ravi.nn0)
  (:shadow :training-set :validation-set :test-set :create-set :create-scores))
