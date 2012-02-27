;; Copyright 2011-2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

;;;; This file does the simulation of trading and prints out the results.

(in-package :pleasance)

(defun position-size (dollar-amount-willing-to-be-risked number-of-pips-risked)
  "Return the position-size that must be taken, given the dollar amount we are willing to risk and the dollar amount that would have been risked with one full contract."
  (/ dollar-amount-willing-to-be-risked ;1 pip for 1 lot = $10.  4 decimal points in the data, not 5.
     (* number-of-pips-risked 100000)))	;Evaluates the the dollar-amount being risked, on GBPUSD & EURUSD pairs.

(defmacro close-trade ()
  "This macro inserts the code that is required to simulate the closing of a trade."
  '(progn
    (if (plusp change)
	(progn
	  (incf num-win)
	  (incf amount-win change))
	(progn
	  (incf num-lose)
	  (incf amount-lose change)))

    ;;Over here we are building up the R-values in terms of pips.  The R-value of each trade is the ratio of the realization of profit (in pips) to the 1R (in pips).
    (incf sum-of-r-values-in-pips (/ change number-of-pips-risked))

    (let ((change-in-dollars (* change 
				100000	;Since "change" is in decimal, and not actually the number of pips.
				(position-size (/ capital 200) number-of-pips-risked))))
      (incf amount-of-dollars-won change-in-dollars)
      (incf capital change-in-dollars)
      (if (< change-in-dollars 0)
	  (setf minimum-account-balance (min minimum-account-balance
					     capital)))

      ;; Over here we are building up the R-values in terms of dollars.  This R-distribution is affected by the position-sizing algorithm.
      (incf sum-of-r-values-in-dollars (/ change-in-dollars R-in-dollars)))))

(defun trades (start-data-point end-data-point)
  "This system currently has a t/p and a s/l.  That is it."
  (do ((i start-data-point (1+ i))
       (opening-price 0)
       (trailing-stop 0)
       (take-profit 0)
       (order-type nil)
       (number-of-pips-risked nil)
       (num-win 0)			;Number of winning trades.
       (num-lose 0)
       (amount-win 0)	   ;Number of winning pips, in a decimal form.
       (amount-lose 0)
       (sum-of-r-values-in-pips 0)
       (amount-of-dollars-won 0)
       (capital 10000)
       (minimum-account-balance 10000)
       (r-in-dollars 0)
       (sum-of-r-values-in-dollars 0))
      ((= i end-data-point)
       (format t "~&Winning-trades: ~A~&Losing trades: ~A~&Winning pips: ~A~&Losing pips: ~A~&R-expectancy (Pips): ~A~&Profit in $: ~A~&Ending capital: ~A~&Absolute drawdown: ~A~&R-expectancy ($): ~A" num-win num-lose amount-win amount-lose (/ sum-of-r-values-in-pips (+ num-win num-lose)) amount-of-dollars-won capital minimum-account-balance (/ sum-of-r-values-in-dollars (+ num-win num-lose))))
    (if (eql order-type nil)
	;; If there are no open trades, open one.
	(if (plusp (unscore (node-output test-set (- i start-data-point)) *array* :function-name #'+5close-diff)) ;If prediction is for price to go up...
	    (setf opening-price (openb (aref *array* (1+ i))) ;Buy
		  trailing-stop (- opening-price
				   (* 3 (atrb (aref *array* i))))
		  take-profit (+ opening-price
				 (unscore (node-output test-set (- i start-data-point)) *array* :function-name #'+5close-diff))
		  order-type 1
		  number-of-pips-risked (- opening-price trailing-stop)
		  r-in-dollars (/ capital 200))
	    (setf opening-price (openb (aref *array* (1+ i))) ;Sell
		  trailing-stop (+ opening-price
				   (* 3 (atrb (aref *array* i))))
		  take-profit (- opening-price 
				 (unscore (node-output test-set (- i start-data-point)) *array* :function-name #'+5close-diff))
		  order-type 0
		  number-of-pips-risked (- trailing-stop opening-price)
		  r-in-dollars (/ capital 200)))
	;; End part where trades are opened.
	;; If trade is open, do the following.
	(case order-type
	  (1				    ;If buy order
	   (cond ((< (low (aref *array* i)) ;if SL is hit, exit trade with loss.
		     trailing-stop)
		  (let ((change (- trailing-stop opening-price)))
		    (close-trade)
		    (setf order-type nil)))
		 ((> (high (aref *array* i)) ;If TP is hit, exit with profit.
		     take-profit)
		  (let ((change (- take-profit opening-price)))
		    (close-trade)
		    (setf order-type nil)))))
	  (0				     ;If sell order
	   (cond ((> (high (aref *array* i)) ;If SL is hit, exit with loss.
		     trailing-stop)
		  (let ((change (- opening-price trailing-stop)))
		    (close-trade)
		    (setf order-type nil)))
		 ((< (low (aref *array* i)) ;If TP is hit, exit with profit.
		     take-profit)
		  (let ((change (- opening-price take-profit)))
		    (close-trade)
		    (setf order-type nil)))))))))