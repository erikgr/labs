;; School stuff.
;;
;; Forecasting algorithms & forecasting accuracy calculation
;;
;; 2016 :: Erik Granholm

(load "csv.lisp")


;; Returns true if all supplied lists
;; are of equal length. I.e a list
;; of distinct list lengths has a length of 1
;;
(defun list-lengths-are-equal (&rest args)
	(let ((lengths (list)))
		(loop for alist in args do
			(setf lengths (append lengths (list (list-length alist)))))
		(if (> (list-length (remove-duplicates lengths)) 1)
			(return-from list-lengths-are-equal nil)
			(return-from list-lengths-are-equal T))))

;; a as percentage of b
;;
(defun percentage (a b)	(* (/ a b) 100))

;; return bottom n elements
;; equal to (last alist n)
;;
(defun right_n (alist n) (subseq alist (- (list-length alist) n) (list-length alist)))

;; return top n elements
;;
;(defun left_n (alist n)
;	(if (>= (list-length alist) n)
;		(subseq alist 0 n)
;		(first alist)))

(defun left_n (alist n)	(subseq alist 0 n))

;; Traverse timeline
;;
(defun timeline (ts)
	(let ((tl nil))
		(loop for n from 1 to (list-length ts) do
			(setf tl (append tl (list (left_n ts n)))))
		(return-from timeline tl)))

;; Generate list of incremental values
;;
(defun gen_alphas (increments)
	(let ((alphas nil))
		(loop for alpha from 0 to 1 by increments do
			(setf alphas (append alphas (list alpha))))
		(return-from gen_alphas alphas)))

;; For exponential smoothing use
;; fc_EXPSMOOTH
;;
(defun do_forecast (ts fc_fn args)
	(let ((fc nil) (fn_args nil))
		(loop for _t in (timeline ts) do
			(setf fn_args (list _t args))
			(setf fc (append fc (list (apply fc_fn fn_args)))))
	(return-from do_forecast fc)))

;;
;; ------------------------
;;  Forecast accuracy
;; ------------------------
;;

;; Forecast error
;;
(defun fa_Et (&key Ft Dt)
	(if (list-lengths-are-equal Ft Dt)
		(mapcar #'- Dt Ft)
		(format *standard-output* "fa_Et(): Invalid lists: unmatching lengths!")))

;; cumulative forecast error
;; 
(defun fa_CFE (&key Ft Dt do_map)
	(let ((sum 0) (error_ts (fa_Et :Ft ft :Dt Dt)))
		(if (functionp do_map)
			(setf error_ts (mapcar do_map error_ts)))
		(loop for num in error_ts do
			(setf sum (+ sum num)))
		(return-from fa_CFE sum)))

;; Mean Error
;;
(defun fa_ME (&key Ft Dt)
	(/ (fa_CFE :Ft ft :Dt Dt :do_map nil) (list-length Dt)))

;; Mean absolute error
;;
(defun fa_MAE (&key Ft Dt)
	(/ (fa_CFE :Ft Ft :Dt Dt :do_map #'abs) (list-length Dt)))

;; Mean squared error
;; TODO: BUG - SQUARED != SQRT :(
(defun fa_MSE (&key Ft Dt)
	(/ (fa_CFE :Ft ft :Dt Dt :do_map #'sqrt) (list-length Dt)))

;; Mean absolute percentage error
;;
(defun fa_MAPE (&key Ft Dt)
	(* 100 (/ (reduce #'+ 
		(mapcar #'/ (mapcar #'abs (fa_Et :Ft Ft :Dt Dt)) Dt))
	(list-length Dt))))


;;
;; ------------------------
;;  Forecasting algorithms
;; ------------------------
;;

;; Naive forecast
;;
(defun fc_NAIVE (ts &optional o)
	(car (last ts)))

;; Moving average
;;
(defun fc_MAVG (ts n)
	(/ (reduce #'+ (last ts n)) (list-length (last ts n))))

;; Weighted moving average
;;
(defun fc_WMAVG (ts w)
	(if (>= (list-length ts) (list-length w))
		(reduce #'+ (mapcar #'* (last ts (list-length w)) w))
		(car ts)))

;; Exponential smoothing
;;
;; Formula
;;
(defun fc_f_EXPSMOOTH (ts ft alpha)
	(+ ft (* alpha (- (car (last ts)) ft))))
;;
;; Algorithm
;;
(defun fc_EXPSMOOTH (ts alpha)
	(let ((ft (/ (reduce #'+ (left_n ts 2)) 2)) (fc nil))
		(loop for _t from 1 to (list-length ts) do
			(setf ft (fc_f_EXPSMOOTH (left_n ts _t) ft alpha))
			(setf fc (append fc (list ft))))
		(return-from fc_EXPSMOOTH fc)))


;; ------------------------
;;  Optimizations
;; ------------------------

;; Optimize alpha for minimal MAPE
;; 
;;
(defun optimize_mape_expsmooth (ts)
	(let ((min_mape 99999) (tmp_mape 0) (opt_alpha))
		(loop for param in (gen_alphas 0.01) do
			(setf tmp_mape (fa_MAPE :Dt ts :Ft (fc_EXPSMOOTH ts param)))
			(if (< tmp_mape min_mape)
				(progn
					(setf min_mape tmp_mape)
					(setf opt_alpha param))))
		(return-from optimize_mape_expsmooth opt_alpha)))

;; Optimize weights for minimal MAPE
;;
(defun optimize_mape_wmavg (ts)
	(let ((min_mape 99999) (tmp_mape 0) (opt_alpha))
		(loop for param in (gen_alphas 0.01) do
			(setf tmp_mape (fa_MAPE :Dt ts :Ft (do_forecast ts #'fc_WMAVG (list param (- 1 param)))))
			(if (< tmp_mape min_mape)
				(progn
					(setf min_mape tmp_mape)
					(setf opt_alpha param))))
		(return-from optimize_mape_wmavg opt_alpha)))

(defparameter *timeseries '(562.674  599  668.516  597.798  579.889  668.233  499.232  215.187  555.813  586.935  546.136  571.111  634.712  639.283  712.182  621.557  621  675.989  
501.322  220.286  560.727  602.53  626.379  605.508  646.783  658.442  712.906  687.714  723.916  707.183  629  237.53  613.296  730.444  734.925  651.812  676.155  748.183  810.681  
729.363  701.108  790.079  594.621  230.716  617.189  691.389  701.067  705.777  747.636  773.392  813.788  766.713  728.875  749.197  680.954  241.424  680.234  708.326  694.238  
772.071  795.337  788.421  889.968  797.393  751  821.255  691.605  290.655  727.147  868.355  812.39  799.556  843.038  847  941.952  804.309  840.307  871.528  656.33  370.508  742  
847.152  731.675  898.527  778.139  856.075  938.833  813.023  783.417  828.11  657.311  310.032  780  860  780  807.993  895.217  856.075  893.268  875  835.088  934.595  832.5  300  
791.443  900  781.729  880  875.024  992.968  976.804  968.697  871.675  1006.852  832.037  345.587  849.528  913.871 868.746 993.733))
;(do_forecast *timeseries #'fc_WMAVG '(0.3 0.7))
(fc_EXPSMOOTH *timeseries 0.7)
;(optimize_mape_expsmooth *timeseries)
;(optimize_mape_wmavg *timeseries)


(fc_EXPSMOOTH *timeseries 0.7)
