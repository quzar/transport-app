(in-package :polyforms)

; Area, Volume, and Conversion Tools
(defun conversion (&key (from :m) (to :m) (dim 1))
  (cond
    ((or (eq to :m) (eq to :meters))
     (case from
           ((:m :meters) 1.0)
           ((:ft :feet) (expt +m/ft+ dim))
           ((:in :inches) (expt (/ +m/ft+ 12) dim))
           ((:cm :centimeters) (expt 0.01 dim))
           (t (error "Bad :from keyword value: ~s." from))))
    ((or (eq from :m) (eq from :meters))
     (/ 1 (conversion :from to :to from :dim dim)))
    (t (* (conversion :from from :to :m :dim dim)
          (conversion :from :m :to to :dim dim)))))

(defun calc-area (height length &key
                         (input-units :m) (output-units :m) (shape :rect))
  (case shape
        ((:rect :rectangle)
         (* height length
            (conversion :from input-units :to output-units :dim 2)))
        ((:tri :triangle)
         (* 0.5 height length
            (conversion :from input-units :to output-units :dim 2)))
        (t (error "Bad :shape keyword value: ~s." shape))))

(defun calc-volume (height length depth &key
                           (input-units :m) (output-units :m) (shape :cube))
  (case shape
        ((:cube)
         (* height length depth
            (conversion :from input-units :to output-units :dim 3)))
        ((:tent)
         (* 0.5 height length depth
            (conversion :from input-units :to output-units :dim 3)))
        (t (error "Bad :shape keyword value: ~s." shape))))

(defun liters-per-100km (mpg)
  (* 100 +L/gallon+ (/ 1 (* +km/mile+ mpg))))

;; SAMPLE PLOTS
(defun plot-elevation (loc1 loc2 &key (steps 2) (min 0.0) (scale 1.0))
  (asterisk-plot (lambda (x)
                   (get-elevation
                     (point-between loc1 loc2 :point x :of steps)))
                 0 ; mininum
                 steps ; maximum
                 1 ; step size
                 :fmt-x-axis (lambda (x)
                               (format nil "~$ km"
                                       (* (distance-between loc1 loc2)
                                          (/ x steps))))
                 :fmt-y-axis (lambda (x) (format nil "~a" x))
                 :y-axis-min min :y-axis-scale scale))
