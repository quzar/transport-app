(in-package :net.polyforms.lisp)

(defun place-near-p (place reference &optional (within 0.3))
  "Is PLACE near REFERENCE location to WITHIN decimal degrees in lat/long?"
  (location-near-p (place-to-location place)
                   (place-to-location reference)
                   within))

(defun location-near-p (location reference &optional (within 0.3))
  "Is LOCATION near REFERENCE location to WITHIN decimal degrees in lat/long?"
  (and (< (abs (- (location-latitude location)
                  (location-latitude reference))) within)
       (< (abs (- (location-longitude location)
                  (location-longitude reference))) within)))

(defun point-between (location1 location2 &key (point 1) (of 2))
  "Calculate point on a straight line between LOCATION1 and LOCATION2."
  (make-location :latitude (+ (location-latitude location1)
                              (* (- (location-latitude location2)
                                    (location-latitude location1))
                                 (/ point of)))
                 :longitude (+ (location-longitude location1)
                               (* (- (location-longitude location2)
                                     (location-longitude location1))
                                  (/ point of)))))

(defun distance-between (location1 location2 &key (unit :kilometers))
  "Calculate the straight line distance on the surface of the earth between
LOCATION1 and LOCATION2 using the \"Great Circle Distance Formula\".  The
values of the keyword UNIT can be :miles, :nautical-miles or :kilometers."
  (flet ((radians (deg) (* deg (/ pi 180.0))))
    (let ((lat1 (radians (location-latitude location1)))
          (lat2 (radians (location-latitude location2)))
          (lon1 (radians (location-longitude location1)))
          (lon2 (radians (location-longitude location2)))
          (radius (case unit
                    ((:miles) 3963.0)
                    ((:nautical-miles) 3437.74677)
                    ((:kilometers) 6378.7)
                    (t (error "Bad :unit keyword value: ~s." unit)))))
      (* radius
         (acos (+ (* (sin lat1) (sin lat2))
                  (* (cos lat1) (cos lat2) (cos (- lon2 lon1)))))))))

(defun nearest-location-in-list (reference location-list)
  "Calculate all the nearest location (to the reference) in the list given."
  (let ((record
         (first (sort
                 (mapcar
                  #'(lambda (x)
                      (list (first x) (second x)     ; makes a
                            (distance-between        ; distance
                             reference (second x)))) ; list
                  location-list)
                 #'(lambda (x y) (< (third x) (third y)))))))
    (list (first record) (second record))))
