(in-package :net.polyforms.lisp)

;; Passengers
(defvar *dave* (make-passenger :name "Dave" :mass-kg 67))
(defvar *gwyn* (make-passenger :name "Gwyn" :mass-kg 52))
(defvar *dad* (make-passenger :name "Dad" :mass-kg 77))
(defvar *mom* (make-passenger :name "Mom" :mass-kg 62))

;; My Modes
(defvar *two-feet* (make-instance 'two-feet :owned t))
(defvar *bicycle* (make-instance 'bicycle :owned t))
(defvar *vibe* (make-instance 'automobile :owned t))

;; Other Modes
(defvar *taxi* (make-instance 'taxi))
(defvar *rental* (make-instance 'rental))
(defvar *carpool* (make-instance 'carpool))
(defvar *transit* (make-instance 'transit))
(defvar *shuttle* (make-instance 'shuttle))
(defvar *greyhound* (make-instance 'greyhound))
(defvar *red-arrow* (make-instance 'red-arrow))
(defvar *via-rail* (make-instance 'via-rail))
(defvar *airplane* (make-instance 'airplane))

;; Modes List
(defvar *modes-list*
  (list *two-feet*
        *bicycle*
        *vibe*
        *taxi*
        *rental*
        *carpool*
        *transit*
        *shuttle*
        *red-arrow*
        *via-rail*))

;; Cities Lists
(defvar *airports-list*
  (convert-place-info-hash *airports-hash*))

(defvar *cities-list*
  (convert-place-info-hash *cities-hash*))

(defvar *big-cities-list*
  (make-location-list
   `("Victoria, BC" "Vancouver, BC" "North Vancouver, BC"
     "Edmonton, AB" "Calgary, AB" "Winnipeg, MB" "Toronto, ON"
     "Ottawa, ON" "Montreal, QC" "Quebec, QC" "Halifax, NS")))

(defvar *red-arrow-stations-list*
  (make-location-list
   `("Edmonton, AB" "Calgary, AB" "Red Deer, AB"
     "Fort McMurray, AB" "Banff, AB" "Lake Louise, AB")))

(defvar *via-rail-stations-list*
  (make-location-list
   `("Kingston, ON" "Toronto, ON" "Winnipeg, MB"
     "Saskatoon, SK" "Edmonton, AB" "Jasper, AB"
     "Kamloops North, BC" "Vancouver, BC")))

(defun find-nearest-station (location mode)
  (cond ((subtypep (type-of mode) 'airplane)
         (nearest-location-in-list
          location *airports-list*))
        ((subtypep (type-of mode) 'via-rail)
         (nearest-location-in-list
          location *via-rail-stations-list*))
        ((subtypep (type-of mode) 'red-arrow)
         (nearest-location-in-list
          location *red-arrow-stations-list*))
        ((subtypep (type-of mode) 'greyhound)
         (nearest-location-in-list ; least-specific
          location *cities-list*))  ; mode goes last
        (t (list nil location)))) ; nothing found

;; TRIP CACHE STUFF
(defun clear-trip-cache (trip)
  (setf (legs trip) nil)
  (setf (food-use trip) nil)
  (setf (fuel-use trip) nil)
  (setf (emissions trip) nil)
  (setf (travel-time trip) nil)
  (setf (moving-time trip) nil)
  (setf (time-cost trip) nil)
  (setf (rental-cost trip) nil)
  (setf (lodging-cost trip) nil)
  (setf (split-for trip) nil))

(defun clear-location-cache (trip)
  (setf (start-location trip) nil)
  (setf (end-location trip) nil)
  (setf (distance trip) nil)
  (setf (uphill-elevation trip) nil))

(defun clear-distance-cache (trip)
  (clear-location-cache trip))

;; SPLIT-TRIP METHODS
(defgeneric split-trip (trip mode)
  (:documentation "Split trip (using mode) into corresponding legs."))

; anything added to save-leg needs to be added to convert-trip as well.
(defmacro save-leg (trip leg mode &body body)
  `(let ((temp-leg (make-instance ,leg)))
     (setf (baggage-kg temp-leg) (baggage-kg ,trip))
     (setf (on-clock? temp-leg) (on-clock? ,trip))
     (setf (passengers temp-leg) (passengers ,trip))
     (setf (start-date temp-leg) (start-date ,trip))
     (setf (start-location temp-leg) (calc-start-location ,trip))
     (setf (end-location temp-leg) (calc-end-location ,trip))
     (if (human-powered? ,mode)
         (setf (uphill-elevation temp-leg)
               (calc-uphill-elevation ,trip))
       (setf (uphill-elevation temp-leg) 0))
     ,@body
     (if (eq (length (legs ,trip)) 0)
         (setf (legs ,trip) (list temp-leg))
       (nconc (legs ,trip) (list temp-leg)))))

(defmethod split-trip (trip mode)
  (setf (legs trip) nil)
  ; initial waiting leg
  (when (> (extra-h mode) 0)
    (save-leg trip 'waiting-leg mode
              (setf (travel-time temp-leg) (extra-h mode))
              (setf (end-location temp-leg)
                    (calc-start-location trip))))
  ; regular & overnight legs
  (let* ((threshold-h (- 16.0 (calc-energy-all trip mode)))
         (threshold-km (* (km/h mode) threshold-h)))
    (cond ((and (> (calc-energy-all trip mode) 2.5) ; no overnight for
                (> (/ (calc-distance-km trip) ; passenger-only modes
                      (km/h mode)) threshold-h)) ; one day's travel
           (do ((travelled 0 (+ travelled threshold-km)))
               ((> (+ travelled threshold-km)
                   (calc-distance-km trip))
                (save-leg trip 'leg mode
                  (setf (distance temp-leg)
                        (- (calc-distance-km trip) travelled))))
             (save-leg trip 'leg mode
               (setf (distance temp-leg)
                     threshold-km))
             (save-leg trip 'overnight-leg mode
               (setf (travel-time temp-leg)
                     (- 24.0 threshold-h)))))
          (t (save-leg trip 'leg mode
               (setf (distance temp-leg)
                     (calc-distance-km trip))))))
  (setf (split-for trip) mode))

(defmethod split-trip (trip (mode automobile))
  (setf (legs trip) nil)
  (let ((distance-left (calc-distance-km trip))
        (swap-position nil))
    ; initial waiting leg (getting to the car)
    (when (> (extra-h mode) 0)
      (save-leg trip 'waiting-leg mode
                (setf (travel-time temp-leg) (extra-h mode))
                (setf (end-location temp-leg)
                      (calc-start-location trip))))
    ; big city rush hour leg (leaving the city)
    (when (and (eq (rush-hour? trip) t)
               (> distance-left 0))
      (save-leg trip 'rush-hour-leg mode
                (decf distance-left
                      (setf (distance temp-leg)
                            (min +city-size-km+ distance-left)))
                (setf (end-location temp-leg)
                      (calc-start-location trip))))
    ; city driving leg (leaving the city)
    (when (and (in-big-city? (calc-start-location trip))
               (eq (rush-hour? trip) nil)
               (> distance-left 0))
      (save-leg trip 'city-leg mode
                (decf distance-left
                      (setf (distance temp-leg)
                            (min +city-size-km+ distance-left)))
                (setf (end-location temp-leg)
                      (calc-start-location trip))))
    ; city driving leg (entering the city)
    (when (and (not (equal (calc-start-location trip)
                           (calc-end-location trip)))
               (in-big-city? (calc-end-location trip))
               (> distance-left 0))
      (setf swap-position (length (legs trip)))
      (save-leg trip 'city-leg mode
                (decf distance-left
                      (setf (distance temp-leg)
                            (min +city-size-km+ distance-left)))
                (setf (start-location temp-leg)
                      (calc-end-location trip))))
    ; highway driving & overnight legs
    (let* ((threshold-h
            (+ 8.0 (min (* (length (passengers trip)) 4.0) 16.0)))
           (threshold-km (* (km/h mode) threshold-h)))
      (cond ((and (< threshold-h 24.0)
                  (> (/ distance-left (km/h mode)) threshold-h))
             (do ((travelled 0 (+ travelled threshold-km)))
                 ((> (+ travelled threshold-km) distance-left)
                  (save-leg trip 'highway-leg mode
                            (setf (distance temp-leg)
                                  (- distance-left travelled))))
               (save-leg trip 'highway-leg mode
                         (setf (distance temp-leg)
                               threshold-km))
               (save-leg trip 'overnight-leg mode
                         (setf (travel-time temp-leg)
                               (- 24.0 threshold-h)))))
            (t (save-leg trip 'highway-leg mode
                         (setf (distance temp-leg)
                               distance-left)))))
    (if swap-position
        (let ((last-leg (nth swap-position (legs trip)))
              (temp-legs (legs trip)))
          (setf (legs trip)
                (nconc (remove last-leg temp-legs) (list last-leg)))))
    ; parking at the airport takes 15 minutes
    (when (and (pay-fuel-and-parking? mode)
               (at-airport? (calc-end-location trip)))
      (save-leg trip 'waiting-leg mode
                (setf (travel-time temp-leg) +airport-parking-extra-h+)
                (setf (start-location temp-leg)
                      (calc-end-location trip))))
    (setf (split-for trip) mode))) ; last call

;; CONVERT TRIP
(defun convert-trip (trip mode)
  (let* ((orig-start-place (start-place trip))
         (orig-end-place (end-place trip))
         (orig-start-location (calc-start-location trip))
         (orig-end-location (calc-end-location trip))
         (start-station (find-nearest-station
                         orig-start-location mode))
         (end-station (find-nearest-station
                       orig-end-location mode))
         (new-trip (make-instance
                    'trip
                    :from (first start-station)
                    :to (first end-station)
                    :passengers (passengers trip)
                    :start-date (start-date trip)
                    :length-days (length-days trip)
                    :parking-rate (parking-rate trip)
                    :baggage-kg (baggage-kg trip)
                    :on-clock? (on-clock? trip) ; don't worry
                    :rush-hour? (rush-hour? trip) ; about these
                    :weekend? (weekend? trip)))) ; for main trip
    ; create trip from home to station
    (setf (trip-to-station new-trip)
          (make-instance
           'trip
           :from orig-start-place
           :to (first start-station)
           :passengers (passengers trip)
           :start-date (start-date trip)
           :length-days (length-days trip)
           :parking-rate (parking-rate trip)
           :baggage-kg (baggage-kg trip)
           :on-clock? (on-clock? trip)
           :rush-hour? (rush-hour? trip)
           :weekend? (weekend? trip)))
    ; create trip from station to destination
    (setf (trip-from-station new-trip)
          (make-instance
           'trip
           :from (first end-station)
           :to orig-end-place
           :passengers (passengers trip)
           :start-date (start-date trip)
           :length-days (length-days trip)
           :parking-rate (parking-rate trip)
           :baggage-kg (baggage-kg trip)
           :on-clock? (on-clock? trip)
           :rush-hour? (rush-hour? trip)
           :weekend? (weekend? trip)))
    ; force save location data since we have it already
    (setf (start-location new-trip)
          (second start-station))
    (setf (end-location new-trip)
          (second end-station))
    (setf (start-location (trip-to-station new-trip))
          orig-start-location)
    (setf (end-location (trip-to-station new-trip))
          (second start-station))
    (setf (start-location (trip-from-station new-trip))
          (second end-station))
    (setf (end-location (trip-from-station new-trip))
          orig-end-location)
    (setf (converted-for new-trip) mode)
    new-trip)) ; return value

;; UTILITY MACROS
(defmacro calc-with-legs (trip mode func)
  `(let ((total 0))
     (unless (eq (split-for ,trip) ,mode)
       (split-trip ,trip ,mode))
     (loop for leg in (legs ,trip) do
           (let ((temp-total 0))
             (incf total
                   (incf temp-total
                         (,func leg ,mode)))))
     total))

(defmacro with-trip-cache (trip mode slot func)
  `(let ((cached-value (,slot ,trip)))
     (if (eq (split-for ,trip) ,mode)
         (if cached-value
             cached-value
           (setf (,slot ,trip)
                 (calc-with-legs ,trip ,mode ,func)))
       (progn (clear-trip-cache ,trip)
              (setf (,slot ,trip)
                    (calc-with-legs ,trip ,mode ,func))))))

(defmacro with-leg-cache (leg slot &body body)
  `(let ((cached-value (,slot ,leg)))
     (if cached-value
         cached-value
       (setf (,slot ,leg) ,@body))))

(defmacro with-distance-cache (trip &body body)
  `(let ((cached-value (distance ,trip)))
     (if cached-value
         cached-value
       (setf (distance ,trip) ,@body))))

(defmacro with-location-cache (trip slot &body body)
  `(let ((cached-value (,slot ,trip)))
     (if cached-value
         cached-value
       (setf (,slot ,trip) ,@body))))

;; UTILITY FUNCTIONS (quick sanity checks)
(defun owned? (mode)
  (and (slot-exists-p mode 'owned) (eq (owned mode) t)))

(defun rented? (mode)
  (and (slot-exists-p mode 'rented) (eq (rented mode) t)))

(defun human-powered? (mode)
  (and (slot-exists-p mode 'human-powered)
       (eq (human-powered mode) t)))

(defun pay-fuel-and-parking? (mode)
  (and (subtypep (type-of mode) 'automobile)
       (or (owned? mode) (rented? mode))))

(defun near-via-rail-station? (location)
  (loop named find-station for station in *via-rail-stations-list* do
        (when (location-near-p location (second station) 0.185) ; was 0.3
          (return-from find-station t))))

(defun near-red-arrow-station? (location)
  (loop named find-station for station in *red-arrow-stations-list* do
        (when (location-near-p location (second station) 0.185) ; was 0.3
          (return-from find-station t))))

(defun at-airport? (location)
  (loop named find-station for station in *airports-list* do
        (when (location-near-p location (second station) 0.092) ; was 0.1
          (return-from find-station t))))

(defun in-big-city? (location)
  (loop named find-city for city in *big-cities-list* do
        (when (location-near-p location (second city) 0.185) ; was 0.3
          (return-from find-city t))))

(defun trip-within-city? (trip)
  (location-near-p
   (calc-start-location trip)
   (calc-end-location trip)
   0.185)) ; was 0.3

;; TRIP/LEG CALC METHODS (methods that store calculations for every leg)
(defgeneric calc-travel-time-h (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode travel-time
            calc-travel-time-h))
  (:method ((leg leg) mode)
           (with-leg-cache
            leg travel-time
            (cond ((slot-exists-p leg 'leg-km/h)
                   (/ (distance leg) (km/h leg)))
                  (t (/ (distance leg) (km/h mode))))))
  (:documentation "Calculate the time needed to travel this distance."))

(defgeneric calc-moving-time-h (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode moving-time
            calc-moving-time-h))
  (:method ((leg leg) mode)
           (if (or (eq (type-of leg) 'waiting-leg)
                   (eq (type-of leg) 'overnight-leg))
               0 ; no moving time if waiting/overnight
             (calc-travel-time-h leg mode)))
  (:documentation "Calculate the time needed to travel this distance."))

(defgeneric calc-food-use-kJ (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode food-use
            calc-food-use-kJ))
  (:method ((leg leg) mode)
           (with-leg-cache
            leg food-use
            (cond ((eq (length (passengers leg)) 0) 0)
                  ((slot-exists-p mode 'energy-one)
                   (* +kJ/kcal+ (calc-travel-time-h leg mode)
                      (+ (* (energy-one mode)
                            (passenger-mass-kg
                             (first (passengers leg))))
                         (if (or (eq (type-of leg) 'waiting-leg)
                                 (eq (type-of leg) 'overnight-leg))
                             (* +MET-idle+
                                (loop for pass in (rest (passengers leg))
                                   sum (passenger-mass-kg pass)))
                             (* (calc-energy-all leg mode)
                                (loop for pass in (rest (passengers leg))
                                   sum (passenger-mass-kg pass)))))))
                  (t (* +kJ/kcal+ (calc-travel-time-h leg mode)
                        (+ (if (or (eq (type-of leg) 'waiting-leg)
                                   (eq (type-of leg) 'overnight-leg))
                               (* +MET-idle+
                                  (loop for pass in (passengers leg)
                                     sum (passenger-mass-kg pass)))
                               (* (calc-energy-all leg mode)
                                  (loop for pass in (passengers leg)
                                     sum (passenger-mass-kg pass))))))))))
  (:documentation "Calculate food needed to go on this trip."))

(defgeneric calc-fuel-use-L (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode fuel-use
            calc-fuel-use-L))
  (:method ((leg leg) mode)
           (with-leg-cache
            leg fuel-use
            (+ (cond ((slot-exists-p leg 'fuel-L/km)
                      (* (distance leg) (fuel-L/km leg)
                         (ceiling (/ (length (passengers leg))
                                     (seats mode)))))
                     (t (* (distance leg) (fuel-L/km mode)
                           (ceiling (/ (length (passengers leg))
                                       (seats mode))))))
               (/ (* (calc-food-use-kJ leg mode)
                     +food-production-waste+) ; effective kJ
                  +kJ/L-gasoline+)))) ; effective L gasoline
  (:documentation "Calculate fuel needed to travel this distance."))

(defgeneric calc-time-cost-$ (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode time-cost
            calc-time-cost-$))
  (:method ((leg leg) mode)
           (with-leg-cache
            leg time-cost
            (cond ((eq (type-of leg) 'waiting-leg)
                   (* (calc-travel-time-h leg mode)
                      (length (passengers leg))
                      (if (on-clock? leg)
                          +working-wage-$/h+
                        +free-time-$/h+)))
                  ((eq (type-of leg) 'overnight-leg)
                   (* (max (- (calc-travel-time-h leg mode) 8.0) 0.0)
                      (length (passengers leg))
                      (if (on-clock? leg)
                          +working-wage-$/h+
                        +free-time-$/h+)))
                  (t (* (calc-travel-time-h leg mode) ; multiply by time
                        (length (passengers leg)) ; how many passengers?
                        (+ (if (human-powered? mode)
                               (if (daylight-savings? (start-date leg))
                                   0 ; no extra cost
                                 +winter-$/h+) ; cost for winter
                             0) ; no extra cost
                           (if (on-clock? leg)
                               +working-wage-$/h+
                             +free-time-$/h+)))))))
  (:documentation "Calculate the cost of the time to travel this trip."))

(defgeneric calc-lodging-cost-$ (trip mode)
  (:method (trip mode)
           (with-trip-cache
            trip mode lodging-cost
            calc-lodging-cost-$))
  (:method ((leg leg) mode)
           (with-leg-cache
            leg lodging-cost
            (if (eq (type-of leg) 'overnight-leg)
                (if (daylight-savings?
                     (start-date leg))
                    +campground-cost-$+
                  (* +hostel-cost-$/person+
                     (+ (if (or (eq (type-of mode) 'taxi)
                                (eq (type-of mode) 'carpool))
                            1 0) ; pay for driver's lodging
                        (length (passengers leg))))) 0)))
  (:documentation "Calculate the lodging costs to travel this trip."))

(defgeneric calc-rental-cost-$ (trip mode)
  (:method (trip mode)
    (declare (ignore trip mode)) 0)
  (:method (trip (mode rental))
           (+ (* (if (and (> (length-days trip) 1.5)
                          (weekend? trip))
                     (/ +rental-cost-$+ 2)
                   +rental-cost-$+) ; discount for weekend trips
                 (max (* (ceiling (length-days trip)) 0.5) 1) ; min 1 day
                 (ceiling (/ (length (passengers trip)) ; # of cars needed
                             (seats mode))))
              (with-trip-cache
               trip mode rental-cost
               calc-rental-cost-$)))
  (:method ((leg leg) (mode rental))
           (with-leg-cache
            leg rental-cost
            (cond ((eq (type-of leg) 'overnight-leg)
                   (* +rental-cost-$+
                      (ceiling (/ (length (passengers leg))
                                  (seats mode)))))
                  (t 0))))
  (:documentation "Calculate the rental costs to travel this trip."))

;; OTHER CALC FUNCTIONS (that don't need generic methods/caching)
(defun calc-start-location (trip)
  (with-location-cache
   trip start-location
   (place-to-location (start-place trip))))

(defun calc-end-location (trip)
  (with-location-cache
   trip end-location
   (place-to-location (end-place trip))))

(defun calc-distance-km (trip)
  (with-distance-cache
   trip (* (distance-between
            (calc-start-location trip)
            (calc-end-location trip))
           +road-distance-error-%+)))

(defun calc-uphill-elevation (trip)
  (with-location-cache
   trip uphill-elevation
   (let* ((uphill-elevation 0)
          (temp-distance
           (calc-distance-km trip))
          (steps
           (if (> temp-distance 1)
               (round (* (log temp-distance) 2))
             1))
          (elevation-list
           (progn
             (format t "~&Getting ~A elevations between ~A and ~A..."
                     steps
                     (if (slot-exists-p trip 'start-place)
                         (start-place trip) (calc-start-location trip))
                     (if (slot-exists-p trip 'end-place)
                         (end-place trip) (calc-end-location trip)))
             (loop for x from 0 to steps collect
                   (get-elevation
                    (point-between (calc-start-location trip)
                                   (calc-end-location trip)
                                   :point x :of steps))))))
     (loop named check-elevations for i
           from 1 to (1- (length elevation-list)) do
           (let ((temp-elevation-gain
                  (- (nth i elevation-list)
                     (nth (1- i) elevation-list))))
             (when (> temp-elevation-gain 0)
               (incf uphill-elevation temp-elevation-gain))))
     uphill-elevation)))

(defun calc-average-grade (trip)
  (if (> (calc-distance-km trip) 0)
      (/ (calc-elevation-gain trip)
         (* (calc-distance-km trip) 1000))
    0)) ; don't divide by zero

(defun calc-uphill-grade (trip)
  (if (> (calc-distance-km trip) 0)
      (/ (calc-uphill-elevation trip)
         (* (calc-distance-km trip) 1000))
    0)) ; don't divide by zero

(defun calc-elevation-gain (trip)
  (- (get-elevation (calc-end-location trip))
     (get-elevation (calc-start-location trip))))

(defun calc-energy-all (trip mode) ; MET energy required for trip
  (+ (energy-all mode)              ; using mode (per passenger)
     (if (and (human-powered? mode)
              (> (length (passengers trip)) 0))
         (+ (* (calc-uphill-grade trip)
               (energy-all mode) +MET-climb-factor+)
            (* (/ (baggage-kg trip)
                  (length (passengers trip)))
               +MET-baggage-factor+))
       0))) ; not human powered

(defun calc-emissions-kg (trip mode)
  (* (calc-fuel-use-L trip mode)
     (if (subtypep (type-of mode) 'automobile)
         +automobile-lifecycle-cost+
       1) ; if no car, don't figure in auto lifecycle
     ; my decision is to charge the user for this cost
     ; regardless of the fact that it was "incurred"
     ; at the time of purchase/production.
     +kg-CO2/L-gasoline+))

(defun calc-emissions-cost-$ (trip mode)
  (* (calc-emissions-kg trip mode) +CO2-$/kg+))

(defun calc-food-cost-$ (trip mode)
  (* (calc-food-use-kJ trip mode) +food-$/kJ+))

(defun calc-fuel-cost-$ (trip mode)
  (if (pay-fuel-and-parking? mode)
      (* (calc-fuel-use-L trip mode) +gasoline-$/L+)
    0)) ; don't pay for fuel

(defun calc-parking-cost-$ (trip mode)
  (if (pay-fuel-and-parking? mode)
      (* (parking-rate trip) (ceiling (length-days trip)) 0.5)
    0)) ; don't pay for parking

(defun calc-carpool-cost-$ (trip mode)
  (if (subtypep (type-of mode) 'carpool)
      (* 2.0 ; pay both ways for driver
         (calc-travel-time-h trip mode)
         (if (on-clock? trip)
             +working-wage-$/h+
           +free-time-$/h+))
    0)) ; not a carpool type

(defun calc-taxi-cost-$ (trip mode)
  (if (subtypep (type-of mode) 'taxi)
      (cond ((location-near-p
              (start-location trip)
              (place-to-location "Edmonton, AB"))
             (do ((total 2.50 (+ total 0.20))
                  (travelled 0 (+ travelled 0.21)))
                 ((> travelled (- (calc-distance-km trip) 0.21))
                  (* total +tip-rate-%+))))
            (t (do ((total 3.00 (+ total 0.20))
                    (travelled 0 (+ travelled 0.15)))
                   ((> travelled (- (calc-distance-km trip) 0.21))
                    (* total +tip-rate-%+)))))
    0)) ; not a taxi type

(defun collect-$/km-costs (trip mode)
  (loop for item in ($/km-costs mode)
        collect (list (* (second item)
                         (calc-distance-km trip))
                      (format nil "~A ~A" "spent on"
                              (first item)))))

(defun collect-$/kg-km-costs (trip mode)
  (loop for item in ($/kg-km-costs mode)
       collect (list (* (second item)
                        (baggage-kg trip)
                        (calc-distance-km trip))
                     (format nil "~A ~A" "spent on"
                             (first item)))))

(defun collect-$/ppj-h-moving-costs (trip mode)
  (loop for item in ($/ppj-h-moving-costs mode)
        collect (list (* (second item)
                         (length (passengers trip))
                         (calc-moving-time-h trip mode))
                      (format nil "~A ~A" "spent on"
                              (first item)))))

(defun collect-$/ppj-km-costs (trip mode)
  (loop for item in ($/ppj-km-costs mode)
       collect (list (* (second item)
                        (length (passengers trip))
                        (calc-distance-km trip))
                     (format nil "~A ~A" "spent on"
                             (first item)))))

(defun collect-$/ppj-costs (trip mode)
  (loop for item in ($/ppj-costs mode)
        collect (list (* (second item)
                         (length (passengers trip)))
                      (format nil "~A ~A" "spent on"
                              (first item)))))

(defmacro fill-calc-table (calc-list)
  `(loop for calc in ,calc-list do
         (if (gethash (second calc) calc-table)
             (incf (gethash (second calc) calc-table)
                   (first calc))
           (setf (gethash (second calc) calc-table)
                 (first calc)))))

(defun make-calc-list (trip mode)
  (nconc
   (list (list (calc-emissions-cost-$ trip mode)
               "spent on carbon offsets")
         (list (calc-food-cost-$ trip mode)
               "spent on food costs")
         (list (calc-fuel-cost-$ trip mode)
               "spent on fuel costs")
         (list (calc-taxi-cost-$ trip mode)
               "spent on taxi costs")
         (list (calc-carpool-cost-$ trip mode)
               "spent by ride provider")
         (list (calc-parking-cost-$ trip mode)
               "spent on parking costs")
         (list (calc-lodging-cost-$ trip mode)
               "spent on lodging costs")
         (list (calc-rental-cost-$ trip mode)
               "spent on rental costs")
         (list (calc-time-cost-$ trip mode)
               "spent on lost productivity"))
   (collect-$/kg-km-costs trip mode)
   (collect-$/km-costs trip mode)
   (collect-$/ppj-costs trip mode)
   (collect-$/ppj-km-costs trip mode)
   (collect-$/ppj-h-moving-costs trip mode)))

(defun compile-costs (trip mode &key table)
  (let ((calc-table
         (if table table
           (make-hash-table :test 'equalp))))
    (fill-calc-table
     (make-calc-list trip mode))
    calc-table))

(defun merge-costs (trip mode-list &key table)
  (let ((calc-table
         (if table
             table
           (make-hash-table :test 'equalp))))
    (fill-calc-table
     (nconc
      (make-calc-list
       trip (second mode-list))
      (make-calc-list
       (trip-to-station trip)
       (first mode-list))
      (make-calc-list
       (trip-from-station trip)
       (third mode-list))))
    calc-table))

(defun calc-total-cost-$ (trip mode &key table (fmt? t))
  (let* ((total 0)
         (calc-table
          (if table table
            (compile-costs trip mode)))
         (calc-list
          (sort
           (loop for cost-type being the
                 hash-keys of calc-table
                 using (hash-value calc)
                 collect (list calc cost-type)
                 summing calc into temp-total
                 finally (setf total temp-total))
           #'(lambda (x y) (> (first x) (first y))))))
    (when fmt?
      (loop for calc in calc-list
         do (if (> (abs (first calc)) 0.005)
                (format t "~&$~$ ~A"
                        (first calc) (second calc))))
      (format t "~&Total cost: $~$" total))
    (values total calc-table)))

(defun print-costs-in-table (table)
  (calc-total-cost-$ nil nil :table table :fmt? t))

(defun make-modes-list (trip &key (exclude-if #'null) exclude)
  (remove-if
   #'(lambda (mode)
       (or
        (funcall exclude-if mode)
        (find (type-of mode) exclude)
        (and (eq (type-of mode) 'transit)
             (not (trip-within-city? trip)))
        (and (eq (type-of mode) 'shuttle)
             (and (not (and (at-airport? (calc-start-location trip))
                            (in-big-city? (calc-end-location trip))))
                  (not (and (in-big-city? (calc-start-location trip))
                            (at-airport? (calc-end-location trip))))))
        (and (eq (type-of mode) 'red-arrow) ; only worth taking if stations are
             (or (not (near-red-arrow-station? ; close enough to start and end
                       (calc-start-location trip)))
                 (not (near-red-arrow-station?
                       (calc-end-location trip)))
                 (trip-within-city? trip)))
        (and (eq (type-of mode) 'via-rail) ; only worth taking if stations are
             (or (not (near-via-rail-station? ; close enough to start and end
                       (calc-start-location trip)))
                 (not (near-via-rail-station?
                       (calc-end-location trip)))
                 (trip-within-city? trip)))))
   *modes-list*))

(defun calc-best-modes (trip &key modes-list exclude (exclude-if #'null))
  "Returns a sorted list of the best modes with calculations for a trip"
  (let* ((trip-modes-list
          (if modes-list modes-list
              (make-modes-list trip
                               :exclude exclude
                               :exclude-if exclude-if)))
         (trip-calcs
          (sort (nconc
                 (loop for mode in trip-modes-list
                    collect (multiple-value-bind (total table)
                                (calc-total-cost-$ trip mode :fmt? nil)
                              (list mode table total)))
                 ; if modes-list is provided, only one mode is to be used
                 ; the following code ADDS to the modes list and assumes
                 ; the existence of *airplane* and *greyhound* as globals
                 (unless (or modes-list
                             (find 'airplane exclude)
                             (equal (find-nearest-station (start-location trip) *airplane*)
                                    (find-nearest-station (end-location trip) *airplane*)))
                   (calc-best-modes-to-station ; airplane is always an option
                    trip *airplane* :exclude (nconc (list 'greyhound) exclude)))
                 (unless (or modes-list
                             (find 'greyhound exclude)
                             (equal (find-nearest-station (start-location trip) *greyhound*)
                                    (find-nearest-station (end-location trip) *greyhound*)))
                   (calc-best-modes-to-station ; greyhound is always an option
                    trip *greyhound* :exclude (nconc (list 'airplane) exclude))))
               #'(lambda (x y) (< (third x) (third y))))))
    trip-calcs))

(defun calc-best-mode (trip &key modes-list exclude (exclude-if #'null))
  "Returns only the best mode with calculations for a trip"
  (first (calc-best-modes trip
                          :modes-list modes-list
                          :exclude exclude
                          :exclude-if exclude-if)))

(defun calc-best-modes-to-station (trip main-mode &key exclude)
  "First converts a trip to include station trips, then calculates best modes"
  (let* ((converted-trip
          (convert-trip trip main-mode))
         (converted-trip-calc
          (calc-best-mode
           converted-trip
           :modes-list (list main-mode)))
         (to-station-calc
          (calc-best-mode
           (trip-to-station converted-trip)
           :exclude (nconc (list (type-of main-mode)) exclude)))
         (from-station-calc
          (calc-best-mode
           (trip-from-station converted-trip)
           :exclude (nconc (list (type-of main-mode)) exclude)
           :exclude-if #'owned?))
         (modes-list
          (list (first to-station-calc)
                (first converted-trip-calc)
                (first from-station-calc))))
    (list (list modes-list
                (merge-costs converted-trip modes-list)
                (+ (third to-station-calc)
                   (third converted-trip-calc)
                   (third from-station-calc))))))

(defun calc-total-time-h (trip mode &key (fmt? t))
  (let ((total (calc-travel-time-h trip mode)))
    (if fmt?
        (format t "~&~A" (fmt-duration total))
      total)))

;; REPORTING FUNCTIONS
(defun print-legs (trip mode)
  (let ((leg-count 0)
        (total-time 0))
    (split-trip trip mode)
    (loop for leg in (legs trip) do
         (format t "~&~A. ~A ~A" (incf leg-count)
                 (type-of mode) (type-of leg))
         (format t " (day #~A)" (1+ (floor (/ total-time 24))))
         (format t "~&start: ~A" (calc-start-location leg))
         (format t "~&end: ~A" (calc-end-location leg))
         (format t "~&distance: ~$ km" (distance leg))
         (format t "~&uphill elevation: ~$ m" (uphill-elevation leg))
         (format t "~&food use: ~$ kJ" (calc-food-use-kJ leg mode))
         (format t "~&fuel use: ~$ L" (calc-fuel-use-L leg mode))
         (format t "~&emissions: ~$ kg" (calc-emissions-kg leg mode))
         (format t "~&time cost: $~$" (calc-time-cost-$ leg mode))
         (format t "~&rental cost: $~$" (calc-rental-cost-$ leg mode))
         (format t "~&lodging cost: $~$" (calc-lodging-cost-$ leg mode))
         (format t "~&travel time: ~A"
                 (fmt-duration (calc-travel-time-h leg mode)))
         (format t "~%~%")
         (incf total-time (calc-travel-time-h leg mode)))))

(defun trip-report (trip mode-list &optional calc-table)
  (if (listp mode-list)
      ; expects a list of three modes
      (let* ((temp-trip
              (if (eq (converted-for trip) (second mode-list))
                  trip
                (convert-trip trip (second mode-list))))
             (temp-calc-table
              (if calc-table
                  calc-table
                (merge-costs temp-trip mode-list))))
        (format t "~&~A -> ~A ->~&~A -> ~A: ~$ km"
                (start-place (trip-to-station temp-trip))
                (start-place temp-trip)
                (end-place temp-trip)
                (end-place (trip-from-station temp-trip))
                (+ (calc-distance-km (trip-to-station temp-trip))
                   (calc-distance-km temp-trip)
                   (calc-distance-km (trip-from-station temp-trip))))
        (format t "~&~A"
                (fmt-duration
                 (+ (calc-travel-time-h
                     (trip-to-station temp-trip)
                     (first mode-list))
                    (calc-travel-time-h
                     temp-trip
                     (second mode-list))
                    (calc-travel-time-h
                     (trip-from-station temp-trip)
                     (third mode-list)))))
        (print-costs-in-table temp-calc-table)
        (format t "~%~%"))
    ; otherwise it's just a single mode trip
      (progn
        (format t "~&~A to ~A: ~$ km"
                (start-place trip)
                (end-place trip)
                (calc-distance-km trip))
        (calc-total-time-h trip mode-list :fmt? t)
        (calc-total-cost-$ trip mode-list :fmt? t)
        (format t "~%~%")))
  nil) ; return value

(defun list-best-modes (trip &key (n 3) exclude (exclude-if #'null))
  "Print the results of trip/mode calculations"
  (let ((trip-calcs (calc-best-modes trip :exclude exclude
                                          :exclude-if exclude-if)))
    (format t "~%~%")
    (loop for i from 0 to (1- (min n (length trip-calcs))) do
         (let ((mode-list (first (nth i trip-calcs)))
               (calc-table (second (nth i trip-calcs))))
           (if (listp mode-list)
               (progn
                 (format t "~&MODE #~A: ~A -> ~A -> ~A"
                         (1+ i) (type-of (first mode-list))
                         (type-of (second mode-list))
                         (type-of (third mode-list)))
                 (trip-report trip mode-list calc-table))
             (progn
               (format t "~&MODE #~A: ~A"
                       (1+ i) (type-of mode-list))
               (trip-report trip mode-list)))))))