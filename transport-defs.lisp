(in-package :net.polyforms.lisp)

; Passenger Definition
(defstruct passenger (name "Unnamed") (mass-kg 67))

; Trip Definition
(defclass trip nil
  ((start-place :accessor start-place :initarg :from
                :initform "10145 109 St, Edmonton, AB")
   (end-place :accessor end-place :initarg :to
              :initform "7008 Roper Rd, Edmonton, AB")
   (passengers :accessor passengers :initarg :passengers
               :initform (list (make-passenger)))
   (start-date :accessor start-date :initarg :start-date :initform (today))
   (length-days :accessor length-days :initarg :length-days :initform 1.0)
   (parking-rate :accessor parking-rate :initarg :parking-rate :initform 0.0)
   (baggage-kg :accessor baggage-kg :initarg :baggage-kg :initform 0.0)
   (on-clock? :accessor on-clock? :initarg :on-clock? :initform nil)
   (rush-hour? :accessor rush-hour? :initarg :rush-hour? :initform nil)
   (weekend? :accessor weekend? :initarg :weekend? :initform nil)
   (start-location :accessor start-location :initform nil) ; calculated
   (end-location :accessor end-location :initform nil) ; calculated
   (trip-distance :accessor distance :initform nil) ; calculated
   (trip-uphill :accessor uphill-elevation :initform nil) ; calculated
   (trip-food-use :accessor food-use :initform nil) ; calculated
   (trip-fuel-use :accessor fuel-use :initform nil) ; calculated
   (trip-emissions :accessor emissions :initform nil) ; calculated
   (trip-travel-time :accessor travel-time :initform nil) ; calculated
   (trip-moving-time :accessor moving-time :initform nil) ; calculated
   (trip-time-cost :accessor time-cost :initform nil) ; calculated
   (trip-rental-cost :accessor rental-cost :initform nil) ; calculated
   (trip-lodging-cost :accessor lodging-cost :initform nil) ; calculated
   (trip-split-for :accessor split-for :initform nil) ; calculated
   (trip-converted-for :accessor converted-for :initform nil) ; calculated
   (trip-to-station :accessor trip-to-station :initform nil) ; calculated
   (trip-from-station :accessor trip-from-station :initform nil) ; calculated
   (legs :accessor legs :initform nil))) ; calculated

; Leg Definitions
(defclass leg nil
  ((start-date :accessor start-date :initform nil)
   (passengers :accessor passengers :initform nil)
   (baggage-kg :accessor baggage-kg :initform nil)
   (on-clock? :accessor on-clock? :initform nil)
   (start-location :accessor start-location :initform nil)
   (end-location :accessor end-location :initform nil)
   (leg-distance :accessor distance :initform nil)
   (leg-uphill :accessor uphill-elevation :initform nil)
   (leg-food-use :accessor food-use :initform nil)
   (leg-fuel-use :accessor fuel-use :initform nil)
   (leg-emissions :accessor emissions :initform nil)
   (leg-travel-time :accessor travel-time :initform nil)
   (leg-time-cost :accessor time-cost :initform nil)
   (leg-rental-cost :accessor rental-cost :initform nil)
   (leg-lodging-cost :accessor lodging-cost :initform nil)))

(defclass waiting-leg (leg)
  ((leg-distance :accessor distance :initform 0.0)
   (leg-travel-time :accessor travel-time :initform 0.0)))

(defclass overnight-leg (leg)
  ((leg-distance :accessor distance :initform 0.0)
   (leg-travel-time :accessor travel-time :initform 0.0)))

(defclass highway-leg (leg)
  ((leg-km/h :accessor km/h :initform +vibe-highway-km/h+)
   (fuel-L/km :accessor fuel-L/km :initform +vibe-highway-L/km+)))

(defclass city-leg (leg)
  ((leg-km/h :accessor km/h :initform +vibe-city-km/h+)
   (fuel-L/km :accessor fuel-L/km :initform +vibe-city-L/km+)))

(defclass rush-hour-leg (leg)
  ((leg-km/h :accessor km/h :initform +vibe-rush-hour-km/h+)
   (fuel-L/km :accessor fuel-L/km :initform +vibe-rush-hour-L/km+)))

; Mode Definitions
(defclass transport-mode nil
  ((km/h :accessor km/h :initform 0.0)
   ($/h :accessor $/h :initform 0.0)
   ($/kg-km-costs :accessor $/kg-km-costs :initform nil)
   ($/km-costs :accessor $/km-costs :initform nil)
   ($/ppj-costs :accessor $/ppj-costs :initform nil)
   ($/ppj-km-costs :accessor $/ppj-km-costs :initform nil)
   ($/ppj-h-moving-costs :accessor $/ppj-h-moving-costs :initform nil)
   (mode-extra-h :accessor extra-h :initform 0.0)
   (fuel-L/km :accessor fuel-L/km :initform 0.0)
   (seats :accessor seats :initform 1)))

(defclass two-feet (transport-mode)
  ((km/h :accessor km/h :initform +two-feet-km/h+)
   ($/kg-km-costs :accessor $/kg-km-costs :initform
                  `(("baggage handling" ,+baggage-$/km+)))
   ($/ppj-h-moving-costs :accessor $/ppj-h-moving-costs :initform
                         `(("exercise benefit" ,(- 1.0 +MET-walking+))))
   (energy-all :accessor energy-all :initform +MET-walking+)
   (human-powered :accessor human-powered :initform t)
   (owned :accessor owned :initform nil :initarg :owned)))

(defclass bicycle (transport-mode)
  ((km/h :accessor km/h :initform +bicycle-km/h+)
   ($/km-costs :accessor $/km-costs :initform
               `(("depreciation" ,+bike-extra-depreciation-$/km+)
                 ("maintenance" ,+bike-maintenance-$/km+)))
   ($/kg-km-costs :accessor $/kg-km-costs :initform
                  `(("baggage handling" ,+baggage-$/km+)))
   ($/ppj-h-moving-costs :accessor $/ppj-h-moving-costs :initform
                         `(("exercise benefit" ,(- 1.0 +MET-cycling+))))
   (mode-extra-h :accessor extra-h :initform +bicycle-extra-h+)
   (energy-all :accessor energy-all :initform +MET-cycling+)
   (human-powered :accessor human-powered :initform t)
   (owned :accessor owned :initform nil :initarg :owned)))

(defclass transit (transport-mode)
  ((km/h :accessor km/h :initform +transit-km/h+)
   ($/ppj-costs :accessor $/ppj-costs :initform
                `(("transit tickets" ,+transit-cost-$+)))
   (mode-extra-h :accessor extra-h :initform +bus-stop-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +bus-L/km+)
   (energy-all :accessor energy-all :initform +MET-idle+)))

(defclass automobile (transport-mode)
  ((km/h :accessor km/h :initform +vibe-highway-km/h+)
   ($/km-costs :accessor $/km-costs :initform
               `(("depreciation" ,+car-extra-depreciation-$/km+)
                 ("fines and fees" ,+car-related-fines-$/km+)
                 ("maintenance" ,+car-maintenance-$/km+)
                 ("tires" ,+car-tires-$/km+)))
   (mode-extra-h :accessor extra-h :initform +parking-lot-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +average-L/km+)
   (energy-one :accessor energy-one :initform +MET-driving+)
   (energy-all :accessor energy-all :initform +MET-idle+)
   (seats :accessor seats :initform 5 :initarg :seats)
   (owned :accessor owned :initform nil :initarg :owned)))

(defclass airplane (transport-mode)
  ((km/h :accessor km/h :initform +airplane-km/h+)
   ($/ppj-km-costs :accessor $/ppj-km-costs :initform
                   `(("plane tickets" ,+airplane-$/km+)))
   (mode-extra-h :accessor extra-h :initform +airport-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +airplane-L/km+)
   (energy-all :accessor energy-all :initform +MET-idle+)))

(defclass via-rail (transport-mode)
  ((km/h :accessor km/h :initform +via-rail-km/h+)
   ($/ppj-km-costs :accessor $/ppj-km-costs :initform
                   `(("train tickets" ,+via-rail-$/km+)))
   (mode-extra-h :accessor extra-h :initform +via-rail-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +via-rail-L/km+)
   (energy-all :accessor energy-all :initform +MET-idle+)))

(defclass greyhound (transport-mode)
  ((km/h :accessor km/h :initform +greyhound-km/h+)
   ($/ppj-km-costs :accessor $/ppj-km-costs :initform
                   `(("bus tickets" ,+greyhound-$/km+)))
   (mode-extra-h :accessor extra-h :initform +greyhound-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +bus-L/km+)
   (energy-all :accessor energy-all :initform +MET-idle+)))

(defclass shuttle (transport-mode)
  ((km/h :accessor km/h :initform +shuttle-km/h+)
   ($/ppj-costs :accessor $/ppj-costs :initform
                `(("shuttle tickets" ,+shuttle-cost-$+)))
   (mode-extra-h :accessor extra-h :initform +shuttle-extra-h+)
   (fuel-L/km :accessor fuel-L/km :initform +bus-L/km+)
   (energy-all :accessor energy-all :initform +MET-idle+)))

(defclass red-arrow (greyhound) ; override greyhound settings
  ((km/h :accessor km/h :initform +red-arrow-km/h+)
   ($/ppj-km-costs :accessor $/ppj-km-costs :initform
                   `(("bus tickets" ,+red-arrow-$/km+)))
   (mode-extra-h :accessor extra-h :initform +red-arrow-extra-h+)))

(defclass rental (automobile) ; override automobile settings!
  (($/km-costs :accessor $/km-costs :initform
               `(("fines and fees" ,+car-related-fines-$/km+)))
   (mode-extra-h :accessor extra-h :initform
                 (+ +rental-car-extra-h+
                    +parking-lot-extra-h+))
   (rented :accessor rented :initform t)))

(defclass taxi (automobile) ; override automobile settings!
  (($/km-costs :accessor $/km-costs :initform nil)
   (mode-extra-h :accessor extra-h :initform +taxi-extra-h+)
   (seats :accessor seats :initform 4)))

(defclass carpool (automobile) ; override automobile settings!
  (($/km-costs :accessor $/km-costs :initform nil)
   (seats :accessor seats :initform 4)))