(in-package :net.polyforms.lisp)

(defstruct location latitude longitude)

(defmethod print-object ((location location) stream)
  (if *print-escape*
      (format stream "#<location ~a,~a>"
              (location-latitude location)
              (location-longitude location))
    (format stream "~a,~a"
            (location-latitude location)
            (location-longitude location))))

(defmacro make-place-info-hash (place-info-list)
  `(let ((place-info-hash (make-hash-table :test 'equalp)))
     (loop for place-info in ,place-info-list do
           (setf (gethash (first place-info) place-info-hash)
                 (rest place-info)))
     place-info-hash))

(defvar *cities-hash*
  (make-place-info-hash
   ; --(place)--(latitude)--(longitude)--(address?)--
   `(("Victoria, BC" 48.428 -123.365)
     ("Vancouver, BC" 49.260 -123.114)
     ("North Vancouver, BC" 49.321 -123.074)
     ("Kamloops North, BC" 50.709 -120.365)
     ("Jasper, AB" 52.874 -118.083)
     ("Edmonton, AB" 53.546 -113.490)
     ("Calgary, AB" 51.045 -114.063)
     ("Red Deer, AB" 52.289 -113.812)
     ("Fort McMurray, AB" 56.716 -111.350)
     ("Banff, AB" 51.180 -115.577)
     ("Lake Louise, AB" 51.426 -116.178)
     ("Medicine Hat, AB" 50.041 -110.678)
     ("Saskatoon, SK" 52.131 -106.659)
     ("Winnipeg, MB" 49.899 -97.141)
     ("Toronto, ON" 43.649 -79.385)
     ("Kingston, ON" 44.232 -76.479)
     ("Ottawa, ON" 45.421 -75.692)
     ("Montreal, QC" 45.512 -73.554)
     ("Quebec, QC" 46.813 -71.219)
     ("Halifax, NS" 44.646 -63.574))))

(defvar *airports-hash*
  (make-place-info-hash
   ; --(place)--(latitude)--(longitude)--(address?)--
   `(("Abbotsford Airport, BC" 49.02219 -122.37888 "YXX")
     ("Calgary Airport, AB" 51.13052 -114.00888 "YYC")
     ("Charlottetown Airport, PE" 46.288677 -63.125755 "YYG")
     ("Comox Airport, BC" 49.70579 -124.90879 "YQQ")
     ("Deer Lake Airport, NL" 49.210835 -57.391388 "YDF")
     ("Edmonton Airport, AB" 53.30897 -113.58868 "YEG")
     ("Fort McMurray Airport, AB" 56.65278 -111.221664 "YMM")
     ("Grande Prairie Airport, AB" 55.182564 -118.89304 "YQU")
     ("Halifax Airport, NS" 44.883305 -63.509632 "YHZ")
     ("Hamilton Airport, ON" 43.163437 -79.92559 "YHM")
     ("Kamloops Airport, BC" 50.701389 -120.444444 "YKA")
     ("Kelowna Airport, BC" 49.953213 -119.37801 "YLW")
     ("Kitchener/Waterloo Airport, ON" 43.457714 -80.383064 "YKF")
     ("London Airport, ON" 43.028748 -81.14817 "YXU")
     ("Moncton Airport, NB" 46.111767 -64.68484 "YQM")
     ("Montreal Airport, QC" 45.45816 -73.74939 "YUL")
     ("Ottawa Airport, ON" 45.33083 -75.67198 "YOW")
     ("Prince George Airport, BC" 53.883606 -122.67487 "YXS")
     ("Québec City Airport, QC" 46.791111 -71.393333 "YQB")
     ("Regina Airport, SK" 50.43216 -104.66614 "YQR")
     ("Saint John Airport, NB" 45.31625 -65.88304 "YSJ")
     ("Saskation Airport, SK" 52.167892 -106.68809 "YXE")
     ("St John's Airport, NL" 47.618397 -52.73647 "YYT")
     ("Sydney Airport, NS" 46.161388 -60.048056 "YQY")
     ("Thunder Bay Airport, ON" 48.371944 -89.32389 "YQT")
     ("Toronto Airport, ON" 43.680847 -79.61159 "YYZ")
     ("Vancouver Airport, BC" 49.193558 -123.18098 "YVR")
     ("Victoria Airport, BC" 48.644245 -123.43019 "YYJ")
     ("Winnipeg International Airport, MB" 49.90361 -97.22687 "YWG")
     ("Yellowknife Airport, NT" 62.49322 -114.60528 "YKW"))))

(defun convert-place-info-hash (&optional (hash *cities-hash*))
  (loop for place being the hash-keys in hash using (hash-value place-info)
        collect (list place (make-location
                             :latitude (first place-info)
                             :longitude (second place-info)))))

(defmacro make-location-list (places-list &optional (hash *cities-hash*))
  `(loop for place in ,places-list
         collect (let ((place-info (gethash place ,hash)))
                   (list place (make-location
                                :latitude (first place-info)
                                :longitude (second place-info))))))
