(in-package :net.polyforms.lisp)

(defvar *default-key* "ABQIAAAARtot_JHl_j9_TeZg6VGd0RTd8zkWFBdqS3wdRKEdVdHbWTz0oxR4RdYYgtOvtrS1-RlAy8E1XOXCMQ")

; Getting locations from Google
(defun geocode (&optional (fmt? t) &key q (key *default-key*) (output :csv))
  (when (or (null q) (null key))
    (error "You must specify an address and a Google Maps API key."))
  (if fmt? (format t "~&Sending query to Google... "))
  (setq output
        (case output
          ((:csv :xml :kml :json) (string-downcase (symbol-name output)))
          (t (error "Bad :output keyword value: ~s." output))))
  (let ((query (query-to-form-urlencoded
                `(("q" . ,q) ("output" . ,output) ("key" . ,key))))
        (url-base "http://maps.google.com/maps/geo"))
    (values (s-http-client:do-http-request
             (format nil "~a?~a" url-base query)))))

(defun place-to-location (place &optional (fmt? t) &key (key *default-key*))
  (let ((result (geocode fmt? :q place :key key :output :csv)))
    ;; Using :csv, a good result starts with "200" and a bad result with "602".
    (when (and result (stringp result))
      (multiple-value-bind (lat long)
          (match-re "200,[^,]+,(-?[0-9.]+),(-?[0-9.]+)" result)
        (when (and lat long)
          (make-location
           :latitude (read-from-string lat)
           :longitude (read-from-string long)))))))
(memoize 'place-to-location :key #'first :test #'equalp)

; Getting elevations from GeoNames
(defun get-elevation (location &optional (fmt? t))
  (when (null location)
    (error "You must specify a location."))
  (if fmt? (format t "~&Sending query to GeoNames... "))
  (let* ((url-base "http://ws.geonames.org/srtm3")
         (query (query-to-form-urlencoded
                 `(("lat" . ,(format nil "~a" (location-latitude location)))
                   ("lng" . ,(format nil "~a" (location-longitude location))))))
         (result (values (s-http-client:do-http-request
                          (format nil "~a?~a" url-base query)))))
    (when (and result (stringp result))
      (let ((new-result (read-from-string result)))
        (max new-result 0)))))
(memoize 'get-elevation :key #'first :test #'equalp)
