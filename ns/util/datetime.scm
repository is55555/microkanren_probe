;; util/datetime.scm
(import (scheme time))

(define (current-date)
(time-utc->date (current-time time-utc)))

(define (date->string d)
;; Format: 2025-04-11T15:32:45Z
(format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
        (date-year d) (date-month d) (date-day d)
        (date-hour d) (date-minute d) (date-second d)))
