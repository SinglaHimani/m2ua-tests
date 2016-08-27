;;; 
;;; Copyright (C) 2007 M. Tuexen tuexen@fh-muenster.de
;;; 
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the
;;; following conditions are met:
;;; 1. Redistributions of source code must retain the above
;;;    copyright notice, this list of conditions and the
;;;    following disclaimer.
;;; 2. Redistributions in binary form must reproduce the
;;;    above copyright notice, this list of conditions and
;;;    the following disclaimer in the documentation and/or
;;;    other materials provided with the distribution.
;;; 3. Neither the name of the project nor the names of
;;;    its contributors may be used to endorse or promote
;;;    products derived from this software without specific
;;;    prior written permission.
;;;  
;;; THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS
;;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
;;; OF SUCH DAMAGE.

;;; $Id: m2ua.scm,v 1.7 2008/04/06 23:48:02 tuexen Exp $

;;; Version 1.0.0
;;;
;;; History of changes:
;;; 22.11.2007 Initial version based on m3ua.scm

;;; This is the IANA registered PPID for M2UA in host byte order
(define m2ua-ppid                            2)

;;; This is the IANA registered port for M2UA
(define m2ua-port                         2904)

;;; Constants for the message classes
(define m2ua-mgmt-message-class              0)
(define m2ua-aspsm-message-class             3)
(define m2ua-asptm-message-class             4)
(define m2ua-maup-message-class              6)
(define m2ua-iim-message-class              10)
(define m2ua-reserved-message-class          2)

;;; Constants for the message types

;;; MGMT messages
(define m2ua-err-message-type                0)
(define m2ua-ntfy-message-type               1)
(define m2ua-reserved-mgmt-message-type      2)

;;; ASPSM messages
(define m2ua-aspup-message-type              1)
(define m2ua-aspdn-message-type              2)
(define m2ua-beat-message-type               3)
(define m2ua-aspup-ack-message-type          4)
(define m2ua-aspdn-ack-message-type          5)
(define m2ua-beat-ack-message-type           6)
(define m2ua-reserved-aspsm-message-type     7)

;;; ASPTM messages
(define m2ua-aspac-message-type              1)
(define m2ua-aspia-message-type              2)
(define m2ua-aspac-ack-message-type          3)
(define m2ua-aspia-ack-message-type          4)
(define m2ua-reserved-asptm-message-type     5)

;;; MAUP messages
(define m2ua-data-message-type               1)
(define m2ua-est-req-message-type            2)
(define m2ua-est-conf-message-type           3)
(define m2ua-rel-req-message-type            4)
(define m2ua-rel-conf-message-type           5)
(define m2ua-rel-ind-message-type            6)
(define m2ua-state-req-message-type          7)
(define m2ua-state-conf-message-type         8)
(define m2ua-state-ind-message-type          9)
(define m2ua-data-ret-req-message-type      10)
(define m2ua-data-ret-conf-message-type     11)
(define m2ua-data-ret-ind-message-type      12)
(define m2ua-data-ret-comp-ind-message-type 13)
(define m2ua-cong-indication-message-type   14)
(define m2ua-data-ack-message-type          15)
(define m2ua-reserved-maup-message-type     16)

;;; IIM messages
(define m2ua-reg-req-message-type            1)
(define m2ua-reg-rsp-message-type            2)
(define m2ua-dereg-req-message-type          3)
(define m2ua-dereg-rsp-message-type          4)
(define m2ua-reserved-iim-message-type       5)

;;; Constant for the protocol version
(define m2ua-version                         1)

;;; Constant for reserved
(define m2ua-spare                           0)

;;;
;;; Creator functions for messages
;;;

(define (m2ua-make-common-header version spare class type length)
  (append (uint8->bytes version)
	  (uint8->bytes spare)
	  (uint8->bytes class)
	  (uint8->bytes type)
	  (uint32->bytes length)))

;;;(m2ua-make-common-header 1 2 3 4 5)
;;;(m2ua-make-common-header m2ua-version m2ua-spare m2ua-mgmt-message-class m2ua-ntfy-message-type 16)

(define (m2ua-increment-version l)
  (if (positive? (length l))
      (cons (+ (car l) 1) (cdr l))
      (list)))
;;;(m2ua-increment-version (list 1 2 3))
;;;(m2ua-increment-version (list))

;;;
;;; Creator functions for parameters
;;;

(define m2ua-parameter-header-length 4)
(define m2ua-common-header-length 8)
(define m2ua-data-parameter-header-length 16)

(define (m2ua-number-of-padding-bytes l)
  (remainder (- 4 (remainder l 4)) 4))
;;; (m2ua-number-of-padding-bytes 0)
;;; (m2ua-number-of-padding-bytes 1)
;;; (m2ua-number-of-padding-bytes 2)
;;; (m2ua-number-of-padding-bytes 3)

(define (m2ua-add-padding l)
  (+ l (m2ua-number-of-padding-bytes l)))
;;; (m2ua-add-padding 2)

(define (m2ua-padding data)
  (zero-bytes (m2ua-number-of-padding-bytes (length data))))
;;;(m2ua-padding (list 1 2 3 4 5))

(define (m2ua-make-parameter tag value)
  (append (uint16->bytes tag)
	  (uint16->bytes (+ (length value) m2ua-parameter-header-length))
	  value
	  (m2ua-padding value)))

(define (m2ua-make-random-parameter l)
  (m2ua-make-parameter (random 2^16) (random-bytes l)))
;;;(m2ua-make-random-parameter 12)

(define (m2ua-add-parameter parameter list)
  (cons parameter (remove (lambda(p) (equal? (m2ua-get-parameter-tag p)
					     (m2ua-get-parameter-tag parameter)))
			  list)))

(define (m2ua-find-parameter tag parameter-list)
  (let ((l (filter (lambda(p) (equal? tag (m2ua-get-parameter-tag p)))
		   parameter-list)))
    (if (null? l)
	(list)
	(car l))))

(define (m2ua-make-message class type parameters)
  (append (m2ua-make-common-header m2ua-version
				   m2ua-spare
				   class
				   type
				   (+ m2ua-common-header-length (apply + (map length parameters))))
	  (apply append parameters)))

;;; Should here it interface parameter being added???

(define m2ua-integer-interface-identifier-tag       #x0001)
(define m2ua-text-interface-identifier-tag          #x0003)
(define m2ua-info-string-tag                        #x0004)
(define m2ua-diagnostic-info-tag                    #x0007)
(define m2ua-integer-range-interface-identifier-tag #x0008)
(define m2ua-heartbeat-data-tag                     #x0009)
(define m2ua-traffic-mode-type-tag                  #x000b)
(define m2ua-error-code-tag                         #x000c)
(define m2ua-status-tag                             #x000d)
(define m2ua-asp-identifier-tag                     #x0011)
(define m2ua-correlation-id-tag                     #x0013)

(define m2ua-protocol-data-1-tag                    #x0300)
(define m2ua-protocol-data-2-tag                    #x0301)
(define m2ua-state-request-tag                      #x0302)
(define m2ua-state-event-tag                        #x0303)
(define m2ua-congestion-status-tag                  #x0304)
(define m2ua-discard-status-tag                     #x0305)
(define m2ua-action-tag                             #x0306)
(define m2ua-sequence-number-tag                    #x0307)
(define m2ua-retrieval-result-tag                   #x0308)
(define m2ua-link-key-tag                           #x0309)
(define m2ua-local-lk-identifier-tag                #x030a)
(define m2ua-sdt-identifier-tag                     #x030b)
(define m2ua-sdl-identifier-tag                     #x030c)
(define m2ua-registration-result-tag                #x030d)
(define m2ua-registration-status-tag                #x030e)
(define m2ua-deregistration-result-tag              #x030f)
(define m2ua-deregistration-status-tag              #x0310)

(define (m2ua-make-integer-interface-identifier-parameter id)
  (m2ua-make-parameter m2ua-integer-interface-identifier-tag
		       (uint32->bytes id)))
;;; (m2ua-make-integer-interface-identifier-parameter 3)

(define (m2ua-make-text-interface-identifier-parameter id)
  (m2ua-make-parameter m2ua-text-interface-identifier-tag
		       (string->bytes id)))
;;; (m2ua-make-text-interface-identifier-parameter "3")

(define (m2ua-make-interface-identifier-parameter id)
  (if (integer? id)
      (m2ua-make-integer-interface-identifier-parameter id)
      (m2ua-make-text-interface-identifier-parameter id)))
;;; (m2ua-make-interface-identifier-parameter 12)
;;; (m2ua-make-interface-identifier-parameter "ABCDE")

(define (m2ua-make-info-string-parameter string)
  (m2ua-make-parameter m2ua-info-string-tag (string->bytes string)))
;;; (m2ua-make-info-string-parameter "Hello")

(define (m2ua-make-diagnostic-info-parameter info)
  (m2ua-make-parameter m2ua-diagnostic-info-tag info))
;;; (m2ua-make-diagnostic-info-parameter (list 1 2 3 4 5))

(define (m2ua-make-integer-range-interface-identifier-parameter id-range-list)
  (m2ua-make-parameter m2ua-integer-range-interface-identifier-tag
		       (apply append (map (lambda (x)
					    (append (uint32->bytes (car x))
						    (uint32->bytes (cadr x))))
					  id-range-list))))

;;; (m2ua-make-integer-range-interface-identifier-parameter (list (list 0 34) (list 255 389)))

(define (m2ua-make-heartbeat-data-parameter data)
  (m2ua-make-parameter m2ua-heartbeat-data-tag data))
;;; (m2ua-make-heartbeat-data-parameter (string->bytes "MUA rocks"))

(define m2ua-traffic-mode-type-override  1)
(define m2ua-traffic-mode-type-loadshare 2)
(define m2ua-traffic-mode-type-broadcast 3)
(define m2ua-traffic-mode-type-invalid   4)

(define (m2ua-make-traffic-mode-type-parameter mode)
  (m2ua-make-parameter m2ua-traffic-mode-type-tag (uint32->bytes mode)))
;;; (m2ua-make-traffic-mode-type-parameter m2ua-traffic-mode-type-override)

(define m2ua-invalid-version-error-code                       #x0001)
(define m2ua-invalid-interface-identifier-error-code          #x0002)
(define m2ua-unsupported-message-class-error-code             #x0003)
(define m2ua-unsupported-message-type-error-code              #x0004)
(define m2ua-unsupported-traffic-mode-type-error-code         #x0005)
(define m2ua-unexpected-message-error-code                    #x0006)
(define m2ua-protocol-error-error-code                        #x0007)
(define m2ua-unsupported-interface-identifier-type-error-code #x0008)
(define m2ua-invalid-stream-identifier-error-code             #x0009)
(define m2ua-refused-management-blocking-error-code           #x000d)
(define m2ua-asp-identifier-required-error-code               #x000e)
(define m2ua-invalid-asp-identifier-error-code                #x000f)
(define m2ua-asp-active-for-interface-identifiers-error-code  #x0010)
(define m2ua-invalid-parameter-value-error-code               #x0011)
(define m2ua-parameter-field-error-error-code                 #x0012)
(define m2ua-unexpected-parameter-error-code                  #x0013)
(define m2ua-missing-parameter-error-code                     #x0016)

(define (m2ua-make-error-code-parameter code)
  (m2ua-make-parameter m2ua-error-code-tag (uint32->bytes code)))
;;; (m2ua-make-error-code-parameter m2ua-protocol-error-error-code)

(define m2ua-as-state-change-status-type 1)
(define m2ua-other-status-type           2)

(define m2ua-as-inactive                 2)
(define m2ua-as-active                   3)
(define m2ua-as-pending                  4)

(define m2ua-insufficient-resources      1)
(define m2ua-alternate-asp-active        2)
(define m2ua-asp-failure                 3)

(define (m2ua-make-status-parameter type info)
  (m2ua-make-parameter m2ua-status-tag
		       (append (uint16->bytes type)
			       (uint16->bytes info))))
;;; (m2ua-make-status-parameter m2ua-other-status-type m2ua-asp-failure)

(define (m2ua-make-asp-id-parameter id)
  (m2ua-make-parameter m2ua-asp-identifier-tag (uint32->bytes id)))
;;; (m2ua-make-asp-id-parameter 1024)

(define (m2ua-make-correlation-id-parameter id)
  (m2ua-make-parameter m2ua-correlation-id-tag (uint32->bytes id)))
;;; (m2ua-make-correlation-id-parameter 3072)

(define (m2ua-make-protocol-data-1-parameter data)
  (m2ua-make-parameter m2ua-protocol-data-1-tag data))
;;; (m2ua-make-protocol-data-1-parameter (string->bytes "M2UA rocks"))

(define (m2ua-make-protocol-data-2-parameter data)
  (m2ua-make-parameter m2ua-protocol-data-2-tag data))
;;; (m2ua-make-protocol-data-2-parameter (string->bytes "M2UA rocks"))

(define m2ua-lpo-set-status       #x0000)
(define m2ua-lpo-clear-status     #x0001)
(define m2ua-emer-set-status      #x0002)
(define m2ua-emer-clear-status    #x0003)
(define m2ua-flush-buffers-status #x0004)
(define m2ua-continue-status      #x0005)
(define m2ua-clear-rtb-status     #x0006)
(define m2ua-audit-status         #x0007)
(define m2ua-cong-clear-status    #x0008)
(define m2ua-cong-accept-status   #x0009)
(define m2ua-cong-discard-status  #x000a)

(define (m2ua-make-state-request-parameter state)
  (m2ua-make-parameter m2ua-state-request-tag (uint32->bytes state)))
;;; (m2ua-make-state-request-parameter m2ua-lpo-clear-status)

(define m2ua-rpo-enter-event #x0001)
(define m2ua-rpo-exit-event  #x0002)
(define m2ua-lpo-enter-event #x0003)
(define m2ua-lpo-exit-event  #x0004)

(define (m2ua-make-state-event-parameter event)
  (m2ua-make-parameter m2ua-state-event-tag (uint32->bytes event)))
;;; (m2ua-make-state-event-parameter m2ua-lpo-exit-event)

(define m2ua-level-none #x0000)
(define m2ua-level-1    #x0001)
(define m2ua-level-2    #x0002)
(define m2ua-level-3    #x0003)

(define (m2ua-make-congestion-status-parameter status)
  (m2ua-make-parameter m2ua-congestion-status-tag (uint32->bytes status)))
;;; (m2ua-make-congestion-status-parameter m2ua-level-none)

(define (m2ua-make-discard-status-parameter status)
  (m2ua-make-parameter m2ua-discard-status-tag (uint32->bytes status)))
;;; (m2ua-make-discard-status-parameter m2ua-level-2)

(define m2ua-rtrv-bsn-action  #x0001)
(define m2ua-rtrv-msgs-action #x0002)

(define (m2ua-make-action-parameter action)
  (m2ua-make-parameter m2ua-action-tag (uint32->bytes action)))
;;; (m2ua-make-action-parameter m2ua-rtrv-msgs-action)

(define (m2ua-make-sequence-number-parameter number)
  (m2ua-make-parameter m2ua-sequence-number-tag (uint32->bytes number)))
;;; (m2ua-make-sequence-number-parameter 127)

(define m2ua-success-result #x0000)
(define m2ua-failure-result #x0001)

(define (m2ua-make-retrieval-result-parameter result)
  (m2ua-make-parameter m2ua-retrieval-result-tag (uint32->bytes result)))
;;; (m2ua-make-retrieval-result-parameter m2ua-failure-result)

(define (m2ua-make-link-key-parameter lk-id sdt-id sdl-id)
  (m2ua-make-parameter m2ua-link-key-tag 
		       (append (m2ua-make-local-lk-identifier-parameter lk-id)
			       (m2ua-make-sdt-identifier-parameter sdt-id)
			       (m2ua-make-sdl-identifier-parameter sdl-id))))
;;; (m2ua-make-link-key-parameter 1 2 3)

(define (m2ua-make-local-lk-identifier-parameter id)
  (m2ua-make-parameter m2ua-local-lk-identifier-tag (uint32->bytes id)))
;;; (m2ua-make-local-lk-identifier-parameter 111)

(define (m2ua-make-sdt-identifier-parameter id)
  (m2ua-make-parameter m2ua-sdt-identifier-tag 
		       (append (uint16->bytes 0)
			       (uint16->bytes id))))
;;; (m2ua-make-sdt-identifier-parameter 111)
;;; FIXME: Error in RFC.

(define (m2ua-make-sdl-identifier-parameter id)
  (m2ua-make-parameter m2ua-sdl-identifier-tag 
		       (append (uint16->bytes 0)
			       (uint16->bytes id))))
;;; (m2ua-make-sdl-identifier-parameter 111)
;;; FIXME: Error in RFC.

(define (m2ua-make-registration-result-parameter triple-list)
  (m2ua-make-parameter m2ua-registration-result-tag 
		       (apply append (map (lambda (triple)
					    (append
					     (m2ua-make-local-lk-identifier-parameter (car triple))
					     (m2ua-make-registration-status-parameter (cadr triple))
					     (m2ua-make-interface-identifier-parameter (caddr triple))))
					  triple-list))))
;;; (m2ua-make-registration-result-parameter (list (list 1 m2ua-unknown-error-reg-status 3)))
;;; (m2ua-make-registration-result-parameter (list (list 1 m2ua-unknown-error-reg-status "ABC")))

(define m2ua-successfully-registered-reg-status        0)
(define m2ua-unknown-error-reg-status                  1)
(define m2ua-invalid-sdli-error-reg-status             2)
(define m2ua-invalid-sdti-error-reg-status             3)
(define m2ua-invalid-link-key-error-reg-status         4)
(define m2ua-permission-denied-error-reg-status        5)
(define m2ua-overlapping-link-key-error-reg-status     6)
(define m2ua-link-key-not-provisioned-error-reg-status 7)
(define m2ua-insufficient-resources-error-reg-status   8)

(define (m2ua-make-registration-status-parameter status)
  (m2ua-make-parameter m2ua-registration-status-tag (uint32->bytes status)))
;;; (m2ua-make-registration-status-parameter m2ua-unknown-error-reg-status)

(define (m2ua-make-deregistration-result-parameter pair-list)
  (m2ua-make-parameter m2ua-deregistration-result-tag 
		       (apply append (map (lambda (pair)
					    (append
					     (m2ua-make-interface-identifier-parameter (car pair))
					     (m2ua-make-deregistration-status-parameter (cadr pair))))
					  pair-list))))
;;; (m2ua-make-deregistration-result-parameter (list (list 1 m2ua-unknown-error-dereg-status)))
;;; (m2ua-make-deregistration-result-parameter (list (list "ABC" m2ua-unknown-error-dereg-status)))

(define m2ua-successfully-deregistered-dereg-status          0)
(define m2ua-unknown-error-dereg-status                      1)
(define m2ua-invalid-interface-identifier-error-dereg-status 2)
(define m2ua-permission-denied-error-dereg-stat              3)
(define m2ua-not-registered-error-dereg-status               4)

(define (m2ua-make-deregistration-status-parameter status)
  (m2ua-make-parameter m2ua-deregistration-status-tag (uint32->bytes status)))
;;; (m2ua-make-registration-status-parameter m2ua-unknown-error-reg-status)



;;;------------------------------------------------------------------
;;; Message Contructors
;;;------------------------------------------------------------------

;;; MGMT messages

(define (m2ua-make-error-message code)
  (m2ua-make-message m2ua-mgmt-message-class
		     m2ua-err-message-type
		     (list (m2ua-make-error-code-parameter code))))
;;; (m2ua-make-error-message m2ua-no-configure-as-for-asp-error-code)

(define (m2ua-make-notify-message type info)
  (m2ua-make-message m2ua-mgmt-message-class
		     m2ua-ntfy-message-type
		     (list (m2ua-make-status-parameter type info))))
;;; (m2ua-make-notify-message m2ua-as-state-change-status-type m2ua-as-inactive)


;;; ASPSM messages

(define (m2ua-make-asp-up-message parameters)
  (m2ua-make-message m2ua-aspsm-message-class
		     m2ua-aspup-message-type
		     (m2ua-add-parameter (m2ua-make-info-string-parameter "M2UA rocks")
					 parameters)))
;;; (m2ua-make-asp-up-message (list))

(define (m2ua-make-asp-down-message)
  (m2ua-make-message m2ua-aspsm-message-class
		     m2ua-aspdn-message-type
		     (list (m2ua-make-info-string-parameter "M2UA rocks"))))
;;; (m2ua-make-asp-down-message)

(define (m2ua-make-beat-message data)
  (m2ua-make-message m2ua-aspsm-message-class 
		     m2ua-beat-message-type 
		     (list (m2ua-make-heartbeat-data-parameter data))))
;;; (m2ua-make-beat-message (string->bytes "M2UA rocks"))

(define (m2ua-make-asp-up-ack-message)
  (m2ua-make-message m2ua-aspsm-message-class
		     m2ua-aspup-ack-message-type
		     (list (m2ua-make-info-string-parameter "M2UA rocks"))))
;;; (m2ua-make-asp-up-ack-message)

(define (m2ua-make-asp-down-ack-message)
  (m2ua-make-message m2ua-aspsm-message-class
		     m2ua-aspdn-ack-message-type
		     (list (m2ua-make-info-string-parameter "M2UA rocks"))))
;;; (m2ua-make-asp-down-ack-message)

(define (m2ua-make-beat-ack-message data)
  (m2ua-make-message m2ua-aspsm-message-class
		     m2ua-beat-ack-message-type
		     (list (m2ua-make-heartbeat-data-parameter data))))
;;; (m2ua-make-beat-ack-message (string->bytes "M2UA rocks"))



;;;
;;; ASPTM messages
;;;


(define (m2ua-make-asp-active-message parameters)
  (m2ua-make-message m2ua-asptm-message-class
		     m2ua-aspac-message-type
		     (m2ua-add-parameter (m2ua-make-info-string-parameter "M2UA rocks")
					 parameters)))
;;; (m2ua-make-asp-active-message (list (m2ua-make-routing-context-parameter (list 3))))

(define (m2ua-make-asp-inactive-message parameters)
  (m2ua-make-message m2ua-asptm-message-class
		     m2ua-aspia-message-type
		     (m2ua-add-parameter (m2ua-make-info-string-parameter "M2UA rocks")
					 parameters)))
;;; (m2ua-make-asp-inactive-message (list))

(define (m2ua-make-asp-active-ack-message parameters)
  (m2ua-make-message m2ua-asptm-message-class
		     m2ua-aspac-ack-message-type
		     (m2ua-add-parameter (m2ua-make-info-string-parameter "M2UA rocks") 
					 parameters)))
;;; (m2ua-make-asp-active-ack-message (list))

(define (m2ua-make-asp-inactive-ack-message parameters)
  (m2ua-make-message m2ua-asptm-message-class
		     m2ua-aspia-ack-message-type
		     (m2ua-add-parameter (m2ua-make-info-string-parameter "M2UA rocks")
					 parameters)))
;;; (m2ua-make-asp-inactive-ack-message (list))



;;;
;;; MAUP messages
;;;

(define (m2ua-make-data-message interface-id payload . id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-message-type
		     (if (null? id)
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-protocol-data-1-parameter payload))
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-protocol-data-1-parameter payload)
			       (m2ua-make-correlation-id-parameter (car id))))))
			 
;;; (m2ua-make-data-message 2 (list 1 2 3))
;;; (m2ua-make-data-message 2 (list 1 2 3) 3)


(define (m2ua-make-establish-request-message interface-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-est-req-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-establish-request-message interface-id)


(define (m2ua-make-establish-confirmation-message interface-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-est-conf-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-establish-confirmation-message interface-id)


(define (m2ua-make-release-request-message interface-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-rel-req-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-release-request-message interface-id)


(define (m2ua-make-release-indication-message interface-id) 
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-rel-ind-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-release-indication-message interface-id)


(define (m2ua-make-release-confirmation-message interface-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-rel-conf-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-release-confirmation-message interface-id)


(define (m2ua-make-state-request-message interface-id status)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-state-req-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id) 
			   (m2ua-make-state-request-parameter status))))
;;; (m2ua-make-state-request-message interface-id m2ua-lpo-clear-status)


(define (m2ua-make-state-confirmation-message interface-id status)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-state-conf-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id)
			   (m2ua-make-state-request-parameter status))))
;;; (m2ua-make-state-confirmation-message interface-id m2ua-lpo-clear-status)


(define (m2ua-make-state-indication-message interface-id event)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-state-ind-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id)
			   (m2ua-make-state-event-parameter event))))
;;; (m2ua-make-state-indication-message interface-id m2ua-rpo-enter-event)


(define (m2ua-make-data-retrieval-request-message interface-id action . number)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-ret-req-message-type
		     (if (null? number)
			 (list (m2ua-make-interface-identifier-parameter interface-id) 
			       (m2ua-make-action-parameter action))
			 (list (m2ua-make-interface-identifier-parameter interface-id) 
			       (m2ua-make-action-parameter action)
			       (m2ua-make-sequence-number-parameter (car number))))))
;;; (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-bsn-action)
;;; (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-bsn-action 3)


(define (m2ua-make-data-retrieval-confirmation-message interface-id action result . number)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-ret-conf-message-type
		     (if (null? number)
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-action-parameter action)
			       (m2ua-make-retrieval-result-parameter result))
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-action-parameter action)
			       (m2ua-make-retrieval-result-parameter result)
			       (m2ua-make-sequence-number-parameter (car number))))))
;;; (m2ua-make-data-retrieval-confirmation-message interface-id m2ua-rtrv-bsn-action m2ua-success-result)
;;; (m2ua-make-data-retrieval-confirmation-message interface-id m2ua-rtrv-bsn-action m2ua-success-result 3)


(define (m2ua-make-data-retrieval-indication-message interface-id payload)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-ret-ind-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id)
			   (m2ua-make-protocol-data-1-parameter payload))))
;;; (m2ua-make-data-retrieval-indication-message interface-id (list 1 2 3))


(define (m2ua-make-data-retrieval-complete-indication-message interface-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-ret-comp-ind-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id))))
;;; (m2ua-make-data-retrieval-complete-indication-message interface-id)


(define (m2ua-make-congestion-indication-message interface-id cong-status . discard-status)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-cong-indication-message-type
		     (if (null? discard-status)
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-congestion-status-parameter cong-status))
			 (list (m2ua-make-interface-identifier-parameter interface-id)
			       (m2ua-make-congestion-status-parameter cong-status)
			       (m2ua-make-discard-status-parameter (car discard-status))))))
;;; (m2ua-make-congestion-indication-message interface-id m2ua-level-1)
;;; (m2ua-make-congestion-indication-message interface-id m2ua-level-1 m2ua-level-2)


(define (m2ua-make-data-ack-message interface-id correlation-id)
  (m2ua-make-message m2ua-maup-message-class
		     m2ua-data-ack-message-type
		     (list (m2ua-make-interface-identifier-parameter interface-id)
			   (m2ua-make-correlation-id-parameter correlation-id))))
;;; (m2ua-make-data-ack-message interface-id 4)

;;;
;;; IIM messages
;;;

(define (m2ua-make-reg-req-message lk-id sdt-id sdl-id)
  (m2ua-make-message m2ua-iim-message-class
		     m2ua-reg-req-message-type
		     (list (m2ua-make-link-key-parameter lk-id sdt-id sdl-id))))
;;; (m2ua-make-reg-req-message link-key-id sdt-id sdl-id)

(define (m2ua-make-reg-rsp-message lk-id status interface-id)
  (m2ua-make-message m2ua-iim-message-class
		     m2ua-reg-rsp-message-type
		     (list (m2ua-make-registration-result-parameter
			    (list (list lk-id status interface-id))))))
;;; (m2ua-make-reg-rsp-message 1 2 3)

(define (m2ua-get-interface-id-from-reg-rsp message)
  (m2ua-get-integer-interface-identifier
   (m2ua-find-parameter m2ua-integer-interface-identifier-tag
			(m2ua-get-parameters-of-parameter
			 (car (m2ua-get-parameters-of-message message))))))
;;; (m2ua-get-interface-id-from-reg-rsp (m2ua-make-reg-rsp-message 1 2 5))

(define (m2ua-make-dereg-req-message id)
  (m2ua-make-message m2ua-iim-message-class
		     m2ua-dereg-req-message-type
		     (list (m2ua-make-interface-identifier-parameter id))))
;;; (m2ua-make-dereg-req-message interface-id)


;;;
;;; General accessor functions for messages
;;;

(define (m2ua-get-common-header l)
  (list-head l m2ua-common-header-length))
;;; (m2ua-get-common-header (m2ua-make-asp-up-message (list)))

(define m2ua-version-offset        0)
(define m2ua-spare-offset          1)
(define m2ua-message-class-offset  2)
(define m2ua-message-type-offset   3)
(define m2ua-message-length-offset 4)

(define (m2ua-get-version l)
  (bytes->uint8 (list-tail l m2ua-version-offset)))

;;;(define hb (m2ua-make-beat-message (string->bytes "M2UA rocks")))
;;;(m2ua-get-version hb)

(define (m2ua-get-spare l)
  (bytes->uint8 (list-tail l m2ua-reserved-offset)))
;;;(m2ua-get-reserved hb)

(define (m2ua-get-message-class l)
  (bytes->uint8 (list-tail l m2ua-message-class-offset)))
;;;(m2ua-get-message-class hb)

(define (m2ua-get-message-type l)
  (bytes->uint8 (list-tail l m2ua-message-type-offset)))
;;;(m2ua-get-message-type hb)

(define (m2ua-get-message-length l)
  (bytes->uint32 (list-tail l m2ua-message-length-offset)))
;;;(m2ua-get-message-length hb)

(define (m2ua-get-parameters-1 l)
  (if (>= (length l) m2ua-parameter-header-length)
      (let ((parameter-length (m2ua-add-padding (m2ua-get-parameter-length l))))
	(cons (list-head l parameter-length)
	      (m2ua-get-parameters-1 (list-tail l parameter-length))))
      (list)))

(define (m2ua-get-parameters-of-message l)
  (if (>= (length l) m2ua-common-header-length)
      (m2ua-get-parameters-1 (list-tail l m2ua-common-header-length))
      (list)))
;;; (m2ua-get-parameters-of-message (m2ua-make-beat-message (string->bytes "M2UA rocks")))
;;; (m2ua-get-parameters-of-message (list 2 2))

(define m2ua-get-parameters m2ua-get-parameters-of-message)

(define (m2ua-get-parameters-of-parameter l)
  (if (>= (length l) m2ua-common-header-length)
      (m2ua-get-parameters-1 (list-tail l m2ua-parameter-header-length))
      (list)))

;;;
;;; General accessor function for parameters
;;;

(define m2ua-parameter-tag-offset    0)
(define m2ua-parameter-length-offset 2)
(define m2ua-parameter-value-offset  4)

(define (m2ua-get-parameter-tag l)
  (bytes->uint16 (list-tail l m2ua-parameter-tag-offset)))
;;; (m2ua-get-parameter-tag (m2ua-make-parameter 1 (list 1 2 3)))

(define (m2ua-get-parameter-length l)
  (bytes->uint16 (list-tail l m2ua-parameter-length-offset)))
;;; (m2ua-get-parameter-length (m2ua-make-parameter 1 (list 1 2 3)))

(define (m2ua-get-parameter-value l)
  (list-tail (list-head l (m2ua-get-parameter-length l))  m2ua-parameter-value-offset))
;;; (m2ua-get-parameter-value (m2ua-make-parameter 1 (list 1 2 3)))

(define (m2ua-get-parameter-padding l)
  (list-tail l (m2ua-get-parameter-length l)))
;;; (m2ua-get-parameter-padding (m2ua-make-parameter 1 (list  1 2 3)))

(define (m2ua-get-integer-interface-identifier l)
  (bytes->uint32 (list-tail l m2ua-parameter-value-offset)))
;;; (m2ua-get-integer-interface-identifier (m2ua-make-integer-interface-identifier-parameter 4))

;;;
;;;  M2UA helper routines
;;;

(define m2ua-maximum-message-length (expt 2 16))

(define (m2ua-connect local-addr local-port remote-addr remote-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (connect s AF_INET (inet-aton remote-addr) remote-port)
	     (if (defined? 'SCTP_NODELAY)
		 (setsockopt s IPPROTO_SCTP SCTP_NODELAY 1))
	     s)
	   (lambda (key . args)
	     (close s)))))

;;; (m2ua-connect "127.0.0.1" 0 "127.0.0.1" m2ua-port)

(define (m2ua-bulk-transfer local-addr local-port remote-addr remote-port message number)
  (let ((s (m2ua-connect local-addr local-port remote-addr remote-port)))
    (dotimes (i number)
      (m2ua-send-message s 0 message))
    (close s)))
;;; (m2ua-bulk-transfer "0.0.0.0" 0 "127.0.0.1" 5001 (m2ua-make-beat-message (random-bytes 200)) 100000)

(define (m2ua-accept local-addr local-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error  
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (listen s 1)
	     (let ((ss (car (accept s))))
	       (close s)
	       (if (defined? 'SCTP_NODELAY)
		   (setsockopt ss IPPROTO_SCTP SCTP_NODELAY 1))
	       ss))
	   (lambda (key . args)
	     (close s)))))
    

;;;(m2ua-accept "127.0.0.1" m2ua-port)

(define (m2ua-send-message socket stream message)
  (catch 'system-error
	 (lambda()
	   (sctp-sendmsg socket (bytes->string message) (htonl m2ua-ppid) stream 0 0 AF_INET INADDR_ANY 0))
	 (lambda (key . args)
	   0)))

(define (m2ua-recv-message socket)
  (let ((buffer (make-string m2ua-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((n (recv! socket buffer)))
	       (string->bytes (substring buffer 0 n))))
	   (lambda (key . args)
	     (list)))))

;;; (m2ua-recv-message s)

(define (m2ua-wait-for-message socket predicate)
  (let ((m (m2ua-recv-message socket)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m2ua-wait-for-message socket predicate))))

(define (m2ua-version-ok? version)
  (= version m2ua-version))
;;; (m2ua-version-ok? m2ua-version)
;;; (m2ua-version-ok? (+ m2ua-version 1))

(define (m2ua-message-class-ok? class)
  (or (= class m2ua-mgmt-message-class)
      (= class m2ua-aspsm-message-class)
      (= class m2ua-asptm-message-class)
      (= class m2ua-maup-message-class)
      (= class m2ua-iim-message-class)))
;;; (m2ua-message-class-ok? m2ua-mgmt-message-class)
;;; (m2ua-message-class-ok? m2ua-reserved-message-class)

(define (m2ua-message-type-ok? class type)
  (cond
    ((= class m2ua-mgmt-message-class)
     (or (= type m2ua-err-message-type)
	 (= type m2ua-ntfy-message-type)))
    ((= class m2ua-aspsm-message-class)
     (or (= type m2ua-aspup-message-type)
	 (= type m2ua-aspdn-message-type)
	 (= type m2ua-beat-message-type)
	 (= type m2ua-aspup-ack-message-type)
	 (= type m2ua-aspdn-ack-message-type)
	 (= type m2ua-beat-ack-message-type)))
    ((= class m2ua-asptm-message-class)
     (or (= type m2ua-aspac-message-type)
	 (= type m2ua-aspia-message-type)
	 (= type m2ua-aspac-ack-message-type)
	 (= type m2ua-aspia-ack-message-type)))
    ((= class m2ua-maup-message-class)
     (or (= type m2ua-data-message-type)
	 (= type m2ua-est-req-message-type)
	 (= type m2ua-est-conf-message-type)
	 (= type m2ua-rel-req-message-type)
	 (= type m2ua-rel-conf-message-type)
	 (= type m2ua-rel-ind-message-type)
	 (= type m2ua-state-req-message-type)
	 (= type m2ua-state-conf-message-type)
	 (= type m2ua-state-ind-message-type)
	 (= type m2ua-data-ret-req-message-type)
	 (= type m2ua-data-ret-conf-message-type)
	 (= type m2ua-data-ret-ind-message-type)
	 (= type m2ua-data-ret-comp-ind-message-type)
	 (= type m2ua-cong-indication-message-type)
	 (= type m2ua-data-ack-message-type)))

    ((= class m2ua-iim-message-class)
     (or (= type m2ua-reg-req-message-type)
	 (= type m2ua-reg-rsp-message-type)
	 (= type m2ua-dereg-req-message-type)
	 (= type m2ua-dereg-rsp-message-type)))))

;;; (m2ua-message-type-ok? m2ua-mgmt-message-class m2ua-err-message-type)
;;; (m2ua-message-type-ok? m2ua-mgmt-message-class m2ua-reserved-mgmt-message-type)
;;; (m2ua-message-type-ok? m2ua-aspsm-message-class m2ua-aspup-message-type)
;;; (m2ua-message-type-ok? m2ua-aspsm-message-class m2ua-reserved-aspsm-message-type)
;;; (m2ua-message-type-ok? m2ua-asptm-message-class m2ua-aspac-message-type)
;;; (m2ua-message-type-ok? m2ua-asptm-message-class m2ua-reserved-asptm-message-type)
;;; (m2ua-message-type-ok? m2ua-maup-message-class m2ua-data-message-type)
;;; (m2ua-message-type-ok? m2ua-maup-message-class m2ua-reserved-maup-message-type)
;;; (m2ua-message-type-ok? m2ua-iim-message-class m2ua-reg-req-message-type)
;;; (m2ua-message-type-ok? m2ua-iim-message-class m2ua-reserved-iim-message-type)


(define (m2ua-check-common-header fd message)
  (if (not (m2ua-version-ok? (m2ua-get-version message)))
      (begin
	(m2ua-send-message fd 0 (m2ua-make-error-message m2ua-invalid-version-error-code))
	#f)
      (if (not (m2ua-message-class-ok? (m2ua-get-message-class message)))
	  (begin
	    (m2ua-send-message fd 0 (m2ua-make-error-message m2ua-unsupported-message-class-error-code))
	    #f)
	  (if (not (m2ua-message-type-ok? (m2ua-get-message-class message)
					  (m2ua-get-message-type message)))
	      (begin
		(m2ua-send-message fd 0 (m2ua-make-error-message m2ua-unsupported-message-type-error-code))
		#f)
	      #t))))

(define (m2ua-data-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-message-type)))
;;; (m2ua-data-message? (m2ua-make-data-message 1 2 3 4 5 6 (list 1 2) (list)))
;;; (m2ua-data-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-error-message? message)
  (and (= (m2ua-get-message-class message) m2ua-mgmt-message-class)
       (= (m2ua-get-message-type message)  m2ua-err-message-type)))
;;; (m2ua-error-message? (m2ua-make-error-message m2ua-unexpected-message-error-code))
;;; (m2ua-error-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-notify-message? message)
  (and (= (m2ua-get-message-class message) m2ua-mgmt-message-class)
       (= (m2ua-get-message-type message)  m2ua-ntfy-message-type)))
;;; (m2ua-notify-message? (m2ua-make-notify-message m2ua-as-state-change-status-type m2ua-as-inactive))
;;; (m2ua-notify-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-beat-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-beat-message-type)))
;;; (m2ua-beat-message? (m2ua-make-beat-message (list 1 2 3)))
;;; (m2ua-beat-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-beat-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-beat-ack-message-type)))
;;; (m2ua-beat-ack-message? (m2ua-make-beat-ack-message (list 1 2 3)))
;;; (m2ua-beat-ack-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-asp-up-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspup-message-type)))
;;; (m2ua-asp-up-message? (m2ua-make-asp-up-message (list)))
;;; (m2ua-asp-up-message? (m2ua-make-asp-down-message))

(define (m2ua-asp-up-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspup-ack-message-type)))
;;; (m2ua-asp-up-ack-message? (m2ua-make-asp-up-ack-message))
;;; (m2ua-asp-up-ack-message? (m2ua-make-asp-down-message))

(define (m2ua-asp-active-message? message)
  (and (= (m2ua-get-message-class message) m2ua-asptm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspac-message-type)))
;;; (m2ua-asp-active-message? (m2ua-make-asp-active-message (list)))
;;; (m2ua-asp-active-message? (m2ua-make-asp-down-message))

(define (m2ua-asp-active-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-asptm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspac-ack-message-type)))
;;; (m2ua-asp-active-ack-message? (m2ua-make-asp-active-ack-message (list)))
;;; (m2ua-asp-active-ack-message? (m2ua-make-asp-down-message))

(define (m2ua-asp-down-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspdn-message-type)))
;;; (m2ua-asp-down-message? (m2ua-make-asp-down-message))
;;; (m2ua-asp-down-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-asp-down-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-aspsm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspdn-ack-message-type)))
;;; (m2ua-asp-down-ack-message? (m2ua-make-asp-down-ack-message))
;;; (m2ua-asp-down-ack-message? (m2ua-make-asp-up-message (list)))

(define (m2ua-asp-inactive-message? message)
  (and (= (m2ua-get-message-class message) m2ua-asptm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspia-message-type)))
;;; (m2ua-asp-inactive-message? (m2ua-make-asp-inactive-message (list)))
;;; (m2ua-asp-inactive-message? (m2ua-make-asp-down-message))

(define (m2ua-asp-inactive-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-asptm-message-class)
       (= (m2ua-get-message-type message)  m2ua-aspia-ack-message-type)))
;;; (m2ua-asp-inactive-ack-message? (m2ua-make-asp-inactive-ack-message (list)))
;;; (m2ua-asp-inactive-ack-message? (m2ua-make-asp-down-message))

(define (m2ua-data-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-message-type)))
;;; (m2ua-data-message? (m2ua-make-asp-inactive-ack-message (list)))
;;; (m2ua-data-message? (m2ua-make-asp-down-message))

(define (m2ua-establish-request-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-est-req-message-type)))
;;; (m2ua-establish-request-message? (m2ua-make-establish-request-message))
;;; (m2ua-establish-request-message? (m2ua-make-asp-down-message))

(define (m2ua-establish-confirmation-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-est-conf-message-type)))
;;; (m2ua-establish-confirmation-message? (m2ua-make-establish-confirmation-message))
;;; (m2ua-establish-confirmation-message? (m2ua-make-asp-down-message))

(define (m2ua-release-request-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-rel-req-message-type)))
;;; (m2ua-release-request-message? (m2ua-make-release-request-message))
;;; (m2ua-release-request-message? (m2ua-make-asp-down-message))

(define (m2ua-release-request-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-rel-req-message-type)))
;;; (m2ua-release-request-message? (m2ua-make-release-request-message))
;;; (m2ua-release-request-message? (m2ua-make-asp-down-message))

(define (m2ua-release-indication-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-rel-ind-message-type)))
;;; (m2ua-release-indication-message? (m2ua-make-release-indication-message))
;;; (m2ua-release-indication-message? (m2ua-make-asp-down-message))

(define (m2ua-release-confirmation-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-rel-conf-message-type)))
;;; (m2ua-release-confirmation-message? (m2ua-make-release-confirmation-message))
;;; (m2ua-release-confirmation-message? (m2ua-make-asp-down-message))

(define (m2ua-state-request-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-state-req-message-type)))
;;; (m2ua-state-request-message? (m2ua-make-state-request-message m2ua-lpo-set-status))
;;; (m2ua-state-request-message? (m2ua-make-asp-down-message))

(define (m2ua-state-confirmation-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-state-conf-message-type)))
;;; (m2ua-state-confirmation-message? (m2ua-make-state-confirmation-message m2ua-lpo-set-status))
;;; (m2ua-state-confirmation-message? (m2ua-make-asp-down-message))

(define (m2ua-state-indication-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-state-ind-message-type)))
;;; (m2ua-state-indication-message? (m2ua-make-state-indication-message m2ua-rpo-enter-event))
;;; (m2ua-state-indication-message? (m2ua-make-asp-down-message))

(define (m2ua-data-retrieval-request-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-ret-req-message-type)))
;;; (m2ua-data-retrieval-request-message? (m2ua-make-data-retrieval-request-message m2ua-rtrv-bsn-action))
;;; (m2ua-data-retrieval-request-message? (m2ua-make-asp-down-message))

(define (m2ua-data-retrieval-confirmation-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-ret-conf-message-type)))
;;; (m2ua-data-retrieval-confirmation-message? (m2ua-make-data-retrieval-confirmation-message m2ua-rtrv-bsn-action m2ua-success-result))
;;; (m2ua-data-retrieval-confirmation-message? (m2ua-make-asp-down-message))

(define (m2ua-data-retrieval-indication-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-ret-ind-message-type)))
;;; (m2ua-data-retrieval-indication-message? (m2ua-make-data-retrieval-indication-message (list 1 2 3)))
;;; (m2ua-data-retrieval-indication-message? (m2ua-make-asp-down-message))

(define (m2ua-data-retrieval-complete-indication-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-ret-comp-ind-message-type)))
;;; (m2ua-data-retrieval-complete-indication-message? (m2ua-make-data-retrieval-complete-indication-message))
;;; (m2ua-data-retrieval-complete-indication-message? (m2ua-make-asp-down-message))

(define (m2ua-congestion-indication-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-cong-indication-message-type)))
;;; (m2ua-congestion-indication-message? (m2ua-make-data-retrieval-complete-indication-message))
;;; (m2ua-congestion-indication-message? (m2ua-make-asp-down-message))

(define (m2ua-data-ack-message? message)
  (and (= (m2ua-get-message-class message) m2ua-maup-message-class)
       (= (m2ua-get-message-type message)  m2ua-data-ack-message-type)))
;;; (m2ua-data-ack-message? (m2ua-make-data-ack-message 1 3))
;;; (m2ua-data-ack-message? (m2ua-make-asp-down-message))

(define (m2ua-reg-req-message? message)
  (and (= (m2ua-get-message-class message) m2ua-iim-message-class)
       (= (m2ua-get-message-type message)  m2ua-reg-req-message-type)))
;;; (m2ua-reg-req-message? (m2ua-make-data-ack-message 1 3))
;;; (m2ua-reg-req-message? (m2ua-make-reg-req-message 1 2 3))

(define (m2ua-reg-rsp-message? message)
  (and (= (m2ua-get-message-class message) m2ua-iim-message-class)
       (= (m2ua-get-message-type message)  m2ua-reg-rsp-message-type)))
;;; (m2ua-reg-rsp-message? (m2ua-make-data-ack-message 1 3))
;;; (m2ua-reg-rsp-message? (m2ua-make-asp-down-message))

(define (m2ua-dereg-req-message? message)
  (and (= (m2ua-get-message-class message) m2ua-iim-message-class)
       (= (m2ua-get-message-type message)  m2ua-dereg-req-message-type)))
;;; (m2ua-dereg-req-message? (m2ua-make-data-ack-message 1 3))
;;; (m2ua-dereg-req-message? (m2ua-make-dereg-req-message 1))

(define (m2ua-dereg-rsp-message? message)
  (and (= (m2ua-get-message-class message) m2ua-iim-message-class)
       (= (m2ua-get-message-type message)  m2ua-dereg-rsp-message-type)))
;;; (m2ua-dereg-rsp-message? (m2ua-make-data-ack-message 1 3))
;;; (m2ua-dereg-rsp-message? (m2ua-make-asp-down-message))



(define m2ua-asp-down           0)
(define m2ua-asp-inactive       1)
(define m2ua-asp-active         2)
(define m2ua-asp-reflect-beat   3)
(define m2ua-asp-send-data      4)
(define m2ua-asp-receive-data   5)
(define m2ua-asp-send-reg-req   6)
(define m2ua-asp-send-dereg-req 7)

(define (m2ua-handle-sgp-message fd state)
  (let ((message (m2ua-recv-message fd)))
    (if (positive? (length message))
	(if (m2ua-check-common-header fd message)
	    (cond 
	     ((m2ua-beat-message? message)
	      (m2ua-send-message fd 0 (m2ua-make-message m2ua-aspsm-message-class
							 m2ua-beat-ack-message-type
							 (m2ua-get-parameters message)))
	      (m2ua-handle-sgp-message fd state))

	     ((m2ua-asp-up-message? message)
	      (if (= state m2ua-asp-active)
		  (m2ua-send-message fd 0 (m2ua-make-error-message m2ua-unexpected-message-error-code)))
	      (m2ua-send-message fd 0 (m2ua-make-asp-up-ack-message))
	      (if (not (= state m2ua-asp-inactive))
		  (m2ua-send-message fd 0 (m2ua-make-notify-message m2ua-as-state-change-status-type
								    m2ua-as-inactive)))
	      (m2ua-handle-sgp-message fd m2ua-asp-inactive))
	     
	     ((m2ua-asp-active-message? message)
	      (if (= state m2ua-asp-down)
		  (begin
		    (m2ua-send-message fd 0 (m2ua-make-error-message m2ua-unexpected-message-error-code))
		    (m2ua-handle-sgp-message fd m2ua-asp-down))
		  (begin
		    (m2ua-send-message fd 0 (m2ua-make-asp-active-ack-message (m2ua-get-parameters message)))
		    (if (not (= state m2ua-asp-active))
			(m2ua-send-message fd 0 (m2ua-make-notify-message m2ua-as-state-change-status-type
									  m2ua-as-active)))
		    (m2ua-handle-sgp-message fd m2ua-asp-active))))
	     
	     ((m2ua-asp-down-message? message)
	      (m2ua-send-message fd 0 (m2ua-make-asp-down-ack-message))
	      (m2ua-handle-sgp-message fd m2ua-asp-down))
	     
	     ((m2ua-asp-inactive-message? message)
	      (if (= state m2ua-asp-down)
		  (begin
		    (m2ua-send-message fd 0 (m2ua-make-asp-down-ack-message))
		    (m2ua-handle-sgp-message fd m2ua-asp-down))
		  (begin
		    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-ack-message (list)))
		    (if (= state m2ua-asp-active)
			(m2ua-send-message fd 0 (m2ua-make-notify-message m2ua-as-state-change-status-type
									  m2ua-as-pending)))
		    (m2ua-handle-sgp-message fd m2ua-asp-inactive))))
	     
	     ((m2ua-data-message? message)
	      (m2ua-handle-sgp-message fd state))

	     ((m2ua-establish-request-message? message)
	      (m2ua-send-message fd 1 (m2ua-make-establish-confirmation-message interface-id))
	      (m2ua-handle-sgp-message fd state))

	     ((m2ua-release-request-message? message)
	      (m2ua-send-message fd 1 (m2ua-make-release-confirmation-message interface-id))
	      (m2ua-handle-sgp-message fd state))

	     ((m2ua-state-request-message? message)
	      (m2ua-send-message fd 1 (m2ua-make-message m2ua-maup-message-class
							 m2ua-state-conf-message-type
							 (list (m2ua-find-parameter m2ua-integer-interface-identifier-tag
										    (m2ua-get-parameters message))
							       (m2ua-find-parameter m2ua-state-request-tag
										    (m2ua-get-parameters message)))))
	      (m2ua-handle-sgp-message fd state))

	     ((m2ua-data-retrieval-request-message? message)
	      (m2ua-send-message fd 1 (m2ua-make-message m2ua-maup-message-class
							 m2ua-data-ret-conf-message-type
							 (list (m2ua-find-parameter m2ua-integer-interface-identifier-tag
										    (m2ua-get-parameters message))
							       (m2ua-find-parameter m2ua-action-tag
										    (m2ua-get-parameters message))
							       (m2ua-make-retrieval-result-parameter m2ua-success-result))))
	      (m2ua-handle-sgp-message fd state))

	     (else
	      (m2ua-send-message fd 0 (m2ua-make-error-message m2ua-unexpected-message-error-code))
	      (m2ua-handle-sgp-message fd state)))))))

(define (m2ua-run-sgp port)
  (let ((fd (m2ua-accept "0.0.0.0" port)))
    (m2ua-handle-sgp-message fd m2ua-asp-down)
    (close fd)))
;;; (m2ua-run-sgp m2ua-port)
