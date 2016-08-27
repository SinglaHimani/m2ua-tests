;;; 
;;; Copyright (C) 2007, 2008 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: m2ua-sgp-tests.scm,v 1.7 2008/04/06 23:48:02 tuexen Exp $

;;; Version 1.0.2
;;;
;;; History
;;;
;;; 06/04/2008: Fix m2ua-sgp-maup-v-14 as reported by Mr. Pason.
;;; 06/04/2008: Rename m2ua-sgp-geh-6 to m2ua-sgp-geh-6-1.
;;; 06/04/2008: Add m2ua-sgp-geh-6-2.
;;; 06/04/2008: Implement m2ua-sgp-iim-v0[1-5], m2ua-sgp-iim-io-01

;;;
;;; Definition of the tests for the SGP
;;;

(define (m2ua-sgp-aspm-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPUP-ACK and and an NTFY is returned.



(define (m2ua-sgp-aspm-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPUP-ACK and and two NTFYs are returned.



(define (m2ua-sgp-aspm-v-03 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-aspm-v-03 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
;;; The test is passed if an ASPUP-ACK and and an NTFY is returned on the first
;;; association, nothing at the point on time on the second. After sending the
;;; ASP-UP on the second association there should only be an ASP-UP-ACK on the
;;; second association.



(define (m2ua-sgp-aspm-v-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    ;;; the m2ua-asp-active-parameters are not considered here...
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message
			     (list (m2ua-make-integer-interface-identifier-parameter interface-id))))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-04 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPAC and and an NTFY is returned.



(define (m2ua-sgp-aspm-v-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    ;;; the m2ua-asp-active-parameters are not considered here...
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message
			     (list (m2ua-make-integer-interface-identifier-parameter interface-id-1)
				   (m2ua-make-integer-interface-identifier-parameter interface-id-2))))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-05 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPAC and and an NTFY is returned.



(define (m2ua-sgp-aspm-v-06 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    ;;; the m2ua-asp-active-parameters are not considered here...
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message
			     (list (m2ua-make-integer-range-interface-identifier-parameter
				    (list (list interface-id-start interface-id-stop))))))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-06 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPAC and and an NTFY is returned.



(define (m2ua-sgp-aspm-v-07 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-inactive-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-07 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPIAC and and an NTFY is returned.



(define (m2ua-sgp-aspm-v-08 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (sleep 1)
    (close fd)))
;;; (m2ua-sgp-aspm-v-08 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if one or two  ASPIAC and and one or two NTFY is returned.



(define (m2ua-sgp-aspm-v-09 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message (m2ua-add-parameter
							   (m2ua-make-interface-identifier-parameter
							    interface-id-1)
							   asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message (m2ua-add-parameter
							   (m2ua-make-interface-identifier-parameter
							    interface-id-2)
							   asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-09 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if two ASPIAC and and two NTFY is returned.



(define (m2ua-sgp-aspm-v-10 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message asp-inactive-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-inactive-ack-message?)
    (sleep 1)
    (close fd)))
;;; (m2ua-sgp-aspm-v-10 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if one or two  ASPIAC-ACK and and one or two NTFY is returned.



(define (m2ua-sgp-aspm-v-11 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message (m2ua-add-parameter
							   (m2ua-make-interface-identifier-parameter
							    interface-id-1)
							   asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message (m2ua-add-parameter
							   (m2ua-make-interface-identifier-parameter
							    interface-id-2)
							   asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message (m2ua-add-parameter
							     (m2ua-make-interface-identifier-parameter
							      interface-id-1)
							     asp-inactive-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-inactive-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message (m2ua-add-parameter
							     (m2ua-make-interface-identifier-parameter
							      interface-id-2)
							     asp-inactive-message-parameters)))
    (m2ua-wait-for-message fd m2ua-asp-inactive-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-v-11 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if two ASPIAC-ACK and and two NTFY is returned.



(define (m2ua-sgp-aspm-v-12 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-asp-active-message
			       (list (m2ua-make-integer-range-interface-identifier-parameter
				      (list (list interface-id-start interface-id-stop))))))
    (m2ua-wait-for-message fd-1 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (close fd-1)
    (close fd-2)))

;;; (m2ua-sgp-aspm-v-12 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
;;; The test is passed if an ASPACTIVE-ACK and and an NTFY is returned on the first
;;; association, nothing at the point on time on the second. 



(define (m2ua-sgp-aspm-v-13 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-asp-active-message
			       (m2ua-add-parameter
				(m2ua-make-interface-identifier-parameter interface-id-1)
				asp-active-message-parameters-1)))
    (m2ua-wait-for-message fd-1 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-active-message
			       (m2ua-add-parameter
				(m2ua-make-interface-identifier-parameter interface-id-2)
				asp-active-message-parameters-2)))
    (m2ua-wait-for-message fd-2 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-2 m2ua-notify-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-aspm-v-13 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)



(define (m2ua-sgp-aspm-v-14 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-asp-active-message
			       (m2ua-add-parameter
				(m2ua-make-interface-identifier-parameter interface-id-1)
				asp-active-message-parameters-1)))
    (m2ua-wait-for-message fd-1 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-active-message
			       (m2ua-add-parameter
				(m2ua-make-interface-identifier-parameter interface-id-2)
				asp-active-message-parameters-1)))
    (m2ua-wait-for-message fd-2 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-2 m2ua-notify-message?)
    (m2ua-send-message fd-2 0 (m2ua-make-asp-inactive-message
			       (m2ua-add-parameter
				(m2ua-make-interface-identifier-parameter interface-id-2)
				asp-inactive-message-parameters-2)))
    (m2ua-wait-for-message fd-2 m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-aspm-v-14 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)



(define (m2ua-sgp-aspm-v-15 tester-addr tester-port sut-addr sut-port number length)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port))
	(beat-message (m2ua-make-beat-message (random-bytes length))))
    (dotimes (i number)
      (m2ua-send-message fd 0 beat-message)
      (m2ua-wait-for-message fd m2ua-beat-ack-message?))
    (close fd)))
;;; (m2ua-sgp-aspm-v-15 tester-addr tester-port sut-addr sut-port 100 10000)

(define (m2ua-sgp-aspm-io-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-io-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPAC is returned.



(define (m2ua-sgp-aspm-io-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-data-message interface-id test-message))
    (close fd)))
;;; (m2ua-sgp-aspm-io-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if the IUT discards the data message.



(define (m2ua-sgp-aspm-io-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-io-03 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPAC message is returned.



(define (m2ua-sgp-aspm-io-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-error-message?)      ;;; the sequence might be different
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-io-04 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-increment-version 
			     (m2ua-make-asp-up-message asp-up-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-common-header m2ua-version
						     m2ua-spare
						     m2ua-reserved-message-class
						     0
						     8))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-common-header m2ua-version
						     m2ua-spare
						     m2ua-asptm-message-class
						     m2ua-reserved-asptm-message-type
						     8))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-03 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message
			     (m2ua-add-parameter
			      (m2ua-make-traffic-mode-type-parameter m2ua-traffic-mode-type-override)
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-04 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-05 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-06 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    ;;; the m2ua-asp-active-parameters are not considered here...
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message
			     (list (m2ua-make-integer-interface-identifier-parameter illegal-interface-id))))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-06 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-07 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message (list)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-07 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.



(define (m2ua-sgp-aspm-iv-08 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message
			     (m2ua-add-parameter
			      (m2ua-make-asp-id-parameter illegal-asp-id)
			      asp-up-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-aspm-iv-08 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERR message is returned.




;;;
;;; IIM message tests.
;;;

(define (m2ua-sgp-iim-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-reg-req-message
			     link-key-id
			     sdt-id
			     sdl-id))
    (m2ua-wait-for-message fd m2ua-reg-rsp-message?)
    (close fd)))
;;; (m2ua-sgp-iim-v-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an REG-RSP is returned.



(define (m2ua-sgp-iim-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-reg-req-message
			     link-key-id
			     sdt-id
			     sdl-id))
    (let ((reg-rsp (m2ua-wait-for-message fd m2ua-reg-rsp-message?)))
      (let ((interface-id (m2ua-get-integer-interface-identifier reg-rsp)))
	(m2ua-send-message fd 0 (m2ua-make-dereg-req-message
				 interface-id))))
    (m2ua-wait-for-message fd m2ua-dereg-rsp-message?)
    (close fd)))
;;; (m2ua-sgp-iim-v-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an DEREG-RSP is returned.


(define (m2ua-sgp-iim-v-03 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    ;;; Move ASP-1 to ACTIVE
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-reg-req-message
			       link-key-id
			       sdt-id
			       sdl-id))
    (let ((reg-rsp (m2ua-wait-for-message fd-1 m2ua-reg-rsp-message?)))
      (let ((interface-id (m2ua-get-integer-interface-identifier reg-rsp)))
	(m2ua-send-message fd-1 0 (m2ua-make-asp-active-message
				   (list (m2ua-make-integer-interface-identifier-parameter interface-id))))
	(m2ua-wait-for-message fd-1 m2ua-asp-active-ack-message?)
	(m2ua-wait-for-message fd-1 m2ua-notify-message?)))
    ;;; Move ASP-2 to INACTIVE
    (m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
    (m2ua-wait-for-message fd-2 m2ua-asp-up-ack-message?)
    
    (m2ua-send-message fd-2 0 (m2ua-make-reg-req-message
			       link-key-id
			       sdt-id
			       sdl-id))
    (m2ua-wait-for-message fd-2 m2ua-reg-rsp-message?)
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-iim-v-03 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
;;; The test is passed if an REG-RSP is returned for ASP-2.



(define (m2ua-sgp-iim-v-04 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    ;;; Move ASP-1 to INACTIVE
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-reg-req-message
			       link-key-id
			       sdt-id
			       sdl-id))
    (let ((reg-rsp (m2ua-wait-for-message fd-1 m2ua-reg-rsp-message?)))
      (let ((interface-id (m2ua-get-integer-interface-identifier reg-rsp)))
        ;;; Move ASP-2 to INACTIVE
	(m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
	(m2ua-wait-for-message fd-2 m2ua-asp-up-ack-message?)
    
	(m2ua-send-message fd-2 0 (m2ua-make-reg-req-message
				   link-key-id
				   sdt-id
				   sdl-id))
	(m2ua-wait-for-message fd-2 m2ua-reg-rsp-message?)
	;;; Now deregister ASP-1
	(m2ua-send-message fd-1 0 (m2ua-make-dereg-req-message interface-id))
	(m2ua-wait-for-message fd-1 m2ua-dereg-rsp-message?)))
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-iim-v-04 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
;;; The test is passed if an DEREG-RSP is returned for ASP-1.



(define (m2ua-sgp-iim-v-05 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
  (let ((fd-1 (m2ua-connect tester-addr tester-port-1 sut-addr sut-port))
	(fd-2 (m2ua-connect tester-addr tester-port-2 sut-addr sut-port)))
    ;;; Move ASP-1 to INACTIVE
    (m2ua-send-message fd-1 0 (m2ua-make-asp-up-message asp-up-message-parameters-1))
    (m2ua-wait-for-message fd-1 m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd-1 m2ua-notify-message?)
    (m2ua-send-message fd-1 0 (m2ua-make-reg-req-message
			       link-key-id
			       sdt-id
			       sdl-id))
    (let ((reg-rsp (m2ua-wait-for-message fd-1 m2ua-reg-rsp-message?)))
      (let ((interface-id-1 (m2ua-get-integer-interface-identifier reg-rsp)))
        ;;; Move ASP-2 to INACTIVE
	(m2ua-send-message fd-2 0 (m2ua-make-asp-up-message asp-up-message-parameters-2))
	(m2ua-wait-for-message fd-2 m2ua-asp-up-ack-message?)
    
	(m2ua-send-message fd-2 0 (m2ua-make-reg-req-message
				   link-key-id
				   sdt-id
				   sdl-id))
	(let ((reg-rsp (m2ua-wait-for-message fd-2 m2ua-reg-rsp-message?)))
	  (let ((interface-id-2 (m2ua-get-integer-interface-identifier reg-rsp)))
	    ;;; Now deregister ASP-1
	    (m2ua-send-message fd-1 0 (m2ua-make-dereg-req-message interface-id-1))
	    (m2ua-wait-for-message fd-1 m2ua-dereg-rsp-message?)
	    ;;; Now deregitser ASP-2
	    (m2ua-send-message fd-2 0 (m2ua-make-dereg-req-message interface-id-2))
	    (m2ua-wait-for-message fd-2 m2ua-dereg-rsp-message?)))))
    (close fd-1)
    (close fd-2)))
;;; (m2ua-sgp-iim-v-05 tester-addr tester-port-1 tester-port-2 sut-addr sut-port)
;;; The test is passed if an DEREG-RSP is returned for ASP-1 and ASP-2.



(define (m2ua-sgp-iim-io-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-reg-req-message
			     link-key-id
			     sdt-id
			     sdl-id))
    (let ((reg-rsp (m2ua-wait-for-message fd m2ua-reg-rsp-message?)))
      (let ((interface-id (m2ua-get-integer-interface-identifier reg-rsp)))
	(m2ua-send-message fd 0 (m2ua-make-asp-active-message
				 (list (m2ua-make-integer-interface-identifier-parameter interface-id))))
	(m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
	(m2ua-wait-for-message fd m2ua-notify-message?)
	(m2ua-send-message fd 0 (m2ua-make-dereg-req-message interface-id))))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-iim-io-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ERROR is returned.




;;;
;;; MAUP message tests.
;;;

(define (m2ua-sgp-maup-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an establish confirmation message is returned.



(define (m2ua-sgp-maup-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-data-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a data message is received.


(define (m2ua-sgp-maup-v-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-release-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-release-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-03 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an release confirmation message is returned.



(define (m2ua-sgp-maup-v-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-release-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-04 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an release indication message is received.



(define (m2ua-sgp-maup-v-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-flush-buffers-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-05 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-06 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-continue-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-06 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-07 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-clear-rtb-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-07 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-08 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-audit-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-08 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-09 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-cong-clear-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-09 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-10 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-cong-accept-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-10 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-11 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id m2ua-cong-discard-status))
    (m2ua-wait-for-message fd m2ua-state-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-11 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state confirmation is received.



(define (m2ua-sgp-maup-v-12 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-state-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-12 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state indication is received.



(define (m2ua-sgp-maup-v-13 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-state-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-13 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state indication is received.



(define (m2ua-sgp-maup-v-14 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    ;;; Now the SS7 link needs to be overloaded...
    (m2ua-wait-for-message fd m2ua-state-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-14 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state indication is received.



(define (m2ua-sgp-maup-v-15 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    ;;; How can I trigger congestion at the IUT?
    ;;; I'm sending 100 messages...
    (dotimes (i 100)
      (m2ua-send-message fd 1 (m2ua-make-data-message interface-id test-message)))
    (m2ua-wait-for-message fd m2ua-state-indication-message?)
    (m2ua-wait-for-message fd m2ua-state-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-15 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a state indication is received.



(define (m2ua-sgp-maup-v-16 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-bsn-action))
    (m2ua-wait-for-message fd m2ua-data-retrieval-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-16 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a retrieval confirmation is received.



(define (m2ua-sgp-maup-v-17 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-bsn-action))
    (m2ua-wait-for-message fd m2ua-data-retrieval-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-msgs-action 0))
    (m2ua-wait-for-message fd m2ua-data-retrieval-confirmation-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-17 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a retrieval confirmation is received.



(define (m2ua-sgp-maup-v-18 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    ;;; Some messages that can be retrieved later.
    (dotimes (i 10)
      (m2ua-send-message fd 1 (m2ua-make-data-message interface-id test-message)))
    (m2ua-send-message fd 1 (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-bsn-action))
    (m2ua-wait-for-message fd m2ua-data-retrieval-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-data-retrieval-request-message interface-id m2ua-rtrv-msgs-action 0))
    (m2ua-wait-for-message fd m2ua-data-retrieval-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-data-retrieval-indication-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-18 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a retrieval confirmation is received.



(define (m2ua-sgp-maup-v-19 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-wait-for-message fd m2ua-data-message?)
    (close fd)))
;;; (m2ua-sgp-maup-v-19 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if a retrieval confirmation is received.



(define (m2ua-sgp-maup-iv-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 0 (m2ua-make-data-message interface-id test-message))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-maup-iv-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error message is received.



(define (m2ua-sgp-maup-iv-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 0 (m2ua-make-data-message interface-id test-message))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-maup-iv-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error message is received.



(define (m2ua-sgp-maup-iv-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message illegal-interface-id m2ua-flush-buffers-status))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-maup-iv-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



;;;
;;; GEH tests
;;;



(define (m2ua-sgp-geh-01-1 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message 
			     (m2ua-add-parameter 
			      (m2ua-make-interface-identifier-parameter illegal-interface-id)
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-01-1 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-01-2 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message 
			     (m2ua-add-parameter 
			      (m2ua-make-interface-identifier-parameter illegal-interface-id)
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-01-2 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-01-3 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message illegal-interface-id))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-01-3 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message 
			     (m2ua-add-parameter 
			      (m2ua-make-traffic-mode-type-parameter m2ua-traffic-mode-type-invalid)
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-02 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-03-1 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message 
			     (m2ua-add-parameter 
			      (m2ua-make-interface-identifier-parameter "Hallo")
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-03-1 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-03-2 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-inactive-message 
			     (m2ua-add-parameter 
			      (m2ua-make-interface-identifier-parameter "Hallo")
			      asp-active-message-parameters)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-03-2 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-03-3 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message "Hallo"))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-03-3 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-message m2ua-reserved-message-class 0 (list)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-04 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-establish-request-message interface-id))
    (m2ua-wait-for-message fd m2ua-establish-confirmation-message?)
    (m2ua-send-message fd 1 (m2ua-make-state-request-message interface-id #xabcd))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-05 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.



(define (m2ua-sgp-geh-06-1 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-message m2ua-aspsm-message-class
					       m2ua-aspup-message-type
					       (list (list 0 4 0 3))))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-06-1 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.


(define (m2ua-sgp-geh-06-2 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    ;;; send a MAUP message with too short integer interface id.
    (m2ua-send-message fd 1 (m2ua-make-message m2ua-maup-message-class
					       m2ua-est-req-message-type
					       (list (list 0 1 0 4))))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-06-2 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.


(define (m2ua-sgp-geh-07 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message 
			     (list (m2ua-make-traffic-mode-type-parameter
				    m2ua-traffic-mode-type-invalid))))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-07 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.


(define (m2ua-sgp-geh-08 tester-addr tester-port sut-addr sut-port)
  (let ((fd (m2ua-connect tester-addr tester-port sut-addr sut-port)))
    (m2ua-send-message fd 0 (m2ua-make-asp-up-message asp-up-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-up-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 0 (m2ua-make-asp-active-message asp-active-message-parameters))
    (m2ua-wait-for-message fd m2ua-asp-active-ack-message?)
    (m2ua-wait-for-message fd m2ua-notify-message?)
    (m2ua-send-message fd 1 (m2ua-make-message m2ua-maup-message-class
					       m2ua-est-req-message-type
					       (list)))
    (m2ua-wait-for-message fd m2ua-error-message?)
    (close fd)))
;;; (m2ua-sgp-geh-08 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an error is received.




