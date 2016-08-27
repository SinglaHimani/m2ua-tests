;;; 
;;; Copyright (C) 2004, 2005 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: m2ua-param-testtool-sgp.scm,v 1.4 2008/04/06 23:48:02 tuexen Exp $

;;; Define a transport address of the system under test
(define sut-addr "127.0.0.1")
(define sut-port   m2ua-port)
(define sut-port-1 m2ua-port)
(define sut-port-2 (1+ m2ua-port))

;;; Define the transport address of the tester
(define tester-addr "127.0.0.1")

(define tester-port   0)
(define tester-port-1 3000)
(define tester-port-2 3001)

;;; Define a valid SS7 message
(define test-message (list 11 34 45 67 67 89))

;;; Define an asp-identifier
(define asp-id          1)
(define asp-id-1        1)
(define asp-id-2        2)
(define illegal-asp-id  3)

;;; Define integer interface ids
(define interface-id 1)
(define interface-id-1 1)
(define interface-id-2 2)
(define interface-id-start 1)
(define interface-id-stop  2)
(define illegal-interface-id 1000)

(define link-key-id 1)
(define sdt-id 1)
(define sdl-id 1)

(define traffic-mode          m2ua-traffic-mode-type-loadshare)

(define asp-up-message-parameters (list))

(define asp-up-message-parameters-1 (list))

(define asp-up-message-parameters-2 (list))

(define asp-active-message-parameters (list))

(define asp-active-message-parameters-1 (list))

(define asp-active-message-parameters-2 (list))

(define asp-active-ack-message-parameters (list))

(define asp-inactive-message-parameters (list))

(define asp-inactive-message-parameters-1 (list))

(define asp-inactive-message-parameters-2 (list))

(define asp-inactive-ack-message-parameters (list))

(define data-message-parameters (list))

