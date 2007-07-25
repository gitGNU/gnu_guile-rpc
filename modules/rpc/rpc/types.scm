;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2007  Free Software Foundation
;;;
;;; This file is part of GNU Guile-RPC.
;;;
;;; GNU Guile-RPC is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guile-RPC is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rpc rpc types)
  :use-module (rpc xdr)
  :use-module (rpc xdr types)
  :export (rpc-auth-flavor
           rpc-opaque-auth
           rpc-message-type
           rpc-reply-stat
           rpc-accept-stat
           rpc-reject-stat
           rpc-auth-stat
           rpc-call-body
           rpc-mismatch-info
           rpc-accepted-reply
           rpc-rejected-reply
           rpc-reply-body
           rpc-message))

;;; Commentary:
;;;
;;; Definition of the XDR types used by the ONC RPC message protocol (RFC
;;; 1831, Section 8).
;;;
;;; Code:


;;;
;;; Section 7.2: Authentication.
;;;

(define rpc-auth-flavor
  (make-xdr-enumeration 'auth_flavor
                        `((AUTH_NONE  . 0)
                          (AUTH_SYS   . 1)
                          (AUTH_SHORT . 2))))
(define rpc-opaque-auth
  (make-xdr-struct-type (list rpc-auth-flavor
                              (make-xdr-variable-length-opaque-array 400))))



;;;
;;; Section 8: RPC Message Protocol.
;;;

(define rpc-message-type
  (make-xdr-enumeration 'msg_type
                        '((CALL . 0) (REPLY . 1))))

(define rpc-reply-stat
  (make-xdr-enumeration 'reply_stat
                        '((MSG_ACCEPTED . 0) (MSG_DENIED . 1))))

(define rpc-accept-stat
  (make-xdr-enumeration 'accept_stat
                        '((SUCCESS       . 0)
                          (PROG_UNAVAIL  . 1)
                          (PROG_MISMATCH . 2)
                          (PROC_UNAVAIL  . 3)
                          (GARBAGE_ARGS  . 4)
                          (SYSTEM_ERR    . 5))))

(define rpc-reject-stat
  (make-xdr-enumeration 'rejet_stat
                        '((RPC_MISMATCH  . 0)
                          (AUTH_ERROR    . 1))))

(define rpc-auth-stat
  (make-xdr-enumeration 'auth_stat
                        '((AUTH_OK           . 0)
                          (AUTH_BADCRED      . 1)
                          (AUTH_REJECTEDCRED . 2)
                          (AUTH_BADVERF      . 3)
                          (AUTH_REJECTEDVERF . 4)
                          (AUTH_TOOWEAK      . 5)
                          (AUTH_INVALIDRESP  . 6)
                          (AUTH_FAILED       . 7))))

(define rpc-call-body
  (make-xdr-struct-type (list xdr-unsigned-integer ;; rpc-version
                              xdr-unsigned-integer ;; program
                              xdr-unsigned-integer ;; version
                              xdr-unsigned-integer ;; procedure
                              rpc-opaque-auth      ;; credentials
                              rpc-opaque-auth      ;; verifier
                              )))

(define rpc-mismatch-info
  (make-xdr-struct-type (list xdr-unsigned-integer    ;; low
                              xdr-unsigned-integer))) ;; high

(define rpc-accepted-reply
  (let* ((reply-data
          (make-xdr-union-type rpc-accept-stat
                               `((SUCCESS       . ,xdr-void)
                                 (PROG_MISMATCH . ,rpc-mismatch-info))
                               xdr-void)))
    (make-xdr-struct-type (list rpc-opaque-auth      ;; verifier
                                reply-data))))

(define rpc-rejected-reply
  (make-xdr-union-type rpc-reject-stat
                       `((RPC_MISMATCH . ,rpc-mismatch-info)
                         (AUTH_ERROR   . ,rpc-auth-stat))
                       #f))

(define rpc-reply-body
  (make-xdr-union-type rpc-reply-stat
                       `((MSG_ACCEPTED . ,rpc-accepted-reply)
                         (MSG_DENIED   . ,rpc-rejected-reply))
                       #f))

(define rpc-message
  (let ((body (make-xdr-union-type rpc-message-type
                                   `((CALL . ,rpc-call-body)
                                     (REPLY . ,rpc-reply-body))
                                   #f)))
    (make-xdr-struct-type (list xdr-unsigned-integer ;; xid
                                body))))


;;; types.scm ends here

;;; arch-tag: be91c387-4fd7-45e4-82b4-4a34f74c119f
