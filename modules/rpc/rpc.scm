;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2007  Free Software Foundation, Inc.
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

(define-module (rpc rpc)
  :use-module (rpc xdr)
  :use-module (rpc rpc types)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (r6rs bytevector)
  :use-module (ice-9 optargs)
  :autoload   (rpc rpc transports) (send-rpc-record
                                    rpc-record-marking-input-port)
  :export (;; messaging
           %onc-rpc-version
           make-rpc-message

           assert-successful-reply

           ;; high-level
           make-synchronous-rpc-call

           ;; error conditions
           &rpc-error &rpc-call-error
           rpc-error? rpc-call-error?
           rpc-invalid-reply-error? rpc-invalid-reply-error:message
           rpc-invalid-reply-xid-error?
           rpc-invalid-reply-xid-error:expected-xid
           rpc-invalid-reply-xid-error:reply-xid
           rpc-program-unavailable-error?
           rpc-program-mismatch-error?
           rpc-procedure-unavailable-error?
           rpc-program-mismatch-error?
           rpc-program-mismatch-error:low-version
           rpc-program-mismatch-error:high-version
           rpc-garbage-arguments-error?
           rpc-system-error?
           rpc-authentication-error?))

;;; Commentary:
;;;
;;; An implementation of ONC RPC (RFC 1831).
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &rpc-error &error
  rpc-error?)

(define-condition-type &rpc-call-error &rpc-error
  rpc-call-error?)

(define-condition-type &rpc-invalid-reply-error &rpc-call-error
  rpc-invalid-reply-error?
  (message  rpc-invalid-reply-error:message))

(define-condition-type &rpc-invalid-reply-xid-error &rpc-call-error
  rpc-invalid-reply-xid-error?
  (expected-xid  rpc-invalid-reply-xid-error:expected-xid)
  (reply-xid     rpc-invalid-reply-xid-error:reply-xid))

(define-condition-type &rpc-program-unavailable-error &rpc-call-error
  rpc-program-unavailable-error?)

(define-condition-type &rpc-program-mismatch-error &rpc-call-error
  rpc-program-mismatch-error?
  (low-version   rpc-program-mismatch-error:low-version)
  (high-version  rpc-program-mismatch-error:high-version))

(define-condition-type &rpc-procedure-unavailable-error &rpc-call-error
  rpc-procedure-unavailable-error?)

(define-condition-type &rpc-garbage-arguments-error &rpc-call-error
  rpc-garbage-arguments-error?)

(define-condition-type &rpc-system-error &rpc-call-error
  rpc-system-error?)

(define-condition-type &rpc-authentication-error &rpc-call-error
  ;; XXX: Needs to be further refined.
  rpc-authentication-error?)



;;;
;;; Creating RPC messages (Section 8).
;;;

(define %onc-rpc-version
  ;; We only support version 2 of ONC RPC (Section 8, struct `call_body').
  2)

(define %dummy-opaque-auth
  ;; An `rpc-opaque-auth' datum.
  (list 'AUTH_NONE '#()))

(define (make-rpc-call-message program version procedure)
  ;; Return an `rpc-call-body' datum.
  (list %onc-rpc-version program version procedure
        %dummy-opaque-auth %dummy-opaque-auth))

(define (make-rpc-mismatch-info-message low-version high-version)
  ;; Return an `rpc-mismatch-info' datum.
  (list low-version high-version))

(define (make-rpc-accepted-reply-message accept-stat . args)
  ;; Return an `rpc-accepted-reply' datum.
  (list %dummy-opaque-auth
        (cons accept-stat
              (case accept-stat
                ((PROG_MISMATCH)
                 (apply make-rpc-mismatch-info-message args))
                ((SUCCESS PROG_UNAVAIL PROC_UNAVAIL GARBAGE_ARGS SYSTEM_ERR)
                 'void)
                (else
                 (error "invalid accept-stat" accept-stat))))))

(define (make-rpc-denied-reply-message reject-stat . args)
  (cons reject-stat
        (case reject-stat
          ((RPC_MISMATCH)
           (apply make-rpc-mismatch-info-message args))
          ((AUTH_ERROR)
           ;; XXX: Not implemented.
           #f)
          (else
           (error "invalid reject-stat" reject-stat)))))

(define (make-rpc-reply-message xid reply-stat . args)
  ;; Return an `rpc-reply-body' datum.
  (let ((body (case reply-stat
                ((MSG_ACCEPTED)
                 (apply make-rpc-accepted-reply-message args))
                ((MSG_DENIED)
                 (apply make-rpc-denied-reply-message args))
                (else
                 (error "invalid reply-stat" reply-stat)))))
    (cons reply-stat body)))

(define (make-rpc-message xid type . args)
  "Return an @code{rpc-message} datum.  @var{type} should be either
@code{CALL} or @code{REPLY} (the two values of the @code{rpc-message-type}
enumeration)."
  (let ((body
         (case type
           ((CALL)
            (apply make-rpc-call-message args))
           ((REPLY)
            (apply make-rpc-reply-message xid args)))))
    (list xid ;; the xid
          (cons type ;; the discriminant
                body))))


;;;
;;; Decoding RPC messages.
;;;

(define (assert-successful-reply rpc-msg xid)
  "Return true if @var{rpc-msg} (an RPC message as returned by a previous
@code{(xdr-decode rpc-message port)} call) is a valid reply for the
invocation labeled with transaction ID @var{xid} indicating that it was
accepted.  If @var{xid} is @code{#t}, any reply transaction ID is accepted
and it is returned (provided the rest of the message denotes an accepted
message).  On failure, an appropriate error condition is raised."
  (let* ((reply-xid (car rpc-msg))
         (reply-msg (cadr rpc-msg)))
    (if (or (eq? xid #t) (equal? xid reply-xid))
        (if (eq? (car reply-msg) 'REPLY)
            (let ((reply-body (cdr reply-msg)))
              (case (car reply-body)
                ((MSG_ACCEPTED)
                 ;; XXX: skip the opaque auth
                 (let ((reply-data (cadddr reply-msg)))
                   (case (car reply-data)
                     ((SUCCESS) reply-xid)
                     ((PROG_UNAVAIL)
                      (raise (condition (&rpc-program-unavailable-error))))
                     ((PROG_MISMATCH)
                      (let* ((hi+lo (cdr reply-data))
                             (lo    (car hi+lo))
                             (hi    (cadr hi+lo)))
                        (raise (condition
                                (&rpc-program-mismatch-error
                                 (low-version   lo)
                                 (high-version  hi))))))
                     ((PROC_UNAVAIL)
                      (raise (condition (&rpc-procedure-unavailable-error))))
                     ((GARBAGE_ARGS)
                      (raise (condition (&rpc-garbage-arguments-error))))
                     ((SYSTEM_ERROR)
                      (raise (condition (&rpc-system-error))))
                     (else
                      (raise (condition (&rpc-call-error)))))))
                ((MSG_DENIED)
                 (case (cadr reply-body)
                   ((RPC_MISMATCH)
                    (let* ((hi+lo (cddr reply-body))
                           (lo    (car hi+lo))
                           (hi    (cadr hi+lo)))
                      (raise (condition
                              (&rpc-program-mismatch-error
                               (low-version   lo)
                               (high-version  hi))))))
                   ((AUTH_ERROR)
                    ;; XXX: Not further decoded.
                    (raise (condition (&rpc-authentication-error))))
                   (else
                    (raise (condition (&rpc-call-error))))))))
            (raise (condition (&rpc-invalid-reply-error
                               (message reply-msg)))))
        (raise (condition (&rpc-invalid-reply-xid-error
                           (expected-xid xid)
                           (reply-xid reply-xid)))))))


;;;
;;; Client-side helpers.
;;;

(define (make-synchronous-rpc-call program version procedure
                                   arg-type result-type)
  "Return a procedure that may be applied to an argument list, transaction
ID, and I/O port, to make a synchronous RPC call to the remote procedure
numbered @var{procedure} in @var{program}, version @var{version}.  On
success, the invocation result is eventually returned.  Otherwise, an error
condition is raised."
  (lambda* (args xid endpoint
                 #:optional (send-message send-rpc-record)
                            (wrap-input-port rpc-record-marking-input-port))
    (let* ((call-msg     (make-rpc-message xid 'CALL program version
                                           procedure))
           (call-msg-len (xdr-type-size rpc-message call-msg))
           (args-msg-len (xdr-type-size arg-type args))
           (msg-len      (+ call-msg-len args-msg-len))
           (msg          (make-bytevector msg-len)))

      (xdr-encode! msg 0 rpc-message call-msg)
      (xdr-encode! msg call-msg-len arg-type args)
      (send-message endpoint msg 0 msg-len)
      (force-output endpoint)

      ;;(format #t "request sent (~a = ~a + ~a)!~%" msg-len
      ;;        call-msg-len args-msg-len)

      ;; Wait for an answer
      (let* ((endpoint  (wrap-input-port endpoint))
             (reply-msg (xdr-decode rpc-message endpoint)))
        (and (assert-successful-reply reply-msg xid)
             (xdr-decode result-type endpoint))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; rpc.scm ends here
