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

(define-module (rpc rpc portmap)
  :use-module (rpc rpc)
  :use-module (rpc rpc types)
  :use-module (rpc xdr)
  :use-module (rpc xdr types)
  :autoload   (rpc rpc transports)  (send-rpc-record
                                     rpc-record-marking-input-port)
  :autoload   (r6rs bytevector)     (make-bytevector)
  :autoload   (ice-9 rdelim)        (read-line)
  :autoload   (ice-9 regex)         (make-regexp)
  :autoload   (srfi srfi-1)         (find)

  :export (%portmapper-port %portmapper-version-number
           %portmapper-program-number

           portmapper-null portmapper-set portmapper-unset
           portmapper-get-port portmapper-dump portmapper-call-it

           read-rpc-service-list lookup-rpc-service-name
           lookup-rpc-service-number))


;;;
;;; Protocol constants.
;;;

(define %portmapper-port           111)
(define %portmapper-program-number 100000)
(define %portmapper-version-number 2)

(define %portmapper-null-proc-number      0)
(define %portmapper-set-proc-number       1)
(define %portmapper-unset-proc-number     2)
(define %portmapper-get-port-proc-number  3)
(define %portmapper-dump-proc-number      4)
(define %portmapper-call-it-proc-number   5)


;;;
;;; Data types.
;;;

(define pmap-struct-type
  (make-xdr-struct-type (list xdr-unsigned-integer    ;; pm_prog
                              xdr-unsigned-integer    ;; pm_vers
                              xdr-unsigned-integer    ;; pm_prot
                              xdr-unsigned-integer))) ;; pm_port

;; `pmap' list should really be defined as follows:
;;
;;   typedef union switch (bool_t) {
;;
;;        case TRUE: struct {
;;                struct pmap;
;;                pmaplist_t foo;
;;        };
;;
;;        case FALSE: struct {};
;;   } pmaplist_t;
;;
;; However, we disallow the creation of recursive XDR types (due to the lack
;; of mutators for XDR type objects).  Therefore, we provide our own decoding
;; mechanism for this type.

(define pmap-list-element-type
  (make-xdr-union-type xdr-boolean
                       `((TRUE  . ,pmap-struct-type)
                         (FALSE . ,xdr-void))
                       #f))

(define (xdr-decode-pmap-list port)
  (let loop ((item (xdr-decode pmap-list-element-type port))
             (result '()))
    (if (or (eof-object? item)
            (eq? 'FALSE (car item)))
        (reverse! result)
        (loop (xdr-decode pmap-list-element-type port)
              (cons (cdr item) result)))))

(define (make-portmapper-dump-call send-message wrap-input-port)
  (define arg-type xdr-void)
  (define program %portmapper-program-number)
  (define version %portmapper-version-number)
  (define procedure %portmapper-dump-proc-number)

  (lambda (args xid endpoint)
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

      ;; Wait for an answer
      (let* ((endpoint  (wrap-input-port endpoint))
             (reply-msg (xdr-decode rpc-message endpoint)))
        (and (assert-successful-reply reply-msg xid)
             (xdr-decode-pmap-list endpoint))))))


;;;
;;; Client side.
;;;

(define portmapper-null
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-null-proc-number
                             xdr-void xdr-void))

(define portmapper-set
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-set-proc-number
                             pmap-struct-type xdr-boolean))

(define portmapper-unset
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-unset-proc-number
                             pmap-struct-type xdr-boolean))

(define portmapper-get-port
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-get-port-proc-number
                             pmap-struct-type xdr-unsigned-integer))

(define portmapper-dump
  (make-portmapper-dump-call send-rpc-record
                             rpc-record-marking-input-port))

(define portmapper-call-it
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-call-it-proc-number
                             xdr-void xdr-void))



;;;
;;; Reading `/etc/rpc'.
;;;

(define (read-rpc-service-list port)
  "Return a list of name-program pairs, showing the connection between an RPC
program human-readable name and its program number."
  (define comment-rx (make-regexp "^[[:blank:]]*#.*$" regexp/extended))
  (define empty-rx (make-regexp "^[[:blank:]]*$" regexp/extended))
  (define service-rx
    (make-regexp
     "^[[:blank:]]*([[:graph:]]+)[[:blank:]]+([[:digit:]]+)([[:blank:]].*)?$"
     regexp/extended))

  (let loop ((line (read-line port))
             (result '()))
    (if (eof-object? line)
        result
        (loop (read-line port)
              (cond ((or (regexp-exec comment-rx line)
                         (regexp-exec empty-rx line))
                     result)
                    ((regexp-exec service-rx line)
                     =>
                     (lambda (match)
                       (cons (cons (match:substring match 1)
                                   (string->number
                                    (match:substring match 2)))
                             result)))
                    (else
                     (error "RPC service parse error" line)))))))

(define (lookup-rpc-service-name service-list program)
  "Lookup RPC program numbered @var{program} in @var{service-list} (a list as
returned by @code{read-rpc-service-list}) and return its human-readable name."
  (let ((result (find (lambda (pair)
                        (equal? (cdr pair) program))
                      service-list)))
    (if (pair? result)
        (car result)
        #f)))

(define (lookup-rpc-service-number service-list program)
  "Lookup RPC program named @var{program} in @var{service-list} (a list as
returned by @code{read-rpc-service-list}) and return its RPC program number."
  (let ((result (find (lambda (pair)
                        (string=? (car pair) program))
                      service-list)))
    (if (pair? result)
        (cdr result)
        #f)))


;;; arch-tag: beb01ebb-2e39-4f9e-ab3f-1e6f5ec52bea
