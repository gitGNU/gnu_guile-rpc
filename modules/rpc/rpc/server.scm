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

(define-module (rpc rpc server)
  :use-module (rpc rpc)
  :autoload   (rpc rpc types)       (rpc-message)
  :autoload   (rpc xdr)             (xdr-type-size xdr-decode xdr-error?)
  :autoload   (rpc rpc transports)  (rpc-record-marking-input-port
                                     send-rpc-record)
  :autoload   (srfi srfi-1)         (find fold alist-delete!)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (srfi srfi-39)
  :use-module (r6rs bytevector)
  :autoload   (r6rs i/o ports)      (lookahead-u8)

  :export (procedure-call-information rpc-call?
           rpc-call-xid rpc-call-program rpc-call-version
           rpc-call-procedure rpc-call-credentials rpc-call-verifier

           make-rpc-program make-rpc-program-version make-rpc-procedure
           rpc-program? rpc-program-version? rpc-procedure?
           rpc-program-number rpc-program-versions rpc-program-lookup-version
           rpc-program-version-number rpc-program-version-procedures
           rpc-program-version-lookup-procedure
           rpc-procedure-number rpc-procedure-handler
           rpc-procedure-argument-xdr-type rpc-procedure-result-xdr-type

           lookup-called-procedure handle-procedure-call
           handle-procedure-lookup-error

           make-i/o-manager i/o-manager?
           i/o-manager-exception-handler i/o-manager-read-handler
           run-input-event-loop

           make-rpc-listening-socket-i/o-manager
           serve-one-stream-request run-stream-rpc-server
           current-stream-connection stream-connection?
           stream-connection-port stream-connection-peer-address
           stream-connection-rpc-program

           serve-one-tcp-request run-tcp-rpc-server
           current-tcp-connection tcp-connection?
           tcp-connection-port tcp-connection-peer-address
           tcp-connection-rpc-program

           ;; error conditions
           &rpc-server-error rpc-server-error?
           &onc-rpc-version-mismatch-error onc-rpc-version-mismatch-error?
           onc-rpc-version-mismatch-error:peer-version
           &rpc-invalid-call-message-error rpc-invalid-call-message-error?
           rpc-invalid-call-message-error:message
           &rpc-connection-lost-error rpc-connection-lost-error?
           rpc-connection-lost-error:port

           &rpc-procedure-lookup-error rpc-procedure-lookup-error?
           &rpc-invalid-program-error &rpc-invalid-version-error
           &rpc-invalid-procedure-error
           rpc-invalid-program-error? rpc-invalid-version-error?
           rpc-invalid-procedure-error?
           rpc-invalid-program-error:program
           rpc-invalid-version-error:program
           rpc-invalid-version-error:version
           rpc-invalid-procedure-error:procedure
           rpc-invalid-procedure-error:version
           rpc-invalid-procedure-error:program))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Facilities to handle RPC call messages on the server-side.
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &rpc-server-error &rpc-error
  rpc-server-error?)

(define-condition-type &onc-rpc-version-mismatch-error &rpc-server-error
  onc-rpc-version-mismatch-error?
  (peer-version  onc-rpc-version-mismatch-error:peer-version))

(define-condition-type &rpc-invalid-call-message-error &rpc-server-error
  rpc-invalid-call-message-error?
  (message rpc-invalid-call-message-error:message))

(define-condition-type &rpc-connection-lost-error &rpc-server-error
  rpc-connection-lost-error?
  (port    rpc-connection-lost-error:port))

;; Procedure lookup errors.

(define-condition-type &rpc-procedure-lookup-error &rpc-server-error
  rpc-procedure-lookup-error?)

(define-condition-type &rpc-invalid-program-error
                       &rpc-procedure-lookup-error
  rpc-invalid-program-error?
  (program  rpc-invalid-program-error:program))

(define-condition-type &rpc-invalid-version-error
                       &rpc-procedure-lookup-error
  rpc-invalid-version-error?
  (version  rpc-invalid-version-error:version)
  (program  rpc-invalid-version-error:program))

(define-condition-type &rpc-invalid-procedure-error
                       &rpc-procedure-lookup-error
  rpc-invalid-procedure-error?
  (procedure  rpc-invalid-procedure-error:procedure)
  (program    rpc-invalid-procedure-error:program)
  (version    rpc-invalid-procedure-error:version))



;;;
;;; Procedure call message processing.
;;;

(define-record-type <rpc-call>
  (%make-rpc-call xid program version procedure credentials verifier)
  rpc-call?
  (xid          rpc-call-xid)
  (program      rpc-call-program)
  (version      rpc-call-version)
  (procedure    rpc-call-procedure)
  (credentials  rpc-call-credentials)
  (verifier     rpc-call-verifier))

(define (procedure-call-information call-msg)
  "Return an @code{<rpc-call>} object that denotes the procedure call
requested in @var{call-msg}.  If @var{call-msg} is not an appropriate RPC
call message, an error condition is raised."
  (let ((xid (car call-msg))
        (call (cadr call-msg)))
    (if (eq? (car call) 'CALL)
        (let ((rpc-version (cadr call))
              (call-args (cddr call)))
          (if (equal? %onc-rpc-version rpc-version)
              (apply %make-rpc-call (cons xid call-args))
              (raise (condition (&onc-rpc-version-mismatch-error
                                 (peer-version rpc-version))))))
        (raise (condition (&rpc-invalid-call-message-error
                           (message call-msg)))))))


;;;
;;; Describing RPC "programs".
;;;

(define-record-type <rpc-program>
  (%make-rpc-program number versions lookup-version)
  rpc-program?
  (number         rpc-program-number)
  (versions       rpc-program-versions)
  (lookup-version %rpc-program-lookup-version))

(define-record-type <rpc-program-version>
  (%make-rpc-program-version number procedures lookup-procedure)
  rpc-program-version?
  (number           rpc-program-version-number)
  (procedures       rpc-program-version-procedures)
  (lookup-procedure %rpc-program-version-lookup-procedure))

(define-record-type <rpc-procedure>
  (make-rpc-procedure number argument-xdr-type result-xdr-type handler)
  rpc-procedure?
  (number             rpc-procedure-number)
  (argument-xdr-type  rpc-procedure-argument-xdr-type)
  (result-xdr-type    rpc-procedure-result-xdr-type)
  (handler            rpc-procedure-handler))


(define (compile-numbered-items get-number items)
  (let* ((numbers (map get-number items))
         (vector  (make-vector (+ 1 (apply max numbers)) #f)))
    (for-each (lambda (item number)
                (vector-set! vector number item))
              items
              numbers)
    (lambda (number)
      (if (>= number (vector-length vector))
          #f
          (vector-ref vector number)))))

(define (make-rpc-program number versions)
  "Return a new object describing the RPC program identified by @var{number}
and consisting of the versions listed in @var{versions}."
  (%make-rpc-program number versions
                     (compile-numbered-items rpc-program-version-number
                                             versions)))

(define (make-rpc-program-version number procedures)
  "Return a new object describing the RPC program version identified by
@var{number} and consisting of the procedures listed in @var{procedures}."
  (%make-rpc-program-version number procedures
                             (compile-numbered-items rpc-procedure-number
                                                     procedures)))

(define (rpc-program-lookup-version program version)
  "Lookup @var{version}, a number denoting an RPC program version, into
@var{program}.  Return an @code{<rpc-program-version>} object on success,
@code{#f} otherwise."
  ((%rpc-program-lookup-version program) version))

(define (rpc-program-version-lookup-procedure version procedure)
  "Lookup @var{procedure}, a number denoting an RPC procedure number, into
@var{version}.  Return an @code{<rpc-procedure>} object on success, @code{#f}
otherwise."
  ((%rpc-program-version-lookup-procedure version) procedure))



;;;
;;; Processing requests.
;;;

(define (lookup-called-procedure call programs)
  "Return the procedure addressed by @var{call} and available in
@var{programs} (a list of @code{<rpc-program>} objects).  If the procedure
could not be found, an error condition is raised:
 @code{&rpc-invalid-program-error}, @code{&rpc-invalid-version-error} or
 @code{&rpc-invalid-procedure-error}."
  (let* ((program-num (rpc-call-program call))
         (program (find (lambda (p)
                          (= program-num (rpc-program-number p)))
                        programs)))
    (if program
        (let* ((version-num (rpc-call-version call))
               (version (rpc-program-lookup-version program version-num)))
          (if version
              (let* ((proc-num (rpc-call-procedure call))
                     (proc (rpc-program-version-lookup-procedure version
                                                                 proc-num)))
                (if proc
                    proc
                    (raise (condition (&rpc-invalid-procedure-error
                                       (procedure proc-num)
                                       (version   version)
                                       (program   program))))))
              (raise (condition (&rpc-invalid-version-error
                                 (program   program)
                                 (version   version-num))))))
        (raise (condition (&rpc-invalid-program-error
                           (program program-num)))))))

(define (handle-procedure-lookup-error c call send-result)
  "Handle procedure lookup error condition @var{c} for @var{call} by sending
the appropriate reply message via @var{send-result}."
  (cond ((rpc-invalid-program-error? c)
         (let* ((reply      (make-rpc-message (rpc-call-xid call)
                                              'REPLY 'MSG_ACCEPTED
                                              'PROG_UNAVAIL))
                (reply-size (xdr-type-size rpc-message reply))
                (raw-reply  (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))

        ((rpc-invalid-version-error? c)
         (let* ((prog        (rpc-invalid-version-error:program c))
                (versions    (map rpc-program-version-number
                                  (rpc-program-versions prog)))
                (min-version (apply min versions))
                (max-version (apply max versions))
                (reply       (make-rpc-message (rpc-call-xid call)
                                               'REPLY 'MSG_ACCEPTED
                                               'PROG_MISMATCH
                                               min-version
                                               max-version))
                (reply-size  (xdr-type-size rpc-message reply))
                (raw-reply   (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))

        ((rpc-invalid-procedure-error? c)
         (let* ((reply       (make-rpc-message (rpc-call-xid call)
                                               'REPLY 'MSG_ACCEPTED
                                               'PROC_UNAVAIL))
                (reply-size  (xdr-type-size rpc-message reply))
                (raw-reply   (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))))

(define (handle-procedure-call call programs input-port send-result)
  "Handle procedure call @var{call} using @var{programs}, reading input from
@var{input-port} and sending the result via @var{send-result}, a
three-argument procedure that is passed a bytevector, offset and octet
count."
  (guard (c ((rpc-procedure-lookup-error? c)
             (handle-procedure-lookup-error c call send-result)))

    (let* ((proc           (lookup-called-procedure call programs))
           (handler        (rpc-procedure-handler proc))
           (input-type     (rpc-procedure-argument-xdr-type proc))
           (result         (handler (xdr-decode input-type input-port)))
           (result-type    (rpc-procedure-result-xdr-type proc))
           (result-size    (xdr-type-size result-type result))

           (reply-prologue (make-rpc-message (rpc-call-xid call)
                                             'REPLY 'MSG_ACCEPTED
                                             'SUCCESS))
           (prologue-size  (xdr-type-size rpc-message reply-prologue))
           (total-size     (+ result-size prologue-size))
           (raw-result     (make-bytevector total-size)))
      (xdr-encode! raw-result
                   (xdr-encode! raw-result 0
                                rpc-message reply-prologue)
                   result-type result)
      (send-result raw-result 0 total-size))))


;;;
;;; Generic event loop.
;;;

(define-record-type <i/o-manager>
  (%make-i/o-manager  exception-handler read-handler)
  i/o-manager?
  (exception-handler  i/o-manager-exception-handler)
  (read-handler       i/o-manager-read-handler))

(define (make-i/o-manager exception-handler read-handler)
  "Return an I/O manager.  When data is available for reading,
@var{read-handler} will be called and passed a port to read from; when an
exception occurs on a port, @var{exception-handler} is called and passed the
failing port."
  (%make-i/o-manager exception-handler read-handler))


(define (run-input-event-loop fd+manager-list timeout idle-thunk)
  "Run an input event loop based on @var{fd+manager-list}, a list of pairs of
input ports (or file descriptors) and I/O managers.  I/O managers are invoked
are invoked and passed the corresponding port when data becomes readable or
when an exception occurs.  I/O manager handlers can:

@itemize
@item return @code{#f}, in which case the port and I/O manager are removed
from the list of watched ports;
@item return a pair containing an input port and I/O manager, in which case
this pair is added to the list of watched ports;
@item return true, in which case the list of watched ports remains unchanged.
@end itemize

When @var{timeout} (a number of microseconds) is reached, @var{idle-thunk} is
invoked.  If timeout is @code{#f}, then an infinite timeout is assumed and
@var{idle-thunk} is never run.  The event loop returns when no watched port
is left."

  ;; XXX: The code has a nice functional style but is far from optimized.  We
  ;; should use, e.g., vlists instead of lists
  ;; (http://en.wikipedia.org/wiki/VList).

  (define timeout-s  (and timeout (quotient  timeout 1000000)))
  (define timeout-us (and timeout (remainder timeout 1000000)))

  (define (handle-events fd-list fd+manager-list get-handler)
    ;; Handle events for file descriptors listed in FD-LIST using the
    ;; FD+MANAGER-LIST alist.  Return a new list of pairs of file descriptor
    ;; and handler.
    (let loop ((fd-list   fd-list)
               (new-alist fd+manager-list))
      (if (null? fd-list)
          new-alist
          (let* ((fd         (car fd-list))
                 (fd+handler (assoc fd fd+manager-list))
                 (handler    (get-handler (cdr fd+handler)))
                 (result     (handler fd)))
            (loop (cdr fd-list)
                  (cond ((pair? result)
                         ;; add a new fd+manager pair.
                         (cons result new-alist))
                        ((not result)
                         ;; remove this fd+manager pair.
                         (alist-delete! fd new-alist))
                        (else
                         ;; keep this fd+manager pair.
                         new-alist)))))))

  (let loop ((fd+manager-list fd+manager-list))

    (if (not (null? fd+manager-list))
        (let* ((fd-list  (map car fd+manager-list))
               (selected (select fd-list '() fd-list
                                 timeout-s timeout-us))
               (reads    (car selected))
               (excepts  (caddr selected)))
          (loop (if (and (null? reads) (null? excepts))
                    (begin
                      (idle-thunk)
                      fd+manager-list)
                    (handle-events reads
                                   (handle-events excepts
                                                  fd+manager-list
                                                  i/o-manager-exception-handler)
                                   i/o-manager-read-handler)))))))


;;;
;;; High-level aids.
;;;

(define (serve-one-stream-request program port)
  "Serve one RPC for @var{program}, reading the RPC from @var{port} (using
the record-marking protocol) and writing the reply to @var{port}.  If
@var{port} is closed or the end-of-file was reached, an
@code{&rpc-connection-lost-error} is raised."
  (let ((input-port (rpc-record-marking-input-port port)))

    (catch 'system-error
      (lambda ()
        (if (eof-object? (lookahead-u8 port))
            (raise (condition (&rpc-connection-lost-error
                               (port port))))
            (let* ((msg  (xdr-decode rpc-message input-port))
                   (call (procedure-call-information msg)))
              (handle-procedure-call call (list program) input-port
                                     (lambda (bv offset count)
                                       (send-rpc-record port bv
                                                        offset count))))))
      (lambda (key . args)
        (raise (condition (&rpc-connection-lost-error
                           (port port))))))))

(define-record-type <stream-connection>
  (make-stream-connection port address rpc-program)
  stream-connection?
  (port        stream-connection-port)
  (address     stream-connection-peer-address)
  (rpc-program stream-connection-rpc-program))

(define current-stream-connection
  ;; A fluid that is parameterized upon connection.
  (make-parameter #f))


(define (make-rpc-connection-i/o-manager program conn close-connection-proc)
  "Return a new I/O manager for a connected socket, represented by stream
connection @var{conn} and serving RPC program @var{program}.  The returned
I/O manager dispatches RPC calls to the procedures of @var{program} and
parameterizes @code{current-stream-connection} before actually invoking RPC
procedure handlers.  If @var{close-connection-proc} is a procedure, it is
called upon connection termination."
  (define (close-conn socket)
    (if (procedure? close-connection-proc)
        (close-connection-proc conn))
    (false-if-exception (close socket))
    ;; Return `#f' so that the connection is removed.
    #f)

  (make-i/o-manager (lambda (socket)
                      ;; Exception handler: remove the connection.
                      (close-conn socket))

                    (lambda (socket)
                      ;; Read handler: process a request.
                      (guard (c ((or (rpc-server-error? c)
                                     (xdr-error? c))
                                 ;; discard the connection
                                 (close-conn socket)))
                             (parameterize ((current-stream-connection conn))
                               (serve-one-stream-request program socket))

                             #t))))

(define (make-rpc-listening-socket-i/o-manager program close-connection-proc)
  "Return a new I/O manager for a listening socket serving RPC program
@var{program}.  The returned I/O manager will produce I/O managers for its
connections using @code{make-rpc-connection-i/o-manager}.  If
@var{close-connection-proc} is a procedure, it will be called upon connection
termination."
  (make-i/o-manager (lambda (socket)
                      ;; Exception handler: remove the connection.
                      (false-if-exception (close socket))
                      #f)

                    (lambda (socket)
                      ;; Read handler: initiate a new connection.
                      (let* ((accepted (accept socket))
                             (port     (car accepted))
                             (address  (cdr accepted))
                             (conn     (make-stream-connection port address
                                                               program)))
                        (cons port
                              (make-rpc-connection-i/o-manager
                                 program conn close-connection-proc))))))


(define (run-stream-rpc-server sockets+rpc-programs timeout
                               close-connection-proc idle-thunk)
  "Run a full-blown connection-oriented (i.e., @code{SOCK_STREAM}, be it
@code{PF_UNIX} or @code{PF_INET}) RPC server for the given listening sockets
and RPC programs.  @var{sockets+rpc-programs} should be a list of listening
socket-RPC program pairs (where ``RPC programs'' are objects as returned by
@code{make-rpc-program}).  @var{timeout} should be a number of microseconds
that the loop should wait for input; when no input is available,
@var{idle-thunk} is invoked, thus at most every @var{timeout} microseconds.
If @var{close-connection-proc} is a procedure, it is called when a connection
is being closed is passed the corresponding @code{<stream-connection>} object."

  (define (socket+rpc-program->socket+handlers p)
    (let ((socket  (car p))
          (program (cdr p)))
      (cons socket
            (make-rpc-listening-socket-i/o-manager program
                                                   close-connection-proc))))

  (run-input-event-loop (map socket+rpc-program->socket+handlers
                             sockets+rpc-programs)
                        timeout
                        idle-thunk))

;; Kept for compatibility with Guile-RPC 0.0.
(define serve-one-tcp-request       serve-one-stream-request)
(define run-tcp-rpc-server          run-stream-rpc-server)
(define current-tcp-connection      current-stream-connection)
(define tcp-connection?             stream-connection?)
(define tcp-connection-port         stream-connection-port)
(define tcp-connection-rpc-program  stream-connection-rpc-program)
(define tcp-connection-peer-address stream-connection-peer-address)


;;; server.scm ends here

;;; arch-tag: 60ab6723-b30d-4c60-809c-f1e54c6c6ac9
