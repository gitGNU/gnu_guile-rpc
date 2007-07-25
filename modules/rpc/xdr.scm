;;; Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Guile-RPC is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; Guile-RPC is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Guile-RPC; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(define-module (rpc xdr)
  :use-module (srfi srfi-9)
  :autoload   (srfi srfi-34)   (raise)
  :use-module (srfi srfi-35)
  :use-module (r6rs bytevector)
  :autoload   (r6rs i/o ports) (get-bytevector-n)

  :export (make-xdr-basic-type xdr-basic-type?
           xdr-basic-type-name xdr-basic-type-size
           xdr-basic-type-encoder xdr-basic-type-decoder

           make-xdr-vector-type xdr-vector-type?
           xdr-vector-base-type xdr-vector-max-element-count

           make-xdr-struct-type xdr-struct-type?
           xdr-struct-base-types

           make-xdr-union-type xdr-union-type?
           xdr-union-discriminant-type
           xdr-union-discriminant/type-alist
           xdr-union-default-type
           xdr-union-arm-type

           xdr-type-size xdr-encode! xdr-decode
           %xdr-endianness

           &xdr-error xdr-error?
           &xdr-type-error xdr-type-error? xdr-type-error:type
           &xdr-input-type-error xdr-input-type-error?
           xdr-input-type-error:value
           &xdr-unknown-type-error xdr-unknown-type-error?
           &xdr-union-discriminant-error xdr-union-discriminant-error?
           &xdr-vector-size-exceeded-error xdr-vector-size-exceeded-error?
           xdr-vector-size-exceeded-error:element-count))


;;; Author: Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Commentary:
;;;
;;; A framework to describe the data types defined in RFC 4506.
;;;
;;; Code:



;;;
;;; Major kinds of types.
;;;

(define-record-type <xdr-basic-type>
  ;; Basic fixed-size XDR types.
  (%make-xdr-basic-type name size type-pred encoder decoder vector-decoder)
  xdr-basic-type?
  (name           xdr-basic-type-name)
  (size           xdr-basic-type-size)         ;; encoded size (in octets)
  (type-pred      xdr-basic-type-type-pred)    ;; input type predicate
  (encoder        xdr-basic-type-encoder)
  (decoder        xdr-basic-type-decoder)
  (vector-decoder xdr-basic-type-vector-decoder))

(define-record-type <xdr-vector-type>
  ;; Variable-length arrays (Sections 4.13 and 4.10).
  (make-xdr-vector-type base-type max-element-count)
  xdr-vector-type?
  (base-type         xdr-vector-base-type)
  (max-element-count xdr-vector-max-element-count))

(define-record-type <xdr-struct-type>
  ;; Fixed-length arrays and structures (Sections 4.12 and 4.14).
  (%make-xdr-struct-type base-types size padding)
  xdr-struct-type?
  (base-types   xdr-struct-base-types) ;; list of base types
  (size         xdr-struct-size)       ;; size in octets (if fixed-length)
  (padding      xdr-struct-padding))   ;; padding required (if fixed-length)

(define-record-type <xdr-union-type>
  ;; Discriminated unions (Section 4.15).
  (%make-xdr-union-type discriminant-type discriminant/type-alist
                        default-type)
  xdr-union-type?
  (discriminant-type        xdr-union-discriminant-type)
  (discriminant/type-alist  xdr-union-discriminant/type-alist)
  (default-type             xdr-union-default-type))


;;;
;;; Error conditions.
;;;

(define-condition-type &xdr-error &error
  xdr-error?)

(define-condition-type &xdr-type-error &error
  xdr-type-error?
  (type      xdr-type-error:type))

(define-condition-type &xdr-unknown-type-error &xdr-type-error
  xdr-unknown-type-error?)

(define-condition-type &xdr-input-type-error &xdr-type-error
  xdr-input-type-error?
  (value     xdr-input-type-error:value))

(define-condition-type &xdr-union-discriminant-error &xdr-type-error
  xdr-union-discriminant-error?)

(define-condition-type &xdr-vector-size-exceeded-error &xdr-type-error
  xdr-vector-size-exceeded-error?
  (element-count xdr-vector-size-exceeded-error:element-count))



;;;
;;; Helpers.
;;;

(define (make-xdr-basic-type name size type-pred
                             encoder decoder . vector-decoder)
  "Return a new basic XDR type.  If @var{vector-decoder} is provided, then it
should be a procedure that will be used to efficiently decode vectors of that
type."
  (let ((vector-decoder (if (null? vector-decoder)
                            '(#f)
                            vector-decoder)))
    (apply %make-xdr-basic-type name size type-pred encoder decoder
           vector-decoder)))

;; Section 3: the "basic block size" is 32 bits.
(define %xdr-atom-size 4)

(define (round-up-size size)
  "Round up @var{size} so that it fits into a 32-bit XDR basic block size."
  (let ((rem (modulo size %xdr-atom-size)))
    (if (= 0 rem)
        size
        (+ size (- %xdr-atom-size rem)))))

(define (make-xdr-struct-type base-types)
  "Return a new XDR struct type and pre-compute its size if possible."
  (let loop ((types base-types)
             (size  0))
    (if (null? types)
        (let* ((rem (modulo size %xdr-atom-size))
               (padding (if (= 0 rem) 0 (- %xdr-atom-size rem))))
          (%make-xdr-struct-type base-types (+ size padding) padding))
        (let ((type (car types)))
          (cond ((xdr-basic-type? type)
                 (loop (cdr types)
                       (+ size (xdr-basic-type-size type))))
                ((xdr-struct-type? type)
                 (let ((s (xdr-struct-size type)))
                   (if (number? s)
                       (loop (cdr types)
                             (+ size s))
                       (%make-xdr-struct-type base-types #f #f))))
                (else
                 ;; Cannot determine struct size statically.
                 (%make-xdr-struct-type base-types #f #f)))))))

(define (make-xdr-union-type discr-type discr/type-alist default-type)
  "Return a new XDR discriminated union type, using @var{discr-type} as the
discriminant type (which must be a 32-bit basic type) and
@var{discr/type-alist} to select the ``arm'' type depending on the
discriminant value.  If no suitable value is found in @var{discr/type-alist}
and @var{default-type} is not @code{#f}, then default type is used as the arm
type."
  (if (and (xdr-basic-type? discr-type)
           (= 4 (xdr-basic-type-size discr-type))
           (list? discr/type-alist))
      (%make-xdr-union-type discr-type discr/type-alist
                            default-type)
      (raise (condition
              (&xdr-union-discriminant-error (type discr-type))))))

(define (xdr-union-arm-type union discriminant)
  "Return the type that should be used for @var{union}'s arm given
 @var{discriminant} (a Scheme value)."
  (let ((arm-type (assoc discriminant
                         (xdr-union-discriminant/type-alist union))))
    (if (pair? arm-type)
        (cdr arm-type)
        (or (xdr-union-default-type union)
            (raise (condition
                    (&xdr-input-type-error (type  union)
                                           (value discriminant))))))))


;;;
;;; Type size.
;;;

(define (xdr-type-size type value)
  "Return the size (in octets) of @var{type} when applied to @var{value}."

  (define (vector-map proc v)
    (let ((len (vector-length v)))
      (let loop ((i 0)
                 (result '()))
        (if (>= i len)
            result
            (loop (+ 1 i)
                  (cons (proc (vector-ref v i))
                        result))))))

  ;; We allow the size of basic types to not be a multiple of 4 and only
  ;; round up the size on structs and vectors.  This is so that we can, e.g.,
  ;; have an `xdr-single-opaque' type whose size is 1.  Thus, padding is only
  ;; performed for structs and vectors; all other types (unions and public
  ;; basic types) are assumed to be a multiple of 4.

  (let loop ((type type)
             (value value))
    (cond ((xdr-basic-type? type)
           (xdr-basic-type-size type))

          ((xdr-vector-type? type)
           (let ((base (xdr-vector-base-type type)))
             (round-up-size
              (apply + 4 ;; 4 octets to encode the length
                     (vector-map (lambda (value)
                                   (loop base value))
                                 value)))))

          ((xdr-struct-type? type)
           (or (xdr-struct-size type)
               (let ((types (xdr-struct-base-types type)))
                 (round-up-size (apply + (map loop types value))))))

          ((xdr-union-type? type)
           (let ((discr (car value)))
             (+ 4 (loop (xdr-union-arm-type type discr)
                        (cdr value)))))

          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


;;;
;;; Encoding.
;;;

(define %xdr-endianness
  ;; Section 3: All items (except opaque arrays) are encoded using this
  ;; endianness.
  (endianness big))

(define %max-vector-size
  ;; Section 4.13: Vector sizes are encoded on 32-bit, hence this limit.
  (- (expt 2 32) 1))


(define (xdr-encode! bv index type value)
  "Encode @var{value}, using XDR type @var{type}, into bytevector @var{bv} at
@var{index}.  Return the index where encoding ended."

  (define (type-error type value)
    (raise (condition (&xdr-input-type-error (type  type)
                                             (value value)))))

  (let loop ((type  type)
             (value value)
             (index index))
    (cond ((xdr-basic-type? type)
           (let ((valid?  (xdr-basic-type-type-pred type))
                 (encode! (xdr-basic-type-encoder type)))
             (if (not (valid? value)) (type-error type value))
             (encode! type value bv index)
             (+ index (xdr-basic-type-size type))))

          ((xdr-vector-type? type)
           (let ((base (xdr-vector-base-type type))
                 (len  (vector-length value))
                 (max  (xdr-vector-max-element-count type)))

             ;; check whether LEN exceeds the maximum element count
             (if (or (and max (> len max))
                     (> len %max-vector-size))
                 (raise (condition
                         (&xdr-vector-size-exceeded-error
                          (type type)
                          (element-count len)))))

             ;; encode the vector length.
             (bytevector-u32-set! bv index len %xdr-endianness)

             (let liip ((value-index 0)
                        (index       (+ 4 index)))
               (if (< value-index len)
                   (liip (+ 1 value-index)
                         (loop base
                               (vector-ref value value-index)
                               index))
                   (round-up-size index)))))

          ((xdr-struct-type? type)
           (let liip ((types  (xdr-struct-base-types type))
                      (values value)
                      (index  index))
             (if (null? values)
                 (round-up-size index)
                 (liip (cdr types)
                       (cdr values)
                       (loop (car types)
                             (car values)
                             index)))))

          ((xdr-union-type? type)
           (let ((discr (car value))
                 (arm   (cdr value)))
             (loop (xdr-union-discriminant-type type) discr index)
             ;; We can safely assume that the discriminant is 32-bit.
             (loop (xdr-union-arm-type type discr) arm (+ index 4))))

          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


;;;
;;; Decoding.
;;;

(define (xdr-decode type port)
  "Decode from @var{port} (a binary input port) a value of XDR type
@var{type}.  Return the decoded value."

  (define (vector-padding base-type count)
    (if (xdr-basic-type? base-type)
        (let ((rem (modulo (* count (xdr-basic-type-size base-type))
                           %xdr-atom-size)))
          (if (= 0 rem)
              0
              (- %xdr-atom-size rem)))
        0))

  (let loop ((type  type))
    (cond ((xdr-basic-type? type)
           (let ((decode (xdr-basic-type-decoder type)))
             (decode type port)))

          ((xdr-vector-type? type)
           (let* ((max    (xdr-vector-max-element-count type))
                  (type   (xdr-vector-base-type type))
                  (decode (and (xdr-basic-type? type)
                               (xdr-basic-type-vector-decoder type)))
                  (raw    (get-bytevector-n port 4))
                  (len    (bytevector-u32-ref raw 0 %xdr-endianness)))

             (if (and max (> len max))
                 (raise (condition
                         (&xdr-vector-size-exceeded-error
                          (type type)
                          (element-count len))))
                 (let ((padding (vector-padding type len))
                       (result
                        (if decode
                            (decode type len port)
                            (let ((vec (make-vector len)))
                              (let liip ((index 0))
                                (if (< index len)
                                    (let ((value (loop type)))
                                      (vector-set! vec index value)
                                      (liip (+ 1 index)))
                                    vec))))))

                   (if (> padding 0) (get-bytevector-n port padding))
                   result))))

          ((xdr-struct-type? type)
           (let liip ((types  (xdr-struct-base-types type))
                      (result '()))
             (if (null? types)
                 (let ((padding (or (xdr-struct-padding type) 0)))
                   (if (> padding 0) (get-bytevector-n port padding))
                   (reverse! result))
                 (let ((value (loop (car types))))
                   (liip (cdr types)
                         (cons value result))))))

          ((xdr-union-type? type)
           (let* ((discr (loop (xdr-union-discriminant-type type)))
                  (value (loop (xdr-union-arm-type type discr))))
             (cons discr value)))

          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; xdr.scm ends here
