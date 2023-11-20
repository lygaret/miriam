(define (signed-x? bitfield)
  (lambda (number)
    (and (exact-integer? number)
         (let ((tester (bitwise-and number bitfield)))
           (or (zero? tester) (= tester bitfield))))))

(define (unsigned-x? bitfield)
  (lambda (number)
    (and (exact-integer? number)
         (= bitfield (bitwise-ior number bitfield)))))

;; does the given number fit in an signed/unsigned of a given size?

(define s-byte-x?  (signed-x? (- (expt 2 7))))
(define s-hword-x? (signed-x? (- (expt 2 15))))
(define s-word-x?  (signed-x? (- (expt 2 31))))
(define s-dword-x? (signed-x? (- (expt 2 63))))

(define u-byte-x?  (unsigned-x? (- (expt 2 8) 1)))
(define u-hword-x? (unsigned-x? (- (expt 2 16) 1)))
(define u-word-x?  (unsigned-x? (- (expt 2 32) 1)))
(define u-dword-x? (unsigned-x? (- (expt 2 64) 1)))

;; sized num constructors

(define (num-x pred key)
  (lambda (x)
    (or (and (pred x) x)
        (and (simple-pair? x key) (pred (cadr x)) (cadr x)))))

(define s-byte  (num-x s-byte-x?  'byte))
(define s-hword (num-x s-hword-x? 'hword))
(define s-word  (num-x s-word-x?  'word))
(define s-dword (num-x s-dword-x? 'dword))

(define u-byte  (num-x u-byte-x?  'byte))
(define u-hword (num-x u-hword-x? 'hword))
(define u-word  (num-x u-word-x?  'word))
(define u-dword (num-x u-dword-x? 'dword))

(define (u/s-byte x)  (or (u-byte x)  (s-byte x)))
(define (u/s-hword x) (or (u-hword x) (s-hword x)))
(define (u/s-word x)  (or (u-word x)  (s-word x)))
(define (u/s-dword x) (or (u-dword x) (s-dword x)))

;; little endian byte-lists

(define (number->bytelist number elem-size)
  (if (integer? number)
      (integer->bytelist number elem-size)
      (if (real? number)
          (cond ((= 4 elem-size) (float32->bytelist number))
                ((= 8 elem-size) (float64->bytelist number))
                (else (error "bad size for a float" number elem-size)))
          (error "bad number type for assembler" number))))

(define (integer->bytelist init-int init-size)
  (let loop ((int  init-int)
             (size init-size))
    (if (zero? size)
        (if (or (zero? init-int)
                (and (positive? init-int) (zero? int))
                (and (negative? init-int) (= -1 int)))
            '()
            (error "integer too big for field width" init-int init-size))
        (cons (bitwise-and int #xFF)
              (loop (arithmetic-shift int -8) (- size 1))))))

;;; The procedures float32->byte-list and float64->byte-list are
;;; based on write-ieee-float32 and write-ieee-float64
;;; from SRFI-56 (withdrawn) by Alex Shinn and distributed under the
;;; following license.  http://srfi.schemers.org

;;; Copyright (c) 2004-2005 by Alex Shinn. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(define (float32->bytelist float)
  (if (zero? float)
      (make-list 4 0)
      (let loop ((temp-mantissa (inexact (if (negative? float) (- float) float)))
		 (exponent      0))
	(cond ((>= temp-mantissa (expt 2 24)) (loop (/ temp-mantissa 2) (+ exponent 1)))
	      ((<  temp-mantissa (expt 2 23)) (loop (* temp-mantissa 2) (- exponent 1)))
	      (else (let ((mantissa   (exact (round temp-mantissa)))
		          (biased-exp (+ exponent 23 127)))
		      (cond ((negative? biased-exp)
			     (let ((f1 (exact (round (* mantissa (expt 2 (- biased-exp 1)))))))
			       (list (bit-field f1 0 8)
				     (bit-field f1 8 16)
				     (bit-field f1 16 23)
				     (if (negative? float) #x80 0))))
		            ((> biased-exp 255)
			     (list 0 0 128 (if (negative? float) 255 127)))
		            (else (list (bit-field mantissa 0 8)
				        (bit-field mantissa 8 16)
				        (bitwise-ior (if (odd? biased-exp) 128 0)
					             (bit-field mantissa 16 23))
				        (if (negative? float)
				            (bitwise-ior (arithmetic-shift biased-exp -1) 128)
				            (arithmetic-shift biased-exp -1)))))))))))

(define (float64->bytelist float)
  (if (zero? float)
      (make-list 8 0)
      (let loop ((temp-mantissa (inexact (if (negative? float) (- float) float)))
		 (exponent      0))
	(cond ((>= temp-mantissa (expt 2 53)) (loop (/ temp-mantissa 2) (+ exponent 1)))
	      ((<  temp-mantissa (expt 2 52)) (loop (* temp-mantissa 2) (- exponent 1)))
	      (else (let ((mantissa (exact (round temp-mantissa)))
		          (biased-exp (+ exponent 52 1023)))
		      (cond ((negative? biased-exp)
			     (let ((f1 (exact (round (* mantissa (expt 2 (- biased-exp 1)))))))
			       (list (bit-field f1 0   8)
				     (bit-field f1 8  16)
				     (bit-field f1 16 24)
				     (bit-field f1 24 32)
				     (bit-field f1 32 40)
				     (bit-field f1 40 48)
				     (bit-field f1 48 53)
				     (if (negative? float) 128 0))))
		            ((> biased-exp 4095)
			     (list 0 0 0 0 0 0 224 (if (negative? float) 255 127)))
		            (else (list (bit-field mantissa 0  8)
				        (bit-field mantissa 8  16)
				        (bit-field mantissa 16 24)
				        (bit-field mantissa 24 32)
				        (bit-field mantissa 32 40)
				        (bit-field mantissa 40 48)
				        (bitwise-ior (arithmetic-shift (bit-field biased-exp 0 4) 4)
					        (bit-field mantissa 48 52))
				        (if (negative? float)
				            (+ (bit-field biased-exp 4 11) 128)
				            (bit-field biased-exp 4 11)))))))))))
