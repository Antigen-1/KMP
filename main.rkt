#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/contract racket/sequence racket/match)
(provide (contract-out (make-KMP-matcher (-> sequence? (-> any/c any/c boolean?) (-> sequence? any)))))

(define (in-sequence-from seq start)
  (let ((index (box start))
        (len (sequence-length seq))
        (Nil (string->symbol (symbol->string (gensym 'Nil)))))
    (in-producer
     (lambda ()
       (define old-index (unbox index))
       (cond ((> len old-index) (set-box! index (add1 old-index))
                                (sequence-ref seq old-index))
             (else Nil)))
     Nil)))

(module+ test
  (for ((v1 (in-sequence-from (in-range 0 10) 1))
        (v2 (in-range 1 10)))
    (check-true (= v1 v2))))

;;PMT is a partial match table represented as a structure containing a sequence, a predicate and a vector
(struct PMT (elements elem=? values-vector))
(define (make-PMT seq elem=?)
  (let ((len (sequence-length seq)))
    (if (zero? len)
        (raise-result-error 'make-PMT "sequence" "starting" 0 seq 0 -1)
        (PMT
         seq
         elem=?
         (let loop ((r '(0)) (n 1))
           (cond ((= n len) (list->vector (reverse r)))
                 (else
                  (let/cc break
                    (call-with-values
                     (lambda ()
                       (for/fold ((l null) (c 0)) ((e1 (in-sequence-from seq n)) (e2 seq))
                         (define new-c (add1 c))
                         (if (elem=? e1 e2) (values (cons new-c l) new-c) (break (loop (append (cons 0 l) r) (+ n new-c))))))
                     (lambda (l _) (loop (append l r) len)))))))))))
(define (PMT-match table seq2)
  (match table
    ((PMT elements elem=? values-vector)
     (let ((len (vector-length values-vector))
           (edge (sequence-length seq2)))
       (let loop ((i 0) (j 0))
         (cond ((= j len) (- i j))
               ((>= i edge) #f)
               (else
                (let/cc break
                  (call-with-values
                   (lambda ()
                     (for/fold ((i i) (j j))
                               ((e1 (in-sequence-from elements j))
                                (e2 (in-sequence-from seq2 i)))
                       (cond ((elem=? e1 e2) (values (add1 i) (add1 j)))
                             ((zero? j) (break (loop (add1 i) 0))) ;; value = -1
                             (else (break (loop i (vector-ref values-vector (sub1 j))))))))
                   loop)))))))))

(module+ test
  (define table (make-PMT (in-string "abababca") char=?))
  (check-equal? (PMT-values-vector table) (vector 0 0 1 2 3 4 0 1))
  (check-exn exn:fail:contract? (lambda () (make-PMT (in-string "") char=?)))
  (check-true (= 2 (PMT-match table (in-string "ababababca")))))

(define (make-KMP-matcher seq elem=?)
  (define PMT (make-PMT seq elem=?))
  (lambda (seq2)
    (PMT-match PMT seq2)))

(module+ test
  (define hello-world-matcher (make-KMP-matcher "Hello, World!" char-ci=?))
  (check-true (= (hello-world-matcher "Antigen-1: hello, world!") 11))
  (check-false (hello-world-matcher "Antigen-1: hello, world")))

(module* contract-test racket/base
  (require (submod "..") rackunit)
  (displayln "Match string1 in string2 (with contracts)")
  (displayln "string1 : 1000 * #\\a + 20 * #\\b")
  (displayln "string2 : 500 * #\\a + 20 * #\\b + 2000 * #\\a + 10 * #\\b + 1500 * #\\a + 40 * #\\b")
  (define string1 (string-append (make-string 1000 #\a) (make-string 20 #\b)))
  (define string2 (string-append (make-string 500 #\a) (make-string 20 #\b) (make-string 2000 #\a) (make-string 10 #\b) (make-string 1500 #\a) (make-string 40 #\b)))
  (displayln "1. Compile:")
  (define matcher (time (make-KMP-matcher string1 char=?)))
  (displayln "2. Match:")
  (check-true (= 3030 (time (matcher string2)))))

(module+ test
  (require (submod ".." contract-test)))
