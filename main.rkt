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

(define (generate-symbol s)
  (string->symbol (symbol->string (gensym s))))

(define (in-sequence-from seq start)
  (let ((index (box start))
        (len (sequence-length seq))
        (Nil (generate-symbol 'Nil)))
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
     (let*-values (((len) (vector-length values-vector))
                   ((more? get) (sequence-generate seq2))
                   ((Nil) (generate-symbol 'Nil))
                   ((state) (vector 0 0 Nil)))
       (define-syntax-rule (define-state name location)
         (define name (case-lambda (() (vector-ref state location))
                                   ((v) (vector-set! state location v)))))
       (define-state i 0)
       (define-state j 1)
       (define-state stage 2)
       (let loop ()
         (define old-i (i))
         (define old-j (j))
         (cond ((= old-j len) (cons (- old-i old-j) old-i))
               ((not (more?)) #f)
               (else
                (define new (let ((v (stage))) (if (eq? v Nil) (get) v)))
                (cond ((elem=? new (sequence-ref elements old-j))
                       (i (add1 old-i)) (j (add1 old-j)) (stage Nil))
                      ((zero? old-j)
                       (i (add1 old-i)) (stage Nil))
                      (else (j (vector-ref values-vector (sub1 old-j))) (stage new)))
                (loop))))))))

(module+ test
  (define table (make-PMT (in-string "abababca") char=?))
  (check-equal? (PMT-values-vector table) (vector 0 0 1 2 3 4 0 1))
  (check-exn exn:fail:contract? (lambda () (make-PMT (in-string "") char=?)))
  (check-equal? (cons 2 10) (PMT-match table (in-string "ababababca"))))

(define (make-KMP-matcher seq elem=?)
  (define PMT (make-PMT seq elem=?))
  (lambda (seq2)
    (PMT-match PMT seq2)))

(module+ test
  (define hello-world-matcher (make-KMP-matcher "Hello, World!" char-ci=?))
  (check-equal? (hello-world-matcher "Antigen-1: hello, world!") (cons 11 24))
  (check-false (hello-world-matcher "Antigen-1: hello, world")))

(module* contract-test racket/base
  (require (submod "..") rackunit)
  (displayln "Match string1 in string2 (with contracts)")
  (displayln "string1 : 1000 * #\\a + 20 * #\\b")
  (displayln "string2 : 500 * #\\a + 20 * #\\b + 2000 * #\\a + 10 * #\\b + 1500 * #\\a + 40 * #\\b")
  (define string1 (string-append (make-string 1000 #\a) (make-string 20 #\b)))
  (define string2 (string-append (make-string 500 #\a) (make-string 20 #\b) (make-string 2000 #\a) (make-string 10 #\b) (make-string 1500 #\a) (make-string 40 #\b)))
  (displayln "Baseline (using `regexp-match-positions`)")
  (displayln "1. Compile:")
  (define pattern (time (regexp string1)))
  (displayln "2. Match:")
  (time (regexp-match-positions pattern string2))
  (displayln "My KMP Implementation")
  (displayln "1. Compile:")
  (define matcher (time (make-KMP-matcher string1 char=?)))
  (displayln "2. Match:")
  (check-equal? (cons 3030 4050) (time (matcher string2))))

(module+ test
  (require (submod ".." contract-test)))
