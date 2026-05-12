;;; test-lorem-optimum-benchmark.el --- Performance benchmarks for lorem-optimum.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Benchmarks for the order-two Markov chain in `lorem-optimum.el'.
;;
;; Every test here is tagged `:perf', so `make test', `make coverage', and the
;; PostToolUse test hook skip them.  Run them deliberately:
;;
;;   make benchmark
;;
;; The tests print timings (learning, generation, tokenization, hash-table
;; growth) for eyeballing.  The only assertions are machine-independent
;; *ratios*: `benchmark-learn-scaling' fails if learn time grows faster than
;; roughly linearly with input size.  A slow CI runner or an old laptop won't
;; trip that — an O(N^2) regression will.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load the module
(require 'lorem-optimum)

;;; Benchmark Helpers

(defun benchmark-time (func)
  "Time execution of FUNC and return milliseconds."
  (let ((start (current-time)))
    (funcall func)
    (let ((end (current-time)))
      (* 1000.0 (float-time (time-subtract end start))))))

(defun generate-test-text (word-count)
  "Generate WORD-COUNT words of test text with some repetition."
  (let ((words '("lorem" "ipsum" "dolor" "sit" "amet" "consectetur"
                 "adipiscing" "elit" "sed" "do" "eiusmod" "tempor"
                 "incididunt" "ut" "labore" "et" "dolore" "magna" "aliqua"))
        (result '()))
    (dotimes (i word-count)
      (push (nth (mod i (length words)) words) result)
      (when (zerop (mod i 10))
        (push "." result)))
    (mapconcat #'identity (nreverse result) " ")))

(defun benchmark-report (name time-ms)
  "Report benchmark NAME with TIME-MS."
  (message "BENCHMARK [%s]: %.2f ms" name time-ms))

;;; Learning Performance Tests

(ert-deftest benchmark-learn-scaling ()
  "Learn time scales no worse than ~linearly with input size.
Reports 1K/10K/100K learn times and the hash-table size, then asserts each
10x jump in input costs less than 40x the time — generous enough that GC
pauses and a slow machine pass, tight enough that O(N^2) fails."
  :tags '(:perf)
  (let* ((t1k (benchmark-time
               (lambda () (cj/markov-learn (cj/markov-chain-create)
                                           (generate-test-text 1000)))))
         (t10k (benchmark-time
                (lambda () (cj/markov-learn (cj/markov-chain-create)
                                            (generate-test-text 10000)))))
         (chain-100k (cj/markov-chain-create))
         (t100k (benchmark-time
                 (lambda () (cj/markov-learn chain-100k
                                             (generate-test-text 100000))))))
    (benchmark-report "Learn 1K words" t1k)
    (benchmark-report "Learn 10K words" t10k)
    (benchmark-report "Learn 100K words" t100k)
    (message "Hash table size after 100K words: %d bigrams"
             (hash-table-count (cj/markov-chain-map chain-100k)))
    (should (> t1k 0.0))
    (should (< t10k (* 40.0 t1k)))
    (should (< t100k (* 40.0 t10k)))))

;;; Multiple Learning Operations

(ert-deftest benchmark-multiple-learns-10x100 ()
  "Report 10 learn operations of 100 words each."
  :tags '(:perf)
  (let ((chain (cj/markov-chain-create))
        (times '()))
    (dotimes (_ 10)
      (let* ((text (generate-test-text 100))
             (time (benchmark-time
                    (lambda () (cj/markov-learn chain text)))))
        (push time times)))
    (let ((total (apply #'+ times))
          (avg (/ (apply #'+ times) 10.0))
          (max-time (apply #'max times)))
      (benchmark-report "10x learn 100 words - TOTAL" total)
      (benchmark-report "10x learn 100 words - AVG" avg)
      (benchmark-report "10x learn 100 words - MAX" max-time)
      (message "Times: %S" (nreverse times)))))

(ert-deftest benchmark-multiple-learns-100x100 ()
  "Report 100 learn operations of 100 words each, sampling for slowdown.
With the O(1) word-vector access and lazy key rebuild, per-chunk time should
stay flat across the run; a growing first-10-vs-last-10 gap means a
regression to quadratic behavior."
  :tags '(:perf)
  (let ((chain (cj/markov-chain-create))
        (times '())
        (measurements '()))
    (dotimes (i 100)
      (let* ((text (generate-test-text 100))
             (time (benchmark-time
                    (lambda () (cj/markov-learn chain text)))))
        (push time times)
        ;; Sample measurements every 10 iterations
        (when (zerop (mod i 10))
          (push (cons i time) measurements))))
    (let ((total (apply #'+ times))
          (avg (/ (apply #'+ times) 100.0))
          ;; `times' is built with `push', so the tail holds the first
          ;; iterations and the head holds the last ones.
          (first-10-avg (/ (apply #'+ (last times 10)) 10.0))
          (last-10-avg (/ (apply #'+ (seq-take times 10)) 10.0)))
      (benchmark-report "100x learn 100 words - TOTAL" total)
      (benchmark-report "100x learn 100 words - AVG" avg)
      (benchmark-report "100x learn - First 10 AVG" first-10-avg)
      (benchmark-report "100x learn - Last 10 AVG" last-10-avg)
      (message "Sampled times (iteration, ms): %S" (nreverse measurements))
      (message "Hash table size: %d bigrams"
               (hash-table-count (cj/markov-chain-map chain)))
      (when (> last-10-avg (* 2.0 first-10-avg))
        (message "WARNING: learning slows down over time! %.1fx (%.2f -> %.2f ms)"
                 (/ last-10-avg first-10-avg) first-10-avg last-10-avg)))))

;;; Generation Performance Tests

(ert-deftest benchmark-generate-100-words ()
  "Report time to generate 100 words."
  :tags '(:perf)
  (let ((chain (cj/markov-chain-create)))
    (cj/markov-learn chain (generate-test-text 1000))
    (benchmark-report "Generate 100 words"
                      (benchmark-time
                       (lambda () (cj/markov-generate chain 100))))))

;;; Tokenization Performance Tests

(ert-deftest benchmark-tokenize-10k-words ()
  "Report time to tokenize 10,000 words."
  :tags '(:perf)
  (let ((text (generate-test-text 10000)))
    (benchmark-report "Tokenize 10K words"
                      (benchmark-time
                       (lambda () (cj/markov-tokenize text))))))

;;; Memory/Size Tests

(ert-deftest benchmark-chain-growth ()
  "Report hash-table growth with increasing input."
  :tags '(:perf)
  (let ((chain (cj/markov-chain-create))
        (sizes '()))
    (dolist (word-count '(100 500 1000 5000 10000))
      (let ((text (generate-test-text word-count)))
        (cj/markov-learn chain text)
        (let ((size (hash-table-count (cj/markov-chain-map chain))))
          (push (cons word-count size) sizes)
          (message "After %d words: %d unique bigrams" word-count size))))
    (message "Growth pattern: %S" (nreverse sizes))))

;;; Comparison: Tokenization vs Learning

(ert-deftest benchmark-tokenize-vs-learn ()
  "Report tokenization time as a fraction of total learning time."
  :tags '(:perf)
  (let* ((text (generate-test-text 5000))
         (tokenize-time (benchmark-time
                         (lambda () (cj/markov-tokenize text))))
         (chain (cj/markov-chain-create))
         (learn-time (benchmark-time
                      (lambda () (cj/markov-learn chain text)))))
    (benchmark-report "Tokenize 5K words" tokenize-time)
    (benchmark-report "Learn 5K words (total)" learn-time)
    (message "Tokenization is %.1f%% of total learning time"
             (* 100.0 (/ tokenize-time learn-time)))))

;;; Real-world Scenario

(ert-deftest benchmark-realistic-usage ()
  "Report a realistic mix: learn from several sources, then generate paragraphs."
  :tags '(:perf)
  (let ((chain (cj/markov-chain-create))
        (learn-total 0.0)
        (gen-total 0.0))
    ;; Simulate learning from 10 different sources
    (dotimes (_ 10)
      (let ((text (generate-test-text 500)))
        (setq learn-total
              (+ learn-total
                 (benchmark-time (lambda () (cj/markov-learn chain text)))))))
    ;; Generate 5 paragraphs
    (dotimes (_ 5)
      (setq gen-total
            (+ gen-total
               (benchmark-time (lambda () (cj/markov-generate chain 50))))))
    (benchmark-report "Realistic: 10 learns (500 words each)" learn-total)
    (benchmark-report "Realistic: 5 generations (50 words each)" gen-total)
    (benchmark-report "Realistic: TOTAL TIME" (+ learn-total gen-total))
    (message "Final chain size: %d bigrams"
             (hash-table-count (cj/markov-chain-map chain)))))

(provide 'test-lorem-optimum-benchmark)
;;; test-lorem-optimum-benchmark.el ends here
