;;; test-lorem-optimum-benchmark.el --- Performance tests for lorem-optimum.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Benchmark and performance tests for the Markov chain implementation.
;;
;; These tests measure:
;; - Learning time scaling with input size
;; - Multiple learning operations (exposes key rebuild overhead)
;; - Generation time scaling
;; - Memory usage (hash table growth)
;;
;; Performance baseline targets (on modern hardware):
;; - Learn 1000 words: < 10ms
;; - Learn 10,000 words: < 100ms
;; - 100 learn operations of 100 words each: < 500ms (current bottleneck!)
;; - Generate 100 words: < 5ms

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

(ert-deftest benchmark-learn-1k-words ()
  "Benchmark learning 1000 words."
  (let* ((text (generate-test-text 1000))
         (chain (cj/markov-chain-create))
         (time (benchmark-time
                (lambda () (cj/markov-learn chain text)))))
    (benchmark-report "Learn 1K words" time)
    (should (< time 50.0))))  ; Should be < 50ms

(ert-deftest benchmark-learn-10k-words ()
  "Benchmark learning 10,000 words.
DISABLED: Takes too long (minutes instead of seconds).
Needs lorem-optimum performance optimization before re-enabling."
  :tags '(:slow)
  (let* ((text (generate-test-text 10000))
         (chain (cj/markov-chain-create))
         (time (benchmark-time
                (lambda () (cj/markov-learn chain text)))))
    (benchmark-report "Learn 10K words" time)
    (should (< time 500.0))))  ; Should be < 500ms

(ert-deftest benchmark-learn-100k-words ()
  "Benchmark learning 100,000 words (stress test)."
  :tags '(:slow)
  (let* ((text (generate-test-text 100000))
         (chain (cj/markov-chain-create))
         (time (benchmark-time
                (lambda () (cj/markov-learn chain text)))))
    (benchmark-report "Learn 100K words" time)
    ;; This may be slow due to key rebuild
    (message "Hash table size: %d bigrams"
             (hash-table-count (cj/markov-chain-map chain)))))

;;; Multiple Learning Operations (Exposes Quadratic Behavior)

(ert-deftest benchmark-multiple-learns-10x100 ()
  "Benchmark 10 learn operations of 100 words each."
  (let ((chain (cj/markov-chain-create))
        (times '()))
    (dotimes (i 10)
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
      (message "Times: %S" (nreverse times))
      ;; Note: Watch if later operations are slower (quadratic behavior)
      (should (< total 100.0)))))  ; Total should be < 100ms

(ert-deftest benchmark-multiple-learns-100x100 ()
  "Benchmark 100 learn operations of 100 words each (key rebuild overhead)."
  :tags '(:slow)
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
          (first-10-avg (/ (apply #'+ (last times 10)) 10.0))
          (last-10-avg (/ (apply #'+ (seq-take times 10)) 10.0)))
      (benchmark-report "100x learn 100 words - TOTAL" total)
      (benchmark-report "100x learn 100 words - AVG" avg)
      (benchmark-report "100x learn - First 10 AVG" first-10-avg)
      (benchmark-report "100x learn - Last 10 AVG" last-10-avg)
      (message "Sampled times (iteration, ms): %S" (nreverse measurements))
      (message "Hash table size: %d bigrams"
               (hash-table-count (cj/markov-chain-map chain)))
      ;; This exposes the quadratic behavior: last operations much slower
      (when (> last-10-avg (* 2.0 first-10-avg))
        (message "WARNING: Learning slows down significantly over time!")
        (message "  First 10 avg: %.2f ms" first-10-avg)
        (message "  Last 10 avg: %.2f ms" last-10-avg)
        (message "  Ratio: %.1fx slower" (/ last-10-avg first-10-avg))))))

;;; Generation Performance Tests

(ert-deftest benchmark-generate-100-words ()
  "Benchmark generating 100 words."
  (let* ((text (generate-test-text 1000))
         (chain (cj/markov-chain-create)))
    (cj/markov-learn chain text)
    (let ((time (benchmark-time
                 (lambda () (cj/markov-generate chain 100)))))
      (benchmark-report "Generate 100 words" time)
      (should (< time 20.0)))))  ; Should be < 20ms

(ert-deftest benchmark-generate-1000-words ()
  "Benchmark generating 1000 words."
  (let* ((text (generate-test-text 10000))
         (chain (cj/markov-chain-create)))
    (cj/markov-learn chain text)
    (let ((time (benchmark-time
                 (lambda () (cj/markov-generate chain 1000)))))
      (benchmark-report "Generate 1000 words" time)
      (should (< time 100.0)))))  ; Should be < 100ms

;;; Tokenization Performance Tests

(ert-deftest benchmark-tokenize-10k-words ()
  "Benchmark tokenizing 10,000 words.
DISABLED: Takes too long (minutes instead of seconds).
Needs lorem-optimum performance optimization before re-enabling."
  :tags '(:slow)
  (let* ((text (generate-test-text 10000))
         (time (benchmark-time
                (lambda () (cj/markov-tokenize text)))))
    (benchmark-report "Tokenize 10K words" time)
    (should (< time 50.0))))  ; Tokenization should be fast

;;; Memory/Size Tests

(ert-deftest benchmark-chain-growth ()
  "Measure hash table growth with increasing input."
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
  "Compare tokenization time to total learning time."
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
  "Benchmark realistic usage: learn from multiple sources, generate paragraphs."
  (let ((chain (cj/markov-chain-create))
        (learn-total 0.0)
        (gen-total 0.0))
    ;; Simulate learning from 10 different sources
    (dotimes (i 10)
      (let ((text (generate-test-text 500)))
        (setq learn-total
              (+ learn-total
                 (benchmark-time (lambda () (cj/markov-learn chain text)))))))

    ;; Generate 5 paragraphs
    (dotimes (i 5)
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
