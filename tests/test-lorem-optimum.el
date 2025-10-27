;;; test-lorem-optimum.el --- Tests for lorem-optimum.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for lorem-optimum.el Markov chain text generation.
;;
;; Tests cover:
;; - Tokenization
;; - Learning and chain building
;; - Text generation
;; - Capitalization fixing
;; - Token joining

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load the module
(require 'lorem-optimum)

;;; Test Helpers

(defun test-chain ()
  "Create a fresh test chain."
  (cj/markov-chain-create))

(defun test-learn (text)
  "Create a chain and learn TEXT."
  (let ((chain (test-chain)))
    (cj/markov-learn chain text)
    chain))

;;; Tokenization Tests

(ert-deftest test-tokenize-simple ()
  "Should tokenize simple words."
  (let ((result (cj/markov-tokenize "hello world")))
    (should (equal result '("hello" "world")))))

(ert-deftest test-tokenize-with-punctuation ()
  "Should separate punctuation as tokens."
  (let ((result (cj/markov-tokenize "Hello, world!")))
    (should (equal result '("Hello" "," "world" "!")))))

(ert-deftest test-tokenize-multiple-spaces ()
  "Should handle multiple spaces."
  (let ((result (cj/markov-tokenize "hello    world")))
    (should (equal result '("hello" "world")))))

(ert-deftest test-tokenize-newlines ()
  "Should handle newlines as whitespace."
  (let ((result (cj/markov-tokenize "hello\nworld")))
    (should (equal result '("hello" "world")))))

(ert-deftest test-tokenize-mixed-punctuation ()
  "Should tokenize complex punctuation."
  (let ((result (cj/markov-tokenize "one, two; three.")))
    (should (equal result '("one" "," "two" ";" "three" ".")))))

(ert-deftest test-tokenize-empty ()
  "Should handle empty string."
  (let ((result (cj/markov-tokenize "")))
    (should (null result))))

(ert-deftest test-tokenize-whitespace-only ()
  "Should return nil for whitespace only."
  (let ((result (cj/markov-tokenize "   \n\t  ")))
    (should (null result))))

;;; Markov Learn Tests

(ert-deftest test-learn-basic ()
  "Should learn simple text."
  (let ((chain (test-learn "one two three four")))
    (should (cj/markov-chain-p chain))
    (should (> (hash-table-count (cj/markov-chain-map chain)) 0))))

(ert-deftest test-learn-creates-bigrams ()
  "Should create bigram mappings."
  (let ((chain (test-learn "one two three")))
    (should (gethash '("one" "two") (cj/markov-chain-map chain)))))

(ert-deftest test-learn-stores-following-word ()
  "Should store following word for bigram."
  (let ((chain (test-learn "one two three")))
    (should (member "three" (gethash '("one" "two") (cj/markov-chain-map chain))))))

(ert-deftest test-learn-builds-keys-list ()
  "Should build keys list lazily when accessed."
  (let ((chain (test-learn "one two three four")))
    ;; Keys are built lazily, so initially nil
    (should (null (cj/markov-chain-keys chain)))
    ;; After calling random-key, keys should be built
    (cj/markov-random-key chain)
    (should (> (length (cj/markov-chain-keys chain)) 0))))

(ert-deftest test-learn-repeated-patterns ()
  "Should accumulate repeated patterns."
  (let ((chain (test-learn "one two three one two four")))
    (let ((nexts (gethash '("one" "two") (cj/markov-chain-map chain))))
      (should (= (length nexts) 2))
      (should (member "three" nexts))
      (should (member "four" nexts)))))

(ert-deftest test-learn-incremental ()
  "Should support incremental learning."
  (let ((chain (test-chain)))
    (cj/markov-learn chain "one two three")
    (cj/markov-learn chain "four five six")
    (should (> (hash-table-count (cj/markov-chain-map chain)) 0))))

;;; Token Joining Tests

(ert-deftest test-join-simple-words ()
  "Should join words with spaces."
  (let ((result (cj/markov-join-tokens '("hello" "world"))))
    (should (string-match-p "^Hello world" result))))

(ert-deftest test-join-with-punctuation ()
  "Should attach punctuation without spaces."
  (let ((result (cj/markov-join-tokens '("hello" "," "world"))))
    (should (string-match-p "Hello, world" result))))

(ert-deftest test-join-capitalizes-first ()
  "Should capitalize first word."
  (let ((result (cj/markov-join-tokens '("hello" "world"))))
    (should (string-match-p "^H" result))))

(ert-deftest test-join-adds-period ()
  "Should add period if missing."
  (let ((result (cj/markov-join-tokens '("hello" "world"))))
    (should (string-match-p "\\.$" result))))

(ert-deftest test-join-preserves-existing-period ()
  "Should not double-add period."
  (let ((result (cj/markov-join-tokens '("hello" "world" "."))))
    (should (string-match-p "\\.$" result))
    (should-not (string-match-p "\\.\\.$" result))))

(ert-deftest test-join-empty-tokens ()
  "Should handle empty token list."
  (let ((result (cj/markov-join-tokens '())))
    (should (equal result "."))))

;;; Capitalization Tests

(ert-deftest test-capitalize-first-word ()
  "Should capitalize first word."
  (let ((result (cj/markov-fix-capitalization "hello world")))
    (should (string-match-p "^Hello" result))))

(ert-deftest test-capitalize-after-period ()
  "Should capitalize after period."
  (let ((result (cj/markov-fix-capitalization "hello. world")))
    (should (string-match-p "Hello\\. World" result))))

(ert-deftest test-capitalize-after-exclamation ()
  "Should capitalize after exclamation."
  (let ((result (cj/markov-fix-capitalization "hello! world")))
    (should (string-match-p "Hello! World" result))))

(ert-deftest test-capitalize-after-question ()
  "Should capitalize after question mark."
  (let ((result (cj/markov-fix-capitalization "hello? world")))
    (should (string-match-p "Hello\\? World" result))))

(ert-deftest test-capitalize-skip-non-alpha ()
  "Should skip non-alphabetic tokens."
  (let ((result (cj/markov-fix-capitalization "hello. 123 world")))
    (should (string-match-p "123" result))))

(ert-deftest test-capitalize-multiple-sentences ()
  "Should capitalize all sentences."
  (let ((result (cj/markov-fix-capitalization "first. second. third")))
    (should (string-match-p "First\\. Second\\. Third" result))))

;;; Generation Tests (deterministic with fixed chain)

(ert-deftest test-generate-produces-output ()
  "Should generate non-empty output."
  (let ((chain (test-learn "Lorem ipsum dolor sit amet consectetur adipiscing elit")))
    (let ((result (cj/markov-generate chain 5)))
      (should (stringp result))
      (should (> (length result) 0)))))

(ert-deftest test-generate-empty-chain ()
  "Should handle empty chain gracefully."
  (let ((chain (test-chain)))
    (let ((result (cj/markov-generate chain 5)))
      (should (or (null result) (string-empty-p result))))))

(ert-deftest test-generate-respects-start ()
  "Should use provided start state if available."
  (let ((chain (test-learn "Lorem ipsum dolor sit amet")))
    (let ((result (cj/markov-generate chain 3 '("Lorem" "ipsum"))))
      (should (stringp result))
      ;; Should start with Lorem or similar
      (should (> (length result) 0)))))

;;; Integration Tests

(ert-deftest test-full-workflow ()
  "Should complete full learn-generate workflow."
  (let ((chain (test-chain)))
    (cj/markov-learn chain "The quick brown fox jumps over the lazy dog")
    (let ((result (cj/markov-generate chain 8)))
      (should (stringp result))
      (should (> (length result) 0))
      (should (string-match-p "^[A-Z]" result))
      (should (string-match-p "[.!?]$" result)))))

(ert-deftest test-latin-like-output ()
  "Should generate Latin-like text from Latin input."
  (let ((chain (test-chain)))
    (cj/markov-learn chain "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
    (let ((result (cj/markov-generate chain 10)))
      (should (stringp result))
      (should (> (length result) 10)))))

;;; Edge Cases

(ert-deftest test-learn-short-text ()
  "Should handle text shorter than trigram."
  (let ((chain (test-learn "one two")))
    (should (cj/markov-chain-p chain))))

(ert-deftest test-learn-single-word ()
  "Should handle single word."
  (let ((chain (test-learn "word")))
    (should (cj/markov-chain-p chain))))

(ert-deftest test-generate-requested-count-small ()
  "Should handle small generation count."
  (let ((chain (test-learn "one two three four five")))
    (let ((result (cj/markov-generate chain 2)))
      (should (stringp result)))))

(provide 'test-lorem-optimum)
;;; test-lorem-optimum.el ends here
