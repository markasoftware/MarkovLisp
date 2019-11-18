(ql:quickload :cl-ppcre)

(defpackage markov
  (:use :cl)
  (:export :make-model
           :gen-next-word
           :gen-sentence))
(in-package :markov)

;; TODO/PROBLEMS:
;; * non-terminal punctuation (commas, parenthesis)

;; Markov Chain model format:
;; { "hello" => (("world" 4 20) ("vorld" 8 16) ("end" 8 8)) }
;;
;; each top-level key is the first word. Each element contains the words that
;; can follow it. The two numbers represent the probability that this word
;; should be picked. It must be traversed in order: So, with a 4/20 chance
;; (equivalent to 1/5), "world" is selected. If it is selected, iteration
;; stops. If it's not selected, iteration continues to "vorld", with a 1/2
;; chance. If that's still not selected, "end" is selected unconditionally.
;;
;; absolute probabilities for each word in the above example:
;;
;; world: 0.2
;; vorld: 0.4
;; end:   0.4
;;
;; Intermediate model fomat:
;; { hello => {world => 4, vorld => 8, end => 8} }
;; total numbers of occurrences

(defun make-model (input)
  "Generate a Markov chain model from a piece of text."
  (let ((model (make-hash-table :test #'equal)))
    (loop for (word next-word) on (ppcre:split "\\s+" input)
         until (null next-word)
         ;; TODO: make list of punctuation more comprehensive
       do (unless (member (aref word (1- (length word))) '(#\. #\? #\!) :test #'eql)
            (unless (gethash word model)
              (setf (gethash word model) (make-hash-table :test #'equal)))
            (incf (gethash next-word (gethash word model) 0))))
    (loop for first-word being each hash-key of model using (hash-value occurrences)
       do (setf (gethash first-word model)
                (loop
                   for total-occurrences = 0 then (+ total-occurrences times)
                   for result = (cons (list next-word times total-occurrences) result)
                   for next-word being the hash-keys of occurrences
                   using (hash-value times)
                   finally (return result))))
    model))

(defun gen-next-word (model last-word)
  "Generate a word to come after last-word using model. Nil for a terminal"
  (loop
     for candidate in (gethash last-word model)
     until (<= (1+ (random (third candidate))) (second candidate))
     finally (return (first candidate))))

(defun gen-sentence (model first-word)
  "Generates a sentence, starting with the given word."
  (loop
     for result = first-word then (concatenate 'string result " " word)
     for word = (gen-next-word model (or word first-word))
     while word
     finally (return result)))
