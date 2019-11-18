(ql:quickload :cl-ppcre)

(defpackage markov
  (:use :cl)
  (:export :make-model
           :gen-words
           :gen-text))
(in-package :markov)

;; TODO/PROBLEMS:
;; * non-terminal punctuation (commas, parenthesis)

;; Markov Chain model format:
;; { hello => (world 0.2 vorld 0.5 end 1) }
;; each element's probability is in respect to the remaining words.
;; so, in the above example, the absolute probability for each is:
;; world: 0.2
;; vorld: 0.4
;; end:   0.4
;;
;; Refined Markov Chain model format:
;; { hello => (world 4 20) (vorld )}
;;
;; to calculate the "cumulative" probability that's stored in the hashmap, you
;; take its global probability divided by 1-(sum of former probabilities). Why?
;; the first element's cumulative obvious = global prob. The second one should
;; have global_prob=local_prob*change_of_getting_to_local_prob. That last long
;; thing is obviously (1-chance_of_selecting_a_former_thing). Solve for local.
;;
;; If we're building the final model from the back to the front, as we do, this
;; actually becomes more elegant. We don't even need to store the total number
;; of occurrences. Instead, we find the number of occurrences of current word /
;; number of occurrences of all words that we've added to the end so far. The
;; last word's number of occurrences is equal to the total occurrences, so
;; 1.0. As you can see, if we went foward through the list as we will during
;; generation, the list of the last n elements can be thought of as a
;; subproblem: What percentage of the occurrences in this sublist does this
;; element have? And we know exactly that as we build up from the end.
;;
;; Intermediate model fomat:
;; { hello => (20 . {world => 4, vorld => 8, end => 8}) }

(defun make-model (input)
  "Generate a Markov chain model from a piece of text."
  (let ((model (make-hash-table :test #'equal)))
    (loop for (word next-word) on (ppcre:split "\\s+" input)
         ;; when destructuring in a for-on loop, it tries to match aganist every
         ;; cdr of the list. So, on the last loop, word will be bound and
         ;; next-word will not.
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

(defun gen-words (model last-word)
  "Generate a word to come after last-word using model. Nil for a terminal"
  (loop
     for candidate in (gethash last-word model)
     ;; ("land" 1 2) --> random(1,2) <= 2
     ;; ("land" 1 5) --> random(1,5) <= 1
     ;; a small (second) means a small chance, so <=
     until (<= (1+ (random (third candidate))) (second candidate))
     finally (return (first candidate))))

(defun gen-text (model first-word)
  "Generates a sentence."
  (loop
     for word = first-word then (gen-words model word)
     while word
     collecting word))
