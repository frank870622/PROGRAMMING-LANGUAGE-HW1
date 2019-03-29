(defun merge_mix (num_a num_b)
    (cond   (
                (eq (length num_a) 0)
                num_b
            )
            (
                (eq (length num_b) 0)
                num_a
            )
            (
                (< (car num_a) (car num_b))
                (cons   (car num_a) 
                        (merge_mix (cdr num_a) num_b)
                )
            )
            (
                (eq 0 0)
                (cons   (car num_b) 
                        (merge_mix num_a (cdr num_b))
                )
            )
    )
)

(defun mergesort (numbers)
    (if (< (length numbers) 2)
        numbers
        (
            merge_mix   (mergesort (subseq numbers 0 (truncate (length numbers) 2)))
                        (mergesort (subseq numbers (truncate (length numbers) 2)  (length numbers)))
        )
    )
)

; main function
(let    ((n (read)) (numbers))
        (setf numbers
            (do ((i 0 (+ i 1)) (tmp nil)) 
                ((>= i n) (reverse tmp))
                (setf tmp (cons (read) tmp))
        )
)
    (format t "~{~A ~}~%" (mergesort numbers))
)