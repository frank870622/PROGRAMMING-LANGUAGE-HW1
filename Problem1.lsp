(defun prime (input)
    (if (< input 2)
        (return (format t "~A is not a prime number ~%" input))
        (if (eq input 2)
            (return (format t "~A is a prime number ~%" input))
        )
    )
    
    (if (> input 2)
            (progn
                (loop   for x from 2 to input
                        do
                        (if (eq (mod input x) 0)
                            (progn
                                (if (eq x input)
                                    (return (format t "~A is a prime number ~%" input))
                                    (return (format t "~A is not a prime number ~%" input))
                                )
                            )
                        )
                )
            )
    )
)

(defun palindrome (input)
    (let ((re (reverse input)))
        (if (equal input re)
            t
            nil
        )
    )
)

(defun fib1 (n)
    (cond   (
                (eq n 0)
                0
            )
            (
                (eq n 1)
                1
            )
            (
                t
                (+ (fib1 (- n 1)) (fib1 (- n 2)))
            )
    )
)

(defun tailfib (n a b)
    (if (eq n 0)
        b
        (progn
            (setf n (- n 1))
            (setf c b)
            (setf b (+ a b))
            (setf a c)
            (tailfib n a b)
        )
    )
)

(defun fib2 (n)
    (cond
        (
            (eq n 0)
            0
        )
        (
            (eq n 1)
            1
        )
        (
            (eq n 2)
            1
        )
        (
            t
            (tailfib (- n 1) 0 1)
        )
    )
)


