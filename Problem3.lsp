(defun longest (a b)
    (if (> (length a) (length b)) a b)
)

(defun lcs (a b)
    (cond
        (   (or (null a) (null b)) 
            nil
        )
        (   (equal (car a) (car b))
            (cons (car a) (lcs (cdr a) (cdr b)))
        )
        (t (longest (lcs a (rest b)) (lcs (rest a) b)))
    )
)

(defun printOut (a b common)
    (cond
        (
            (and (null common) (null a) (null b))
            nil
        )
        (
            (null a)
            (progn
                (format t "~A~%" (concatenate 'string "+" (car b)))
                (printOut a (cdr b) common)
            )
        )
        (
            (null b)
            (progn
                (format t "~A~%" (concatenate 'string "-" (car a)))
                (printOut (cdr a) b common)
            )
        )
        (
            (and (equal (car a) (car common)) (equal (car b) (car common)))
            (progn
                (format t "~A~%" (concatenate 'string " " (car a)))
                (printOut (cdr a) (cdr b) (cdr common))
            )
        )
        (
            (not (equal (car a) (car common)))
            (progn
                (format t "~A~%" (concatenate 'string "-" (car a)))
                (printOut (cdr a) b common)
            )
        )
        (
            (not (equal (car b) (car common)))
            (progn
                (format t "~A~%" (concatenate 'string "+" (car b)))
                (printOut a (cdr b) common)
            )
        )
    )
)

(defun openfile (filename1 filename2)
    (let
        (
            (file1 ())
            (file2 ())
            (result ())
        )
        (with-open-file (stream filename1)
            (do (
                    (line (read-line stream nil) (read-line stream nil))
                )
                ((null line))
                (push line file1)
            )
        )
        (with-open-file (stream filename2)
            (do (
                    (line (read-line stream nil) (read-line stream nil))
                )
                ((null line))
                (push line file2)
            )
        )
        (printOut (reverse file1) (reverse file2) (lcs (reverse file1) (reverse file2)))
    )
)

(openfile "file1.txt" "file2.txt")