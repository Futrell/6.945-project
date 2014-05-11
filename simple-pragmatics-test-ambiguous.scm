;;; A more realistic world where messages are ambiguous.

(define universe '(man
                   man-with-only-glasses
                   man-with-hat-and-glasses))

(define lexicon
  '((man man)
    (man man-with-nothing)
    
    (man-with-only-glasses man)
    (man-with-only-glasses man-with-glasses)
    (man-with-only-glasses man-with-only-glasses)

    (man-with-hat-and-glasses man)
    (man-with-hat-and-glasses man-with-glasses)
    (man-with-hat-and-glasses man-with-hat)
    (man-with-hat-and-glasses man-with-hat-and-glasses)))

