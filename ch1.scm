(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)

(atom? 'turkey)

(atom? 1942)

(atom? 'u)

(atom? '*abc$)

(list? '(atom))

(list? '(atom turkey or))

;;(list? '(atom turkey) or)

(list? '((atom turkey) or))