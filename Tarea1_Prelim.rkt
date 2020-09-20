#lang racket
#|==================================================================|#

(define (getPos_aux elem lst index)
  (cond
    ((null? lst) #f)
    ((equal? elem (car lst)) index)
    (else (getPos_aux elem (cdr lst) (+ index 1)))
  )
)

;;Obtener el índice de ese elemeto
(define (getPos elem lst)
  (getPos_aux elem lst 0)
)


#|==================================================================|#

(define (at_aux index lst cont)
  (cond
    ((null? lst) #f)
    ((equal? index cont) (car lst))
    (else (at_aux index (cdr lst) (+ cont 1)))
  )
)

;;Obtener el elemento en ese índice
(define (at index lst)
  (at_aux index lst 0)
)


#|==================================================================|#


;;Verifica si hay ceros en una fila
(define (hayCeros? lst)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) 0) #t)
    (else (hayCeros? (cdr lst)))
  )
)

(define (full_aux matrx)
  (cond
    ((null? matrx) #t)
    ((hayCeros? (car matrx)) #f)
    (else (full_aux (cdr matrx)))
  )
)

;;Verifica si la matrix está llena => empate
(define (full? matrx)
  (cond
    ((and (list? matrx) (list? (car matrx))) (full_aux matrx))
  )
)

;;(full? '((1 2 1 2 0) (2 2 2 2 1) (1 2 2 1 2)))
#|==================================================================|#

;;Verifica si las dimensiones están en el rango
(define (validDim rows col)
  (cond
    ((and (>= rows 8) (<= rows 16) (>= col 8) (<= col 16)) #t)
    (else #f)
  )
)

;;Hace una lista de tantos ceros como número de columnas se den
(define (makeRow col_number)
  (cond
    ((zero? col_number) null)
    (else (cons 0 (makeRow (- col_number 1))))
  )
)

(define (generateMatrx_aux m n)
  (cond
    ((zero? m) null)
    (else (cons (makeRow n) (generateMatrx_aux (- m 1) n)))
  )
)

;;Generar una matriz de dimensiones m x n vacía (solo de ceros) 
(define (generateMatrx m n)
  (cond
    ((validDim m n) (generateMatrx_aux m n))
    (else #f)
  )
)

#|==================================================================|#

;;Cuenta los elementos de una lista
(define (len lst)
  (cond
    ((null? lst) 0)
    (else (+ 1 (len (cdr lst))))
  )
)

;;doFunc: Realiza una función en cada elemento de una lista
;;E: función por realizar, lista donde se va a aplicar la función
;;S: lista con la función aplicada
;;R: null si la lista es nula o recibe un #f
(define (doFunc func lista)
  (cond
    ((or (null? lista) (boolean? lista)) null)
    (else (cons (func (car lista)) (doFunc func (cdr lista)))) 
  )
)

;;Devuelve la traspuesta de una matriz
(define (traspuesta matrx)
  (cond
    ((null? (car matrx)) '()) 
    (else (cons (doFunc car matrx) (traspuesta (doFunc cdr matrx))))
  )
)

(define (fullCol_aux colNum matrx cont)
  (cond
    ((null? matrx) #f)
    ((equal? cont colNum) (not (hayCeros? (car matrx))))
    (else (fullCol_aux colNum (cdr matrx) (+ cont 1)))
  )
)

;;Verifica si una columna está llena
(define (fullColumn? colNum matrx)
  (fullCol_aux colNum (traspuesta matrx) 0)
)


#|==================================================================|#










