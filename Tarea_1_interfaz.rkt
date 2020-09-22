#lang racket/base

(require racket/gui)
(require racket/include)

(require (prefix-in htdp: 2htdp/image))

(require "Logica.rkt")
;(include "Logica.rkt")
;(include "foo.rkt")

; Matriz del juego actual
(define matrix null)

; Función que asigna la matriz del juego en el momento
(define (asignar_matriz matriz)
    (set! matrix (cons matriz matrix))
)

; Hace un botón para tirar una ficha en el juego
(define (crear_boton col)

  (new button% [parent panel_botones]
             [label (number->string col)]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (writeln (play 1 col (car matrix) ) )
                         (asignar_matriz (play 1 col (car matrix)))
                         (refrescar_tablero)
                         (crear_matriz_interfaz (car matrix))
                         )]
                        
  )
  
)

;

; Hace el espacio dependiendo del numero que toque

(define (crear_espacio num panelx)

  (if(= num 0)
     (new message% [parent panelx]
            [label (read-bitmap "EspacioVacio.png" )])
     
     #f
  )
  (if(= num 1)
     (new message% [parent panelx]
            [label (read-bitmap "EspacioJugador.png" )])
     
     #f
  )
  (if(= num 2)
     (new message% [parent panelx]
            [label (read-bitmap "EspacioMaquina.png" )])
     
     #f
  )
     
)
  


;Crea una fila de la matriz en la interfaz
(define (crear_fila_mat_widget)
   (new horizontal-panel%	 
   	 	[parent panel]	 
                [style '(border)]
                [alignment '(left top)]
    )
)

;Logica crear fila matriz interfaz
(define (crear_fila_matriz_interfaz fila widget)
  (cond
    ( (null? fila ) 1
       
    )(else
          (crear_espacio (car fila) widget)
          (crear_fila_matriz_interfaz (cdr fila) widget)
        )
  )
)

;Logica crear matriz interfaz
(define(crear_matriz_interfaz matriz)
  (cond
    ( (null? matriz ) 1)
      
    ( (not(list? matriz)) #t) 
    
    (else
          (crear_fila_matriz_interfaz (car matriz) (crear_fila_mat_widget))
          (crear_matriz_interfaz (cdr matriz))
        )
  )
)

;Función que crea n botones para las columnas
(define (crear_botones_columnas num cont)

  (cond

    ((number? num)
    
     (cond

       ((= num 0) 1)
       (else

        (crear_boton cont )
        (crear_botones_columnas (- num 1) (+ cont 1))

        )
    
    
       )
     )
    )
  
 )

;Hacer un frame que represente la ventana
(define frame (new frame%
                   [label "Tarea 1"]
                   [width 300]
                   [height 300]))

; Mensaje del nombre del juego
(define msg (new message% [parent frame]
                          [label "4Line"]))

; Para indicar al jugador que se le pide en los text fields
(new message% [parent frame]
                          [label "Coloque el numero de filas y columnas "])

; Textfield para las filas
(define filas_TF(new text-field%
                     [parent frame]
                     [label "Filas: "]
                     [init-value "0"]
                     ))

; Textfield para las columnas
(define columnas_TF(new text-field%
                     [parent frame]
                     [label "Columnas: "]
                     [init-value "0"]
                     ))

;Botón para crear el tablero e iniciar el juego
(new button% [parent frame]
             [label "Iniciar juego"]
             ; Callback procedure for a button click:
             
             [callback (lambda (button event)
                (cond
                  ((and (string->number (send filas_TF get-value)) (string->number (send columnas_TF get-value)) (not (false? (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))) )
                   (crear_botones_columnas (string->number (send columnas_TF get-value)) 0)
                   (write (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))
                   (asignar_matriz (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))
                   (crear_matriz_interfaz (car matrix))
                  )
                  (else (writeln #f))
                )
                         )]
             )

;Definir el área de los botones de interfaz
(define panel_botones(new horizontal-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 ))

;Definir el área de juego de interfaz
(define panel(new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 ))
 

;Reiniciar el tablero d ejuego

(define (refrescar_tablero)
    (send frame delete-child panel)
     (set! panel (new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 )

     )
)

(send frame show #t)