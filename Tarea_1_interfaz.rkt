#lang racket/base

(require racket/gui)
(require racket/include)

(require (prefix-in htdp: 2htdp/image))

(require "Logica.rkt")
;(include "Logica.rkt")
;(include "foo.rkt")



; ||============= Variables necesarias para el juego y sus respectivos set! ==========||


;Condición de que el juego comenzó
(define gameStart #f)

; Función que dice si el juego comenzó o no

(define (asignar_gameStart bool)
    (set! gameStart (cons bool gameStart))
)

; Matriz del juego actual
(define matrix null)

; Función que asigna la matriz del juego en el momento
(define (asignar_matriz matriz)
    (set! matrix (cons matriz matrix))
)

; ||============= Creación de widwets interfaz  ==========||


; Widget frame que represente la ventana
(define frame (new frame%
                   [label "Tarea 1"]
                   [width 300]
                   [height 300]))

; String del nombre del juego
(define msg (new message% [parent frame]
                          [label "4Line"]))

; Mensaje para indicar al jugador que se le pide en los text fields
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

                  
                  ((and (string->number (send filas_TF get-value)) (string->number (send columnas_TF get-value)) (not (false? (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))) (not gameStart) )
                   (crear_botones_columnas (string->number (send columnas_TF get-value)) 0)
                   (write (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))
                   (asignar_matriz (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))
                   (crear_matriz_interfaz (car matrix))
                   (asignar_gameStart #t)
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

;Dialog box cuando el jugador gana
(define wonWindow (new dialog% [label "El juego ha terminado: GANADOR"]
                       [parent frame]
                       [width 300]	 

       )
)

; Función para mostrar la ventana que ganó
(define (showWonWindow)
  
   (send wonWindow show #t)
  
 )

; ||============= Funciones que afectan la interfaz  ==========||

; Hace un botón para tirar una ficha en el juego
(define (crear_boton col)

  (new button% [parent panel_botones]
             [label (number->string col)]
             [horiz-margin 1]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (writeln (play 1 col (car matrix) ) )
                         (cond
                           ;Si la función jugar devuelve un string es que el jugador ya ganó
                           ((not (string? (play 1 col (car matrix) )) )
                            (asignar_matriz (play 1 col (car matrix)))
                            (refrescar_tablero)
                            (crear_matriz_interfaz (car matrix))
                            
                           )
                           (else (showWonWindow))
                         )
                         )]
                        
  )
  
)


; Hace el espacio que representa fichas dependiendo del numero que toque (0 es vacío, 1 es ficha jugador, 2 ficha máquina)

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
  


;Crea un widget que representa una fila de la matriz en la interfaz
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

; Esta función se encarga de crear toda la matriz en la interfaz
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

;Función que crea n botones para las columnas del juego en la interfaz

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


 

;Función que reinicia el tablero de juego en la interfaz, cada vez que se hace un movimiento

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