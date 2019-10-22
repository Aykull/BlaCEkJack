#lang racket

;;Deck Principal
#|'(Ks Qs Js 10s 09s 08s 07s 06s 05s 04s 03s 02s As
    Kh Qh Jh 10h 09h 08h 07h 06h 05h 04h 03h 02h Ah
    Kd Qd Jd 10d 09d 08d 07d 06d 05d 04d 03d 02d Ad
    Kc Qc Jc 10c 09c 08c 07c 06c 05c 04c 03c 02c Ac)|#

;;Deck de Pruebas para conseguir el BlackJack
#| '(Ks Qs Js 10s As As Ks Qs Js 10s As As
     As As As As As As 08j 09u 06f  As As 
     Ks Qs Js 10s As As As As As As As As
     08j 09u 06f As As Ks Qs Js 10s As As
     As As As As As As 08j 09u 06f)|#


;;Libreria de Cartas para graficar
(require games/cards)


;;Deck definido para barajar las cartas
(define Deck  '(Ks Qs Js 10s 09s 08s 07s 06s 05s 04s 03s 02s As
                Kh Qh Jh 10h 09h 08h 07h 06h 05h 04h 03h 02h Ah
                Kd Qd Jd 10d 09d 08d 07d 06d 05d 04d 03d 02d Ad
                Kc Qc Jc 10c 09c 08c 07c 06c 05c 04c 03c 02c Ac))


;;Función Principal
(define (bCEj X)
  (cond ((= X 3) (aux1bCEj X Deck))
        (else "Cantidad de jugadores no permitida")))


;;Funcion Auxiliar 1 Principal, la cual construye la lista de jugadores con sus respectivas cartas
;;R1 (Numero Random 1), R2 (Numero Random 2)
(define (aux1bCEj X deck)
  (aux2bCEj X (random 52) (random 52) 52 deck))


;;Funcion Auxiliar 2 Principal, la cual construye la lista de jugadores con sus respectivas cartas
(define (aux2bCEj X R1 R2 lenDeck deck)
  (cond ((= X -1) (list deck))
        ;;Compara si las dos cartas son iguales
        ((= R1 R2) (aux2bCEj X (random lenDeck) (random lenDeck) lenDeck deck))
    ;;retorna una lista con los jugadores y el diler
    (else (cons (list (cartasAleatorias deck R1 0) (cartasAleatorias deck R2 0))
                (aux2bCEj (- X 1) (random (- lenDeck 2)) (random (- lenDeck 2)) (- lenDeck 2) (modificarDeck R1 R2 0 0 deck))))))


;;Funcion para modificar el deck eliminando 2 cartas y que no se repitan las cartas
(define (modificarDeck pos pos2 cont cont2 deck)
  (cond ((null? deck) '())
        ((and (= pos cont) (= pos2 cont2)) (cdr deck))
        ((= pos cont) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck)))
        ((= pos2 cont) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck)))
        (else (cons (car deck) (modificarDeck pos pos2 (+ cont 1) (+ cont2 1) (cdr deck))))))


;;Funcion para modificar el deck eliminando 1 carta
(define (modificarDeck2 pos cont deck)
  (cond ((null? deck) '())
        ((= pos cont) (cdr deck))
        (else (cons (car deck) (modificarDeck2 pos (+ cont 1) (cdr deck))))))


;;Función Carta Aleatoria
(define (cartasAleatorias deck numRandom contRandom)
  (cond
    ((null? deck) '())
    ((= numRandom contRandom)(car deck)) 
    (else (cartasAleatorias (cdr deck) numRandom (+ 1 contRandom)))))


;;Funcion len de una Lista
(define (lenList lista)
  (cond ((null? lista) 0)
  (else (+ 1 (lenList (cdr lista))))))


;;Funcion para solicitar una carta
(define (PedirCarta Jugador deck)
  (aux1PedirCarta Jugador (random (- (lenList (car (cddddr deck))) 1))  deck))


;;Funcion auxiliar 1 para solicitar una carta verifica la lista del jugador antes de agregar carta
(define (aux1PedirCarta Jugador numRandom deck)
  (cond ((null? deck) '())
        ((>= numRandom (lenList (car (cddddr deck)))) "Error el random sobrepasa el limite")
        ((> Jugador 5) "Error El Jugador No Existe")
        ((= Jugador 1) (if (equal? (verificar21 (car deck) 1) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (cons (verificar21 (car deck) 1) (car deck))))
        
        ((= Jugador 2) (if (equal? (verificar21 (car (cdr deck)) 2) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (cons (verificar21 (car (cdr deck)) 2) (car (cdr deck)))))
        
        ((= Jugador 3) (if (equal? (verificar21 (car (cddr deck)) 3) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (cons (verificar21 (car (cddr deck)) 3) (car (cddr deck)))))
    
        ((= Jugador 4) (if (equal? (verificar21 (car (cdddr deck)) 4) #t) 
                           (aux2PedirCarta Jugador numRandom deck)
                           (cons (verificar21 (car (cdddr deck)) 4) (car (cdddr deck)))))
        (else "Error")))


;;Funcion auxiliar 2 para solicitar una carta para un jugador con carta agregada X
(define (aux2PedirCarta Jugador numRandom deck)
  (cond ((null? deck) '())
        ((>= numRandom (lenList (car (cddddr deck)))) "Error el random sobrepasa el limite")
        ((> Jugador 5) "Error El Jugador No Existe")
        ((= Jugador 1) (if (equal? (verificar21 (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 1) #t) 
                           (append (list (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (cadr deck) (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (cons (verificar21 (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 1)
                                 (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))))
                       
        ((= Jugador 2) (if (equal? (verificar21 (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 2) #t) 
                           (append (list (car deck))
                                   (list (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (cons (verificar21 (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 2)
                                 (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))))        
        
        ((= Jugador 3) (if (equal? (verificar21 (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 3) #t) 
                           (append (list (car deck) (cadr deck))
                                   (list (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (cons (verificar21 (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 3)
                                 (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))))
    
        ((= Jugador 4) (if (equal? (verificar21 (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 4) #t) 
                           (append (list (car deck) (cadr deck) (caddr deck))
                                   (list (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                                   (list (modificarDeck2 numRandom 0 (car (cddddr deck)))))
                           (cons (verificar21 (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))) 4)
                                 (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))))
        (else "Error")))


;;Funcion Para realizar una comparacion entre el Deck del GUI con Deck de la logica
(define (comparacion carta)
  (auxComparacion carta 0 Deck))


;;Funcion auxiliar para realizar una comparacion
(define (auxComparacion carta cont deck)
  (cond ((null? deck) "Carta no encontrada")
        ((equal? carta (car deck)) (deckGUI cont (make-deck)))
        (else (auxComparacion carta (+ 1 cont) (cdr deck)))))


;;Funcion que grafica la carta del deck
(define (deckGUI carta deck)
  (cond ((= carta 0) 
         (write (send (car deck) get-value))
         (write (send (car deck) get-suit)))
        (else (deckGUI (- carta 1) (cdr deck)))))


;;Funcion que verifica que la suma sea 21 o menor
(define (verificar21 listaJugador J)
  (auxVerificar21 listaJugador J 0))


(define (auxVerificar21 listaJugador J suma)
  (cond ((null? listaJugador) (verificarSuma J suma))
        ;;Compara si la carta es un A para sumar 1 o 11
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "A") (auxVerificar21 (cdr listaJugador) J (+ suma 11)))
        ;;Compara si la carta es un K para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "K") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        ;;Compara si la carta es un Q para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "Q") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        ;;Compara si la carta es un J para sumar 10
        ((equal? (substring (symbol->string  (car listaJugador)) 0 1) "J") (auxVerificar21 (cdr listaJugador) J (+ suma 10)))
        (else (auxVerificar21 (cdr listaJugador) J (+ suma (string->number (substring (symbol->string  (car listaJugador)) 0 2)))))))


;;Verifica si la suma es 21 para declarar BlackJack o Fail o #t
(define (verificarSuma J suma)
  (cond ((= J 4) (if (>= suma 17) "Crupier" #t))
        ((= suma 21) "BlackJack")
        ((> suma 21) "Fail")
        (else #t)))


;;Funcion plantarse
(define (plantarse J deck)
  (cond ((> J 5) (mostrarResultados deck))
        ((= J 1)("Siga con el J2"))))


;;Funcion que muestra los resultados
(define (mostrarResultados deck)
  (car deck))
  

;(verificar21 '(10h 02s))
;(string->number (substring (symbol->string  '9h) 0 1))
;(comparacion 'Ac)
(PedirCarta 4 (bCEj 3))
;(bCEj 3)

