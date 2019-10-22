#lang racket

;;Libreria de Cartas para graficar
(require games/cards)


;;Deck definido para barajar las cartas
(define Deck  '(Ks Qs Js 10s 09s 08s 07s 06s 05s 04s 03s 02s As
                Kh Qh Jh 10h 09h 08h 07h 06h 05h 04h 03h 02h Ah
                Kd Qd Jd 10d 09d 08d 07d 06d 05d 04d 03d 02d Ad
                Kc Qc Jc 10c 09c 08c 07c 06c 05c 04c 03c 02c Ac))


;;Función Principal
(define (bCEj X)
  (aux1bCEj X Deck))


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


;;Funcion auxiliar para solicitar una carta para un jugador X
(define (auxPedirCarta Jugador numRandom deck)
  (cond ((null? deck) '())
        ((>= numRandom (lenList (car (cddddr deck)))) "Error el random sobrepasa el limite")
        ((> Jugador 5) "Error J5 No Existe")
        ((= Jugador 1) (append (list (append (car deck) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                               (list (cadr deck) (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck))))))
        ((= Jugador 2) (append (list (car deck))
                               (list (append (car (cdr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                               (list (caddr deck) (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck))))))
        ((= Jugador 3) (append (list (car deck) (cadr deck))
                               (list (append (car (cddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                               (list (cadddr deck) (modificarDeck2 numRandom 0 (car (cddddr deck))))))
        ((= Jugador 4) (append (list (car deck) (cadr deck) (caddr deck))
                               (list (append (car (cdddr deck)) (list (cartasAleatorias (car (cddddr deck)) numRandom 0))))
                               (list (modificarDeck2 numRandom 0 (car (cddddr deck))))))
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
(define (verificar21 listaJugador)
  (auxVerificar21 listaJugador 0))


;;Funcion Auxiliar de verificacion de 21
(define (auxVerificar21 listaJugador suma)
  (cond ((null? listaJugador) #t)
        ((= (+ suma (string->number (substring (symbol->string  (car listaJugador)) 0 2))) 21) "BlackJack")
        ((> (+ suma (string->number (substring (symbol->string  (car listaJugador)) 0 2))) 21) "Fail")
        (else (auxVerificar21 (cdr listaJugador) (+ suma (string->number (substring (symbol->string  (car listaJugador)) 0 2)))))))


;(verificar21 '(10h 02s))
;(string->number (substring (symbol->string  '9h) 0 1))
;(comparacion 'Ac)
;(auxPedirCarta 3 43 MatrixJuego)
;(bCEj 3)


         