#lang racket
(provide (all-defined-out))

;;Función Principal
(define (bCEj X)
  (aux1bCEj X '(Ah 2h 3h 4h 5h 6h 7h 8h 9h 10h Jh Qh Kh
               Ad 2d 3d 4d 5d 6d 7d 8d 9d 10d Jd Qd Kd
               Ac 2c 3c 4c 5c 6c 7c 8c 9c 10c Jc Qc Kc
               As 2s 3s 4s 5s 6s 7s 8s 9s 10s Js Qs Ks)))

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
    (else (cons (list (cartasAleatorias deck R1 0) (cartasAleatorias deck R2 0))  (aux2bCEj (- X 1) (random (- lenDeck 2)) (random (- lenDeck 2)) (- lenDeck 2) (modificarDeck R1 R2 0 0 deck))))))

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

;;Se define una variable de la matriz del juego
(define MatrixJuego (bCEj 3))
;;MatrixJuego

;;Funcion para solicitar una carta y "sumar 21"
(define (PedirCarta Jugador numRandom deck)
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


;(PedirCarta 1 43 MatrixJuego)
