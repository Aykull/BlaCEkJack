;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Logica) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;Función Principal
(define (bCEj X)
  (auxbCEj X '(Ah 1h 2h 3h 4h 5h 6h 7h 8h 9h 10h Jh Qh Kh
            Ad 1d 2d 3d 4d 5d 6d 7d 8d 9d 10d Jd Qd Kd
            Ac 1c 2c 3c 4c 5c 6c 7c 8c 9c 10c Jc Qc Kc
            As 1s 2s 3s 4s 5s 6s 7s 8s 9s 10s Js Qs Ks)))

;;Funcion Auxiliar Principal, la cual construye la lista
;;de jugadores con sus respectivas cartas
(define (auxbCEj X deck)
  (cond ((= X -1) '())
    ;;retorna una lista con los jugadores y el diler
    (else (cons (list (cartasAleatorias deck (random (lenList deck)) 0) (cartasAleatorias deck (random (lenList deck)) 0))  (auxbCEj (- X 1) deck)))))

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

             

(bCEj 3)