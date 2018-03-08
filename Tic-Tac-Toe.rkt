#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/list)

(define-values
  (rows cols scn_w scn_h grids grid_images grid_posns current game? win win_text)
  (values 3 3 200 200 empty empty empty "x" #t " " (text "" 10 "black")))


(define-values
  (g_w g_h )
  (values (/ scn_w cols) (/ scn_h rows)))


(define scn (empty-scene scn_w scn_h))
(define x_image
  (add-line (add-line (rectangle g_w g_h "outline" "black") (/ g_w 6) (/ g_w 6) (- g_w (/ g_w 6)) (- g_h (/ g_w 6)) "black") (- g_w (/ g_w 6)) (/ g_w 6) (/ g_w 6) (- g_h (/ g_w 6)) "black"))

(define o_image
  (overlay
   (circle (- (/ g_w 2) (/ g_w 6)) "outline" "black")
   (rectangle g_w g_h "outline" "black"))
  )

(define grid_values
  (let loop ((i 0))
    (if (< i (* cols rows)) (append (list " ") (loop (add1 i))) empty)
    ))

(define (find-index x y)
  (let ((i (floor (/ x g_w)))
        (j (floor (/ y g_h))))
    (+ i (* j cols))))

(define (change-value l i v)
  (append (take l i) (list v) (rest (drop l i)))
  )

(define (render ws)
  (place-images
   (append
    (list
     win_text
     )
    grid_images)
   (append
    (list
     (make-posn (/ scn_w 2) (/ scn_h 2))
     )
    grid_posns)
   scn)
  )

(define (check-win g1 g2 g3 what)
  (if (and (string=? (list-ref grid_values g1) what) (string=? (list-ref grid_values g2) what) (string=? (list-ref grid_values g3) what)) #t #f)
  )

(define (key-handler ws ke)
  (if (not game?)
      (if (key=? ke "r") (set!-values (current game? win grid_values)
                               (values "x" #t "" (let loop ((i 0))
                                 (if (< i (* cols rows)) (append (list " ") (loop (add1 i))) empty)
                                 ))) void)
      void)
  )

(define (mouse-handler ws x y me)
  (if (and game? (string=? me "button-down") (string=? " " (list-ref grid_values (find-index x y)))  (< 0 x scn_w) (< 0 y scn_h))
      (begin
        (set! grid_values (change-value grid_values (find-index x y) current))
        (if (string=? current "x") (set! current "o") (set! current "x"))) void ))

(define (step ws)
  (set! grids (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                 (if (< i cols) (let ((index (+ i (* j cols))))
                                                  (append (list (list (cond
                                                                        ((string=? (list-ref grid_values index) " ") (rectangle g_w g_h "outline" "black"))
                                                                        ((string=? (list-ref grid_values index) "x") x_image)
                                                                        ((string=? (list-ref grid_values index) "o") o_image)
                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
                                                  ) (loop_j (add1 j)))
                                 ) empty)
                ))

  (set! grid_posns (let loop ((i 0))
                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
                     ))

  (set! grid_images (let loop ((i 0))
                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
                      ))

  (if (or (check-win 0 1 2 "x") (check-win 3 4 5 "x") (check-win 6 7 8 "x")
          (check-win 0 3 6 "x") (check-win 1 4 7 "x") (check-win 2 5 8 "x")
          (check-win 2 4 6 "x") (check-win 0 4 8 "x")) (set!-values (game? win) (values #f "x")) void)

  (if (or (check-win 0 1 2 "o") (check-win 3 4 5 "o") (check-win 6 7 8 "o")
          (check-win 0 3 6 "o") (check-win 1 4 7 "o") (check-win 2 5 8 "o")
          (check-win 2 4 6 "o") (check-win 0 4 8 "o")) (set!-values (game? win) (values #f "o")) void)
  (set! win_text (text (if (string=? win "x") "'X' WINS!!" "'O' WINS!!") (floor (/ scn_w 7.0 )) (color 0 0 0 (if game? 0 255)))))

(big-bang 0
          (on-tick step)
          (on-mouse mouse-handler)
          (on-draw render)
          (on-key key-handler)
          )