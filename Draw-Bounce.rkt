#lang racket/base

(require 2htdp/image)
(require lang/posn)
(require 2htdp/universe)
(require racket/list)
(require racket/math)



(define-values
  (scn_h scn_w p_curve p_w p_h block_curve block_h block_w b_r pt_list pt_images pt_positions fr_list fr_images fr_positions CD CD2 y)
  (values 300 300 3 70 15 2 25 45 7 empty empty empty empty empty empty 3 14 240))

(define countdown 3)
(define countdown2 14)

(define-struct pt (x y life dir spd))

(define player_image (polygon (list
                               (make-posn p_curve 0)
                               (make-posn (- p_w p_curve) 0)
                               (make-posn p_w p_curve)
                               (make-posn p_w (- p_h p_curve))
                               (make-posn (- p_w p_curve) p_h)
                               (make-posn p_curve p_h)
                               (make-posn 0 (- p_h p_curve))
                               (make-posn 0 p_curve)
                               )
                              255 "white"
                              ))

(define fire_player_image (polygon (list
                               (make-posn p_curve 0)
                               (make-posn (- p_w p_curve) 0)
                               (make-posn p_w p_curve)
                               (make-posn p_w (- p_h p_curve))
                               (make-posn (- p_w p_curve) p_h)
                               (make-posn p_curve p_h)
                               (make-posn 0 (- p_h p_curve))
                               (make-posn 0 p_curve)
                               )
                              255 "white"
                              ))

(define ball_image (circle b_r "solid" "white"))
(define fire_ball_image (overlay (circle (+ b_r 15) 5 "orange") (circle (+ b_r 10) 10 "orange") (circle (+ b_r 5) 20 "orange") (circle (- b_r 1) 100 "white") (circle (- b_r 2) "solid" "gold") (circle b_r "solid" "darkorange") (circle (+ b_r 1) "solid" "darkred")))
(define +ball_image (beside (overlay/align "center" "center" (rectangle 2 8 255 "white") (rectangle 8 2 255 "white")) (circle b_r "solid" "DeepPink")))
(define shield_pu_image (overlay (rectangle 13 2 100 "white") (rectangle 15 4 255 "MediumSlateBlue")))
(define shield_image (overlay (rectangle (- scn_w 5) 4 100 "white") (rectangle scn_w 6 255 "mediumslateblue")))

(define regular_block_image (polygon (list
                                      (make-posn block_curve 0)
                                      (make-posn (- block_w block_curve) 0)
                                      (make-posn block_w block_curve)
                                      (make-posn block_w (- block_h block_curve))
                                      (make-posn (- block_w block_curve) block_h)
                                      (make-posn block_curve block_h)
                                      (make-posn 0 (- block_h block_curve))
                                      (make-posn 0 block_curve)
                                      )
                                     255 "white"
                                     ))
(define powerup_block_image (polygon (list
                                     (make-posn block_curve 0)
                                     (make-posn (- block_w block_curve) 0)
                                     (make-posn block_w block_curve)
                                     (make-posn block_w (- block_h block_curve))
                                     (make-posn (- block_w block_curve) block_h)
                                     (make-posn block_curve block_h)
                                     (make-posn 0 (- block_h block_curve))
                                     (make-posn 0 block_curve)
                                     )
                                    255 "magenta"
                                    ))

(define (degtorad a) (degrees->radians a))

(define (destroy l n)
  (append (take l n) (rest (drop l n)))
  )

(define (add-particle x y n)
  (let ((lst pt_list))
    (let loop ((i 0))
      (if (< i n) (let ((ang (random 360)) (spd (random 1 5)) (life (random 5 10))) (append (loop (add1 i)) (list (list x y life ang (* spd (cos (degtorad ang))) (* spd (sin (degtorad ang)))))))
          lst)
      )
    )
  )

(define (add-fire x y n)
  (let ((lst fr_list))
    (let loop ((i 0))
      (if (< i n) (let ((ang (random 360)) (spd (* 3 (random 2))) (life (random 5))) (append (loop (add1 i)) (list (list x y life ang (* spd (cos (degtorad ang))) (* spd (sin (degtorad ang)))))))
          lst)
      )
    )
  )

(define (decrease_life l pos_l)
  (if (empty? l) l (append (list (list (posn-x (first pos_l)) (posn-y (first pos_l)) (- (list-ref (first l) 2) 0.7) (list-ref (first l) 3) (list-ref (first l) 4) (list-ref (first l) 5))) (decrease_life (rest l) (rest pos_l))))
  )

(define (decrease_yspd l)
  (if (empty? l) l (append (list (list (list-ref (first l) 0) (list-ref (first l) 1) (list-ref (first l) 2) (list-ref (first l) 3) (list-ref (first l) 4) (- (list-ref (first l) 5) 0.2))) (decrease_yspd (rest l))))
  )

(define (increase_yspd l)
  (if (empty? l) l (append (list (list (list-ref (first l) 0) (list-ref (first l) 1) (list-ref (first l) 2) (list-ref (first l) 3) (list-ref (first l) 4) (+ (list-ref (first l) 5) 0.6))) (increase_yspd (rest l))))
  )

(define (move_pt l pt_l)
  (if (empty? l) l (append (list (make-posn (+ (list-ref (first pt_l) 4) (posn-x (first l))) (- (posn-y (first l)) (list-ref (first pt_l) 5) ))) (move_pt (rest l) (rest pt_l))))
  )



(define (render ws)
  (place-images
   (append
    (list
     regular_block_image
     powerup_block_image
     ball_image
     +ball_image
     fire_ball_image
     player_image
     fire_player_image
     shield_pu_image
     shield_image
     )
    pt_images
    fr_images
    )
   (append
    (list
     (make-posn 50 50)
     (make-posn 100 50)
     (make-posn 100 240)
     (make-posn 150 240)
     (make-posn 200 (modulo y 300))
     (make-posn 100 260)
     (make-posn 200 260)
     (make-posn 100 210)
     (make-posn (/ scn_w 2) 280)
     )
    pt_positions
    fr_positions
    )
   (rectangle scn_w scn_h 209 "black"))
  )

(define (key-handler ws ke)
  (cond
    ((key=? ke " ") (begin
                      (set! pt_list (add-particle 150 150 7))
                      (set! pt_images
                            (let ((lst empty))
                              (let loop ((i 0))
                                (if (< i (length pt_list)) (append (list (rotate (random 360) (rectangle 5 5 255 "white"))) (loop (add1 i))) lst)
                                )
                              )
                            )

                      (set! pt_positions
                            (let ((lst empty))
                              (let loop ((i 0))
                                (if (< i (length pt_list)) (append (list (make-posn (list-ref (list-ref pt_list i) 0) (list-ref (list-ref pt_list i) 1))) (loop (add1 i))) lst)
                                )
                              )
                            )))
    )
  )


(define (step ws)
  (begin
    (set! pt_list (decrease_life pt_list pt_positions))
    (set! pt_list (decrease_yspd pt_list))
    (set! pt_positions (move_pt pt_positions pt_list))

    (set! fr_list (decrease_life fr_list fr_positions))
    (set! fr_list (increase_yspd fr_list))
    (set! fr_positions (move_pt fr_positions fr_list))

    (let loop ((i 0))
      (if (< i (length pt_list)) (if (< (list-ref (list-ref pt_list i) 2) 0) (begin
                                                                               (set! pt_images (destroy pt_images i))
                                                                               (set! pt_positions (destroy pt_positions i))
                                                                               (set! pt_list (destroy pt_list i))
                                                                               )(loop (add1 i)))
          void)
      )
    (let loop ((i 0))
      (if (< i (length fr_list)) (if (< (list-ref (list-ref fr_list i) 2) 0) (begin
                                                                               (set! fr_images (destroy fr_images i))
                                                                               (set! fr_positions (destroy fr_positions i))
                                                                               (set! fr_list (destroy fr_list i))
                                                                               )(loop (add1 i)))
          void)
      )

    (if (< countdown 0) (begin
                          (set! fr_list (add-fire 200 (modulo y 300) 4))
                          (set! fr_images
                                (let ((lst empty))
                                  (let loop ((i 0))
                                    (if (< i (length fr_list)) (append (list (rectangle 2 2 255 "orange")) (loop (add1 i))) lst)
                                    )
                                  )
                                )

                          (set! fr_positions
                                (let ((lst empty))
                                  (let loop ((i 0))
                                    (if (< i (length fr_list)) (append (list (make-posn (list-ref (list-ref fr_list i) 0) (list-ref (list-ref fr_list i) 1))) (loop (add1 i))) lst)
                                    )
                                  )
                                )
                          (set! countdown CD)
                          ) (set! countdown (- countdown 1.5)))

    (if (< countdown2 0) (begin
                           (set! pt_list (add-particle 150 70 7))
                           (set! pt_images
                                 (let ((lst empty))
                                   (let loop ((i 0))
                                     (if (< i (length pt_list)) (append (list (rotate (random 360) (rectangle 5 5 255 "white"))) (loop (add1 i))) lst)
                                     )
                                   )
                                 )

                           (set! pt_positions
                                 (let ((lst empty))
                                   (let loop ((i 0))
                                     (if (< i (length pt_list)) (append (list (make-posn (list-ref (list-ref pt_list i) 0) (list-ref (list-ref pt_list i) 1))) (loop (add1 i))) lst)
                                     )
                                   )
                                 )
                           (set! countdown2 CD2)
                           ) (set! countdown2 (- countdown2 1)))

    (set! y (+ 0 y))

    (if (> (length fr_list) 15) (begin
                                  (set! fr_images (take fr_images 15))
                                  (set! fr_positions (take fr_positions 15))
                                  (set! fr_list (take fr_list 15))
                                  ) void)
    )
  )

(big-bang 0
          (on-tick step)
          (on-draw render)
          (on-key key-handler)
          )