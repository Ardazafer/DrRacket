#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;####################################################||  Variable Definitions ||##############

(define-values
  (u1 d1 u2 d2 ptr_r ptr_h p1_y p2_y p1_yto p2_yto scn_w scn_h bg_color fg_color margin spd score_1 score_2 dir_v dir_h b_spd in-game? pause? glow alpha angle)
  (values 0 0 0 0 60 20 300 300 300 300 1000 600 "black" "white" 30 25 0 0 0 1 15 0 #f -1 50 0.785398))

(define b_x (/ scn_w 2))
(define b_y (/ scn_h 2))

;####################################################||  Graphical Definitions ||##############
(define player (rectangle 20 120 "solid" fg_color))
(define ball (place-image (circle 10 "solid" fg_color) 11 11 (circle 11 "solid" bg_color)))
(define scn (empty-scene scn_w scn_h))


;####################################################||  Big-Bang Functions ||#################

(define (draw ws)
  (place-image player margin p1_y (place-image player (- scn_w margin) p2_y (place-image ball b_x b_y scn)))
  )

(define (step ws)
  (begin
    ;Updating the arrow's alpha so that it will disappear in the game.
    (define myarrow (beside (rectangle ptr_h (* ptr_h (/ 2 5)) 255 (if (= in-game? 2) fg_color (color 255 255 255 0))) (rotate -90 (triangle (* ptr_h (/ 4 5)) 255 (if (= in-game? 2) fg_color (color 255 255 255 0))))))
    ;Updating the pointer.
    (define pointer (overlay/align "right" "middle" myarrow (circle ptr_r "outline" (color 255 255 255 0))))
    ;Moving the players.
    (if (= in-game? 1) (set!-values (p1_yto p2_yto) (values (+ p1_yto (* spd (+ u1 d1))) (+ p2_yto (* spd (+ u2 d2))))) void)
    ;Making "PRESS SPACE TO START" text to fade out and in.
    (if (= glow -1) (if (>= alpha 70) (set! alpha (- alpha 15)) (set! glow 1)) (if (<= alpha 240) (set! alpha (+ 15 alpha)) (set! glow -1)))

    ;Updating the scene every tick
    (set! scn (add-line (add-line (add-line (place-image  (place-image (text/font (number->string score_1) 100 fg_color #f "roman" "normal" "bold" #f) (- (/ scn_w 2) 50) 100 (place-image (text/font (number->string score_2) 100 fg_color #f "roman" "normal" "bold" #f) (+ (/ scn_w 2) 50) 100 (place-image (text/font "PRESS “SPACE” TO START" 50 (color 255 255 255 (if (= in-game? 1) 0 (if (= in-game? 0) alpha 0))) #f "roman" "normal" "bold" #f) (/ scn_w 2) (* scn_h (/ 2 3)) (place-image (text/font "PAUSED" 100 (color 255 255 255 (if pause? 255 0)) #f "roman" "normal" "bold" #f) (/ scn_w 2) (/ scn_h 2) (place-image (rotate (* -1 (/ (* 180 (if (< b_x (/ scn_w 2)) angle (* -1 (+ 3.141592653589793 angle)))) pi)) pointer) b_x b_y (place-image (text/font "CLICK ANYWHERE TO START" 30 (color 255 255 255 (if (= in-game? 2) 255 0)) #f "roman" "normal" "bold" #f) (/ scn_w 2) (/ scn_h 2)(rectangle scn_w scn_h "solid" bg_color))))))) (/ scn_w 2) (/ scn_h 2) (empty-scene scn_w scn_h)) 15 10 (- scn_w 15) 10 (pen (color 255 255 255 150) 6 "long-dash" "butt" "bevel")) 15 (- scn_h 10) (- scn_w 15) (- scn_h 10) (pen (color 255 255 255 150) 6 "long-dash" "butt" "bevel")) (/ scn_w 2) 20 (/ scn_w 2) (- scn_h 30) (pen (color 255 255 255 150) 4 "long-dash" "butt" "bevel")))

    ;Limiting the players' y position so that they won't be able to escape from the window.
    (if (<= p2_yto 80) (set! p2_yto 80) (void))
    (if (>= p2_yto (- scn_h 80)) (set! p2_yto (- scn_h 80)) (void))
    (if (<= p1_yto 80) (set! p1_yto 80) (void))
    (if (>= p1_yto (- scn_h 80)) (set! p1_yto (- scn_h 80)) (void))

    ;All the game physics :D.
    (cond
      ((= in-game? 1) (if (not pause?) (begin
                                         ;Smoothing the movement of players.
                                         (set!-values (p1_y p2_y) (values (+ p1_y (/ (- p1_yto p1_y) 4)) (+ p2_y (/ (- p2_yto p2_y) 4))))

                                         ;Vertical ball collisions.
                                         (if (= dir_v 1) ; If the ball is moving downwards along the y axis...
                                             
                                             (if (> b_y 35) ; While the ball stays in the game window...
                                                 (set! b_y (+ b_y (* b_spd (sin angle)))) ; Keep moving downwards
                                                 (set!-values (dir_v angle) (values 0 (* -1 angle)))) ; If it is below the game view change direction.
                                             (if (< b_y (- scn_h 20)) ; Same process for upwards.
                                                 (set! b_y (+ b_y (* b_spd (sin angle))))
                                                 (set!-values (dir_v angle) (values 1 (* -1 angle)))))

                                         ;Horizontal ball collisions.
                                         (if (= dir_h 1) ; If the ball is moving rightwards along the x axis...
                                             (if (< b_x (- scn_w 50)) ; While the ball stays in the game window...
                                                 (set! b_x (+ b_x (* b_spd (cos angle)))) ;Keep moving rightwards
                                                 (if (and (< b_x (- scn_w 25)) (>= b_y (- p2_y 70)) (<= b_y (+ p2_y 70))) ;When the ball passes the x axis of the player
                                                     (set! dir_h 0) ;If it hits the player change direction
                                                     (if (> b_x scn_w) ;Else
                                                         ;If the ball goes out of the window, restart the game
                                                         (set!-values (b_x b_y in-game? pause? score_1 p2_y p1_y p1_yto p2_yto) (values (+ margin 25) (/ scn_h 2) 2 #f (add1 score_1) (/ scn_h 2) (/ scn_h 2) (/ scn_h 2) (/ scn_h 2)))
                                                         (set! b_x (+ b_x b_spd)))));Else, Until the ball goes out of the window keep going.
                                             (if (>= b_x 60);Same process for leftwards
                                                 (set! b_x (- b_x (* b_spd (cos angle))))
                                                 (if (and (> b_x 30) (>= b_y (- p1_y 70)) (<= b_y (+ p1_y 70)))
                                                     (set! dir_h 1)
                                                     (if (< b_x 0)
                                                         (set!-values (b_x b_y in-game? pause? score_2 p2_y p1_y p1_yto p2_yto) (values (- scn_w margin 25) (/ scn_h 2) 2 #f (add1 score_2) (/ scn_h 2) (/ scn_h 2) (/ scn_h 2) (/ scn_h 2)))
                                                         (set! b_x (- b_x b_spd))))))
                                         )
                          void))
      (else void)
      )))


(define (key-handler ws key)
  (cond
    ((= in-game? 1) (if (not pause?) (begin
                                       (cond
                                         ((string=? key "up") (set! u2 -1))
                                         ((string=? key "down") (set! d2 1))
                                         ((string=? key "w") (set! u1 -1)) 
                                         ((string=? key "s") (set! d1 1))
                                         ((or (string=? key "p") (string=? key "escape")) (set! pause? (not pause?)))
                                         (else void)
                                         )
                                       ) (if (or (string=? key "p") (string=? key "escape")) (set! pause? (not pause?)) (void))))
    (else (cond
            ((string=? key " ") (set! in-game? 1))
            (else void)
            ))
    ))

(define (mouse ws x y me)
  (if (= in-game? 2)
      (if (and (or (string=? me "button-down") (string=? me "drag")) (= in-game? 2))
          (set!-values (dir_v angle) (values
                                      (if (> y (/ scn_h 2)) 0 1)
                                      (cond
                                        ((> (atan (/ (- y b_y) (abs (- x b_x 0.000001)))) 0.785398) 0.785398)
                                        ((< (atan (/ (- y b_y) (abs (- x b_x 0.000001)))) -0.785398) -0.785398)
                                        (else (atan (/ (- y b_y) (abs (- x b_x 0.000001))))))))
          (if (string=? me "button-up")
              (set! in-game? 1)
              void))
      void)
  )

(define (release-handler ws key)
  (cond
    ((key=? key "up") (set! u2 0))
    ((key=? key "down") (set! d2 0))
    ((key=? key "w") (set! u1 0))
    ((key=? key "s") (set! d1 0))
    )
  )

;####################################################||  Starting The Game ||##############

(big-bang 0
          (on-key key-handler)
          (on-release release-handler)
          (on-tick step)
          (on-draw draw)
          (on-mouse mouse)
          )