#lang racket/base
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

(define-values
  (lines scn_w scn_h xt_array yt_array xp_array yp_array rect_array x1 y1 x2 y2 mode)
  (values empty 500 500 empty empty empty empty (list (list 0 0 0 0)) 0 0 0 0 "rectangle"))

(define scn (empty-scene scn_w scn_h))

(define (draw-lines xto yto xprev yprev)
  (cond
    ((empty? xto) (if (string=? mode "pen") scn (draw-rect rect_array)))
    (else (add-line (draw-lines (rest xto) (rest yto) (rest xprev) (rest yprev)) (first xprev) (first yprev) (first xto) (first yto) "black" ))
    )
  )

(define (draw-rect r)
  (let loop ((i 0))
    (cond
      ((= i (length r)) (if (string=? mode "rectangle") scn (draw-lines xt_array yt_array xp_array yp_array)))
      (else (place-image (rectangle (list-ref (list-ref r i) 0) (list-ref (list-ref r i) 1) "outline" "black") (list-ref (list-ref rect_array i) 2) (list-ref (list-ref rect_array i) 3) (loop (add1 i))))
      )
    ))

(define (draw ws)
  (begin
    
    (draw-lines xt_array yt_array xp_array yp_array)
    (draw-rect rect_array)
    )
  )

(define (step ws)
  ws
  )

(define (mouse ws x y me)
  (cond
    ((string=? mode "rectangle")
     (cond
       ((string=? me "button-down") (set!-values (x1 y1) (values x y)))
       ((string=? me "drag") (begin (set! rect_array (list-set rect_array (- (length rect_array) 1) (list-set (last rect_array) 0 (abs (- x x1)))))
                                    (set! rect_array (list-set rect_array (- (length rect_array) 1) (list-set (last rect_array) 1 (abs (- y y1)))))
                                    (set! rect_array (list-set rect_array (- (length rect_array) 1) (list-set (last rect_array) 2 (+ x1 (/ (- x x1) 2)))))
                                    (set! rect_array (list-set rect_array (- (length rect_array) 1) (list-set (last rect_array) 3 (+ y1 (/ (- y y1) 2)))))
                                    ))
       ((string=? me "button-up") (set! rect_array (append rect_array (list (list 0 0 0 0)))))
       (else ws)
       )
     )

    ((string=? mode "pen")
     (cond
       ((string=? me "button-down") (set!-values (xt_array yt_array xp_array yp_array) (values (append xt_array (list x)) (append yt_array (list y)) (append xp_array (list x)) (append yp_array (list y)))))
       ((string=? me "drag") (set!-values
                              (xt_array yt_array xp_array yp_array)
                              (values (append xt_array (list x)) (append yt_array (list y))
                                      (if (empty? xp_array)
                                          (append xp_array (list x))
                                          (append xp_array (list (last xt_array))))
                                      (if (empty? yp_array)
                                          (append yp_array (list y))
                                          (append yp_array (list (last yt_array)))))))
    
       (else ws)
       )
     )
    (else ws)
    ))
(define (key-handler ws ke)
  (cond
    ((string=? ke "p") (set! mode "pen"))
    ((string=? ke "r") (set! mode "rectangle"))
    (else ws)
    ))


(define (main init)
  (big-bang init
            (on-mouse mouse)
            (on-tick step)
            (on-draw draw)
            (on-key key-handler)
            ))

(main 0)