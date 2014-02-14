(define images
  (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11"))
;  (build-list (lambda (i) (+ "" i) 11)))

  ;(list "white" "black" "red" "green" "blue"
  ;      "stripe-1" "stripe-2" "stripe-3" "stripe-4" "stripe-5" "stripe-6" "stripe-7" "stripe-8" "stripe-9"
  ;      "dots-1" "dots-2" "dots-3" "dots-4" "dots-5" "dots-6"
  ;      "gradient-top" "gradient-left" "gradient-top" "gradient-bottom" "gradient-spot"))

(define (choose l)
  (list-ref l (random (length l))))

(define (transform x y r s) (list x y r s))

(define (transform-x t) (list-ref t 0))
(define (transform-y t) (list-ref t 1))
(define (transform-rotate t) (list-ref t 2))
(define (transform-scale t) (list-ref t 3))

(define (make-terminal)
  (list
   "terminal"
   (transform (random 127) (random 127) (choose (list 0 (/ 6.282 4))) 1)
   (choose images)))

(define (program-type t) (list-ref t 0))
(define (terminal-transform t) (list-ref t 1))
(define (terminal-image t) (list-ref t 2))

(define (make-operator a b)
  (list
   "op"
   (choose
    (list "copy" "destination-atop" "destination-in"
          "destination-out" "destination-over" "source-atop" "source-in"
          "source-out" "source-over" "lighter" "xor"))
   a b))

(define (operator-type t) (list-ref t 1))
(define (operator-operand-a t) (list-ref t 2))
(define (operator-operand-b t) (list-ref t 3))

(define (make-random-egg depth)
  (if (or (zero? depth) (< (random 10) 2))
      (make-terminal)
      (make-operator (make-random-egg (- depth 1))
                     (make-random-egg (- depth 1)))))


(console.log "started egglab")

(define egg 0)
(define canvas (document.getElementById "canvas"))
(define ctx (canvas.getContext "2d"))

(set! ctx.fillStyle "#000000")
(set! ctx.strokeStyle "#ffffff")

(define image-lib ())

(define (load-image! fn finished)
  (let ((image (js "new Image()")))
    (set! image.onload
          (lambda ()
            (set! image-lib (cons (list fn image) image-lib))
            (when (eq? (length image-lib)
                       (length images))
                  (finished))))
    (set! image.src (+ "images/themes/natural" fn ".png"))))

(define (load-images! l finished)
  (for-each
   (lambda (fn)
     (load-image! fn finished))
   l))

(define (find-image fn l)
  (cond
   ((null? l) #f)
   ((eq? (car (car l)) fn) (cadr (car l)))
   (else (find-image fn (cdr l)))))

(define (draw-egg ctx x y program)
  (if (eq? (program-type program) "terminal")
      (begin
        (set! ctx.fillStyle
              (ctx.createPattern
               (find-image (terminal-image program) image-lib) "repeat"))

        (ctx.translate 64 64)
        (ctx.rotate (transform-rotate (terminal-transform program)))
        (ctx.translate -64 -64)

        (ctx.translate (transform-x (terminal-transform program))
                       (transform-y (terminal-transform program)))

        ;;        (ctx.scale (transform-scale (terminal-transform program))
        ;;                   (transform-scale (terminal-transform program)))

        (ctx.fillRect (- 0 (transform-x (terminal-transform program)))
                      (- 0 (transform-y (terminal-transform program)))
                      (* 127 2) (* 127 2))

        ;;        (ctx.scale (/ 1 (transform-scale (terminal-transform program)))
        ;;                   (/ 1 (transform-scale (terminal-transform program))))

        (ctx.translate (- 0 (transform-x (terminal-transform program)))
                       (- 0 (transform-y (terminal-transform program)))))
      (begin
        (ctx.save)
        (set! ctx.globalCompositeOperation (operator-type program))
        (ctx.save)
        (draw-egg ctx x y (operator-operand-a program))
        (ctx.restore)
        (ctx.save)
        (draw-egg ctx x y (operator-operand-b program))
        (ctx.restore)
        (ctx.restore))))

(define (draw-eggs ctx w c)
  (when (not (zero? c))
        ;(ctx.save)
        (let ((canvas (document.createElement "canvas")))
          (set! canvas.width 128)
          (set! canvas.height 128)
          (let ((tctx (canvas.getContext "2d"))
                (x (* (modulo (- c 1) w) 127))
                (y (* (Math.floor (/ (- c 1) w)) 127)))

            (console.log (+ "drawing egg" c))
            (let ((egg (make-random-egg 12)))
              ;;(console.log egg)
              (draw-egg tctx 0 0 egg))

            (ctx.putImageData
             (tctx.getImageData 0 0 127 127) x y)

            (ctx.drawImage egg x y)
            ))
        (window.setTimeout (lambda () (draw-eggs ctx w (- c 1))) 500)))


(set! egg (js "new Image()"))

(set! egg.onload
      (lambda ()
        (load-images!
         images
         (lambda ()
           (console.log "drawing egg")
           (draw-eggs ctx 4 16)))))

(set! egg.src "images/egg.png")
