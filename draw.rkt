#lang racket/gui

; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

; this draws a circle on the canvas when the mouse is down
(define (draw-circle dc x y)
  (let ((width 4)
        (height 4))
    (send dc set-pen no-pen) ; outline of shape
    (send dc set-brush blue-brush)
    (send dc draw-ellipse (- x (/ width 2)) (- y (/ height 2)) width height)))

(define frame (new frame% [label "Drawing"]
                   [width 300]
                   [height 300]))

;TODO replace with a status bar
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define my-canvas%
  (class canvas%

    (define draw-state #f)

    ; mouse events
    (define/override (on-event event)
      (let ((event-type (send event get-event-type)))
        (cond ((equal? event-type 'left-down)
               (set! draw-state #t))
              ((equal? event-type 'left-up)
               (set! draw-state #f)))
        (when draw-state
          (let ((mouse-x (send event get-x))
                (mouse-y (send event get-y)))
            (draw-circle (send this get-dc) mouse-x mouse-y)))))

    ; keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    
    (super-new)))

; Make the drawing area
(define canvas (new my-canvas% [parent frame]))

; Show the frame
(send frame show #t)
