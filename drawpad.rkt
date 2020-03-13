#lang racket/gui

; maps number keys to colours
(define colours (hash
 #\1 (make-color #xff #x69 #xb4) ;pink
 #\2 (make-color #x00 #xff #xff) ; cyan
 #\3 (make-color #xff #xff #x00) ; yellow
 #\4 (make-color #xff #x45 #x00) ; red
 #\5 (make-color #x7f #xff #x00) ; green
 #\6 (make-color #x94 #x00 #xd3) ; purple
 #\7 (make-color #xff #xff #xff) ; white
 #\8 (make-color #x82 #x82 #x82) ; grey
 #\9 (make-color #x00 #x00 #x00))) ; black

(struct coord (x y))

(define (event->coord event)
   (coord (send event get-x) (send event get-y)))

(define pen (new pen% [color "BLACK"] [width 1]))

(define frame (new frame% [label "Drawpad"]
                   [min-width 300]
                   [min-height 300]))

(send frame create-status-line)
(send frame set-status-text "Ready.")

(define my-canvas%
  (class canvas%

    ; #f when mouse is not down, otherwise stores the most recent previous coord of the pen
    (field (drawing-state #f))

    ; mouse events
    (define/override (on-event event)
      (when (coord? drawing-state)
        (let ((current-coord (event->coord event)))
          (send (send this get-dc)
                draw-line
                (coord-x drawing-state) (coord-y drawing-state)
                (coord-x current-coord) (coord-y current-coord))
          (set! drawing-state current-coord)))
      (let ((event-type (send event get-event-type)))
        (cond ((equal? event-type 'left-down)
                 (set! drawing-state (event->coord event))
                 (send frame set-status-text "Drawing..."))
              ((equal? event-type 'left-up)
               (send frame set-status-text "Ready.")
               (set! drawing-state #f)))))

    ; keyboard events
    (define/override (on-char event)
      (when (send event get-control-down)
        (let ((key-code (send event get-key-code)))
          (cond ((equal? key-code #\=)
                 (set! pen (new pen%
                                [color (send pen get-color)]
                                [width (+ (send pen get-width) 1)]))
                 (send frame set-status-text "Pen size +"))
                ((equal? key-code #\-)
                 (set! pen (new pen%
                                [color (send pen get-color)]
                                [width (max 0 (- (send pen get-width) 1))]))
                 (send frame set-status-text "Pen size -"))
                ((and (char? key-code) (char-numeric? key-code))
                 (set! pen (new pen%
                              [color (hash-ref colours key-code)]
                              [width (send pen get-width)]))
                 (send frame set-status-text "Pen colour changed."))))
        (send (send canvas get-dc) set-pen pen)))
    
    (super-new)))

; Make the drawing area
(define canvas (new my-canvas% [parent frame]))
(send (send canvas get-dc) set-pen pen) 

(define menu-bar (new menu-bar% [parent frame]))

(define file-menu
  (new menu%
    [label "&File"]
    [parent menu-bar]))

(new menu-item%
  [label "&New"]
  [parent file-menu]
  [callback (λ (m event)
              (send canvas refresh))])

(new menu-item%
  [label "E&xit"]
  [parent file-menu]
  [callback (λ (m event)
              (send frame show #f))])

; Show the frame
(send frame show #t)
