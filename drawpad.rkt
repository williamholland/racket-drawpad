#lang racket/gui

(define pink (make-color #xff #x69 #xb4))
(define cyan (make-color #x00 #xff #xff))
(define yellow (make-color #xff #xff #x00))
(define red (make-color #xff #x45 #x00))
(define green (make-color #x7f #xff #x00))
(define purple (make-color #x94 #x00 #xd3))
(define white (make-color #xff #xff #xff))
(define grey (make-color #x82 #x82 #x82))

(struct coord (x y))

(define (event->coord event)
   (coord (send event get-x) (send event get-y)))

(define pen (new pen% [color "BLACK"] [width 1]))

(define frame (new frame% [label "Drawing"]
                   [min-width 300]
                   [min-height 300]))

(send frame create-status-line)
(send frame set-status-text "Ready.")

(define my-canvas%
  (class canvas%

    ; #f when mouse is not down, otherwise stores the most recent previous coord of the pen
    (define drawing-state #f)

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
        (cond ((equal? (send event get-key-code) #\=)
               (set! pen (new pen%
                              [color (send pen get-color)]
                              [width (+ (send pen get-width) 1)]))
               (send frame set-status-text "Pen size +"))
              ((equal? (send event get-key-code) #\-)
               (set! pen (new pen%
                              [color (send pen get-color)]
                              [width (max 0 (- (send pen get-width) 1))]))
               (send frame set-status-text "Pen size -"))
              ((equal? (send event get-key-code) #\1)
               (set! pen (new pen%
                              [color pink]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color pink"))
              ((equal? (send event get-key-code) #\2)
               (set! pen (new pen%
                              [color cyan]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color cyan"))
              ((equal? (send event get-key-code) #\3)
               (set! pen (new pen%
                              [color yellow]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color yellow"))
              ((equal? (send event get-key-code) #\4)
               (set! pen (new pen%
                              [color red]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color red"))
              ((equal? (send event get-key-code) #\5)
               (set! pen (new pen%
                              [color green]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color green"))
              ((equal? (send event get-key-code) #\6)
               (set! pen (new pen%
                              [color purple]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color purple"))
              ((equal? (send event get-key-code) #\7)
               (set! pen (new pen%
                              [color white]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color white"))
              ((equal? (send event get-key-code) #\8)
               (set! pen (new pen%
                              [color grey]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color grey"))
              ((equal? (send event get-key-code) #\9)
               (set! pen (new pen%
                              [color "BLACK"]
                              [width (send pen get-width)]))
               (send frame set-status-text "Pen color black")))
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
