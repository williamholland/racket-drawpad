#lang racket/gui

(struct coord (x y))

(define (event->coord event)
   (coord (send event get-x) (send event get-y)))

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
      (send frame set-status-text "Canvas keyboard."))
    
    (super-new)))

; Make the drawing area
(define canvas (new my-canvas% [parent frame]))

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
