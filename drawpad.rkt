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

(define default-pen (new pen% [color "BLACK"] [width 4]))

(struct coord (x y))

(define (event->coord event)
   (coord (send event get-x) (send event get-y)))

(define (coord-add c1 c2)
  (coord (+ (coord-x c1) (coord-x c2))
         (+ (coord-y c1) (coord-y c2))))

(define (coord-subtract c1 c2)
  (coord (- (coord-x c1) (coord-x c2))
         (- (coord-y c1) (coord-y c2))))

(define line%
  (class object%

    (init-field [coords '()]
                [pen default-pen])
    
    (define/public (last-coord)
      (car coords))
    
    (define/public (add-coord coord)
      (set! coords (cons coord coords)))

    (define/public (draw dc offset-coord)
      (send dc set-pen pen)
      (foldl (λ (coord1 coord2)
               (let ((ocoord1 (coord-add offset-coord coord1))
                     (ocoord2 (coord-add offset-coord coord2)))
                 (send dc
                       draw-line
                       (coord-x ocoord1) (coord-y ocoord1)
                       (coord-x ocoord2) (coord-y ocoord2)))
                 coord1) (car coords) coords))

    (super-new)))

(define frame (new frame% [label "Drawpad"]
                   [min-width 300]
                   [min-height 300]))

(send frame create-status-line)
(send frame set-status-text "Ready.")

(define my-canvas%
  (class canvas%

    ; #f when mouse is not down, otherwise stores the current line
    (field (current-line #f))
    (field (dragging #f))
    (field (lines '()))
    (field (pen default-pen))
    (field (offset-coord (coord 0 0)))

    (define/public (clear-lines)
      (set! lines '())
      (send this refresh))

    (define/private (draw-lines)
      (let ((dc (send this get-dc)))
        (for ([l lines])
          (send l draw dc offset-coord))))

    (define/public (undo)
      (set! lines (cdr lines))
                 (send this refresh)
                 (draw-lines))

    (define/private (screen-coord-to-absolute coord)
      (coord-subtract coord offset-coord))

    (define/private (absolute-to-screen-coord coord)
      (coord-add coord offset-coord))

    (define/private (on-drag event)
      (set! offset-coord
            (coord-add offset-coord
                       (coord-subtract (event->coord event)
                                       dragging)))
      (send this refresh)
      (draw-lines)
      (set! dragging (event->coord event)))

    (define/private (on-draw event)
      (let* ([current-coord (event->coord event)]
             [current-adjusted (screen-coord-to-absolute current-coord)]
             [previous-coord (send current-line last-coord)]
             [previous-adjusted (absolute-to-screen-coord previous-coord)])
        (send (send this get-dc)
              draw-line
              (coord-x previous-adjusted) (coord-y previous-adjusted)
              (coord-x current-coord) (coord-y current-coord))
        (send current-line add-coord  current-adjusted)))

    ; when mouse button is clicked we go to dragging or drawing depending on if ctrl is down
    (define/private (on-mouse-down event)
      (cond ((send event get-control-down)
             (set! dragging (event->coord event))
             (send frame set-status-text "Dragging..."))
            (else
             (set! current-line (new line% [pen pen]))
             (send current-line add-coord (screen-coord-to-absolute (event->coord event)))
             (send frame set-status-text "Drawing..."))))

    ; save any line drawn and reset all the state
    (define/private (on-mouse-up event)
      (when current-line
        (set! lines (cons current-line lines)))
      (set! current-line #f)
      (set! dragging #f)
      (send frame set-status-text "Ready."))

    ; mouse events
    (define/override (on-event event)
      (cond ((coord? dragging)
             (on-drag event))
            (current-line
             (on-draw event)))
      (let ((event-type (send event get-event-type)))
        (cond ((equal? event-type 'left-down)
               (on-mouse-down event))
              ((equal? event-type 'left-up)
               (on-mouse-up event)))))

    ; keyboard events
    (define/override (on-char event)
      (when (send event get-control-down)
        (let ((key-code (send event get-key-code)))
          (cond ((equal? key-code #\z)
                 (undo))
                ((equal? key-code #\=)
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

    (define/override (on-paint)
      (draw-lines))
    
    (super-new)))

; Make the drawing area
(define canvas (new my-canvas% [parent frame]))
(send (send canvas get-dc) set-pen default-pen) 

(define menu-bar (new menu-bar% [parent frame]))

(define file-menu
  (new menu%
    [label "&File"]
    [parent menu-bar]))

(new menu-item%
  [label "&New"]
  [parent file-menu]
  [callback (λ (m event)
              (send canvas clear-lines))])

(new menu-item%
  [label "E&xit"]
  [parent file-menu]
  [callback (λ (m event)
              (send frame show #f))])

(define edit-menu
  (new menu%
    [label "&Edit"]
    [parent menu-bar]))

(new menu-item%
  [label "Undo (Ctrl+Z)"]
  [parent edit-menu]
  [callback (λ (m event)
              (send canvas undo))])

; Show the frame
(send frame show #t)
