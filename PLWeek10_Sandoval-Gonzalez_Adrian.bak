#lang racket

(require racket/draw)  ; Graphics library
(require colors)

; PART ONE

; Create and image 512x288
(define imageWidth 512)
(define imageHeight 288)

(define target (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap target])) ; A drawing context

; This function protects against crashing if a color is outside 0-255
(define (nice-make-color r g b)
  (make-color
   (if (< r 0)
       0
       (if (> r 255) 255 r))
   (if (< g 0)
       0
       (if (> g 255) 255 g))
   (if (< b 0)
       0
       (if (> b 255) 255 b))
   ))

; Draw a rectangle to fill the screen with a background color
(send dc set-pen "black" 1 'transparent) ; pen color line_width fill_mode
(send dc set-brush (nice-make-color 50 50 100) 'solid)  ; fill color line_width fill_mode
(send dc draw-rectangle  0 0  imageWidth imageHeight)  ; background color

; Set pen color and fill
(send dc set-pen "green" 2 'solid) ; pen color line_width fill_mode
(send dc set-brush (nice-make-color 50 0 100) 'solid)  ; fill color line_width fill_mode

; Draw initial square (polygon with four points)
(define mySquare (new dc-path%)) ; Create polygon
(send mySquare move-to -10 -10)
(send mySquare line-to 10 -10)
(send mySquare line-to 10 10)
(send mySquare line-to -10 10)
(send mySquare close)

; Draw og square
(send dc draw-path mySquare)

; Translate and draw again
(send mySquare translate 50 50)
(send dc draw-path mySquare) ; draw polygon

; Scale and draw again
(send mySquare scale 2 2) ; polygon scale
(send dc draw-path mySquare) ; draw polygon

; PART TWO

; Translate to draw ring that cycles through colors
(send mySquare translate 100 100) ; then polygon translate

; Set pen color and fill
(send dc set-pen "black" 2 'transparent) ; pen color line_width fill_mode

; Draw a loop that cycles through every color in the color wheel.
(for ([i (in-range 127)])
  (define hue (/ i 127.0))     ; Noramlize the hue value to values between 0 and 1
  (define myHsv (hsv hue 1 1)) ; Create HSV with saturation and value at 1
  (define myColor (hsv->color myHsv)) ; Convert HSV to RGB
  (send dc set-brush myColor 'solid)
  (send mySquare rotate 0.05) ; Rotate the square
  (send dc draw-path mySquare)) ; Draw the polygon

(send target save-file "sandoval-gonzalez_assignment9.png" 'png) ; save image as png

target    ; Display image