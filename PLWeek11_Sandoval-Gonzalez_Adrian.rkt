#lang racket

(require racket/draw)  ; Graphics library
(require colors)

; Create an image 2048x1152
(define imageWidth 2048)
(define imageHeight 1152)

(define target (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap target])) ; A drawing context

; This function converts a polygon from world to screen coords, draws it, then converts back to world coords
(define (draw-to-screen poly)
  (define-values (woo hoo) (send poly get-datum))
  (define polyCoords (caar woo))

  ; Get x and y coords
  (define xCoord (vector-ref polyCoords 0))
  (define yCoord (vector-ref polyCoords 1))

  (define screenScale 0.4)
  (define xTrans (/ imageWidth 2))
  (define yTrans (/ imageHeight 2))

  ; 1. CONVERT TO SCREEN COORDS
  ; Scale polygon for screen
  (send poly scale screenScale screenScale)

  ; Translate polygon for screen
  (send poly translate xTrans yTrans)
  
  ; 2. DRAW SCREEN POLYGON
  (send dc draw-path poly)

  ; 3. RESET TO WORLD COORDS
  ; Translate polygon for world
  (send poly translate (- 0 xTrans) (- 0 yTrans))

  ; Scale polygon for world
  (send poly scale (/ 1 screenScale) (/ 1 screenScale))  
  )

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

; Function to duplicate a polygon
(define (dupe-poly oldPolygon)
  ; Create a new path object
  (define newPolygon (new dc-path%))
  ; Copy the content of the old polygon to the new one
  (send newPolygon append oldPolygon)
  ; Return the new polygon
  newPolygon) 

; Draw a rectangle to fill the screen with a background color
(send dc set-pen "black" 1 'transparent) ; pen color line_width fill_mode
(send dc set-brush (nice-make-color 50 0 0) 'solid)  ; fill color line_width fill_mode
(send dc draw-rectangle  0 0  imageWidth imageHeight)  ; background color

; Set pen color and fill
(send dc set-pen "black" 3 'solid) ; pen color line_width fill_mode
(send dc set-brush (nice-make-color 0 100 0) 'solid)  ; fill color line_width fill_mode

(define leaf (new dc-path%))
(send leaf move-to 100 350) ; start point (same as the bottom left, now top left)
(send leaf line-to 0 600) ; inverted first peak
(send leaf line-to 125 550) ; inverted valley
(send leaf line-to 150 600) ; inverted peak
(send leaf line-to 175 550) ; inverted valley
(send leaf line-to 200 600) ; inverted peak
(send leaf line-to 225 550) ; inverted valley
(send leaf line-to 250 600) ; inverted peak
(send leaf line-to 275 550) ; inverted valley
(send leaf line-to 400 600) ; inverted last peak
(send leaf line-to 300 350) ; end point (same as bottom right, now top right)
(send leaf close) ; close the path
  
; Transform inverted leaf
(send leaf scale 10 7)

(define polyCount (box 0)) ; Init poly counter as mutable box

; Initialize a global variable to track the smallest polygon size
(define smallestWidth (box +inf.0))

; This function recursively draws a leaf fractal
(define (make-image depth scaleAmount rotateAmount inputPoly)
  (when (> depth 0) ; loop on polygons
    ; Apply transformations
    (send inputPoly scale scaleAmount scaleAmount) ; Scale it
    (send inputPoly rotate rotateAmount) ; Rotate it
    (send inputPoly translate -50 -50) ; Translate it

    ; Get the current bounding box to check width
    (define-values (left top width height) (send inputPoly get-bounding-box))

    ; Draw the polygon if it's still visible and meaningful
    (when (> width 1.0e-13) ; Check if still larger than the absolute smallest allowed * -10
      (draw-to-screen inputPoly) ; Draw it
      
      ; Update the smallest width encountered
      (when (< width (unbox smallestWidth))
        (set-box! smallestWidth width))
      (set-box! polyCount (+ (unbox polyCount) 1)) ; Increment polygon count
      
      ; Ramp up the scaling gradually once the depth is less than 30
      (define new-scale (if (< depth 30)
                           (* scaleAmount 0.9)
                           scaleAmount)) ; Else, keep it the same

      ; Recursive call
      (make-image (- depth 1) new-scale rotateAmount inputPoly))) )

(define depth 150)
(define numAngles 6)
(define scaleAmount 0.98)

; LANGUAGE FEATURE
; Iterates 900 times (150 depths * 6 angles). Creates multiple instances of the fractal drawing process.
; Varies depth: Controls how many times 'make-image' recursively calls itself. In other words, how many layers of a leaf are drawn.
; Varies angles: Controls the initial rotation of the leaf to distribute them evenly.
(for* ([d (in-range 1 (+ depth 1))] ; Varies the depth
       [a (in-range numAngles)]) ; Varies the angles
  (define angle (* a (/ (/ pi 2) numAngles))) ; Calculate the current angle
  (define newPoly (dupe-poly leaf)) ; Duplicate the initial leaf
  (make-image d scaleAmount angle newPoly)) ; Recursive call decrements the depth each time

(send target save-file "assignment11_sandoval-gonzalez_adrian.png" 'png) ; save image as png

(printf "Polygon count: ~v\n" (unbox polyCount))
(printf "Smallest polygon width: ~v\n" (unbox smallestWidth))
target    ; Display image