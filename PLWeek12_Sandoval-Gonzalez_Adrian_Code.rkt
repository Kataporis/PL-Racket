#lang racket
; Time spent: 20-30 hours

(require racket/draw)  ; Graphics library
(require colors)

; Create an image 2048x1152
(define imageWidth 2048)
(define imageHeight 1152)

(define target (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap target])) ; A drawing context

; Init world bounds (arbitrary)
(define xMin -2560)
(define xMax 2560)
(define yMin -1440)
(define yMax 1440)
(define numFrames 600)

(define initWidth (- xMax xMin)) ; Init width of world space
(define finalWidth 1.0e-12) ; 600th (final) frame should be this size
(define zoomAmount (expt (/ finalWidth initWidth) (/ 1 numFrames))) ; Calculate the required zoom to converge on that value over 600 iterations

; This function converts a polygon from world to screen coords, draws it, then converts back to world coords
(define (draw-to-screen poly xMin xMax yMin yMax)
  (define xScale (/ imageWidth (- xMax xMin)))
  (define yScale (/ imageHeight (- yMax yMin)))

  ; Get center of world space
  (define worldCenterX (/ (+ xMin xMax) 2))
  (define worldCenterY (/ (+ yMin yMax) 2))

  (define xTrans (- (/ imageWidth 2) (* xScale worldCenterX)))
  (define yTrans (- (/ imageHeight 2) (* yScale worldCenterY)))

  ; 1. CONVERT TO SCREEN COORDS
  ; Scale polygon for screen
  (send poly scale xScale yScale)

  ; Translate polygon for screen
  (send poly translate xTrans yTrans)
  
  ; 2. DRAW SCREEN POLYGON
  (send dc draw-path poly)

  ; 3. RESET TO WORLD COORDS
  ; Translate polygon for world
  (send poly translate (- 0 xTrans) (- 0 yTrans))

  ; Scale polygon for world
  (send poly scale (/ 1 xScale) (/ 1 yScale))  
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

; This function makes sequential file names
(define (make-output-name testnum prefix) ; only good up to 999
  (let ((suffix 
         (cond
           [(< testnum 10) (format "00~v.png" testnum)]
           [(< testnum 100) (format "0~v.png" testnum)]
           [ (format "~v.png" testnum)])))
    (string-append prefix suffix)))

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
(send leaf scale 5 7)

; Initialize a global variable to track the smallest polygon size
(define smallestWidth (box +inf.0))

; This function recursively draws a leaf fractal
(define (make-image depth scaleAmount rotateAmount inputPoly xMin xMax yMin yMax)
  (when (> depth 0) ; loop on polygons
    ; Apply transformations
    (send inputPoly scale scaleAmount scaleAmount) ; Scale it
    (send inputPoly rotate rotateAmount) ; Rotate it
    (send inputPoly translate 50 50) ; Translate it

    ; Get the current bounding box to check width
    (define-values (left top width height) (send inputPoly get-bounding-box))

    ; Draw the polygon if it's still visible and meaningful
    (when (> width 1.0e-13) ; Check if still larger than the absolute smallest allowed * an order of magnitude
      (draw-to-screen inputPoly xMin xMax yMin yMax) ; Draw it
      (when (< width (unbox smallestPolyWidth))
        (set-box! smallestPolyCoords (cons left top))
        (set-box! smallestPolyWidth width))

    (make-image (- depth 1) scaleAmount rotateAmount inputPoly xMin xMax yMin yMax))) ) ; Recursive call

; Init variables
(define depth 10100)
(define numAngles 6)
(define scaleAmount 0.99)
(define smallestPolyCoords (box (cons +inf.0 +inf.0))) ; Track coords of smallest polygon to zoom on it. Initialize
(define centerX 58)
(define centerY -7)
(define smallestPolyWidth (box +inf.0))

; ASSIGNMENT 12
; Zoom into the center of the image ~600 times
(for ([i (in-range numFrames)])
  ; Draw a rectangle to fill the screen with a background color
  (send dc set-pen "black" 1 'transparent) ; pen color line_width fill_mode
  (send dc set-brush (nice-make-color 50 0 0) 'solid)  ; fill color line_width fill_mode
  (send dc draw-rectangle  0 0  imageWidth imageHeight)  ; background color

  ; Set pen color and fill
  (send dc set-pen "black" 4 'solid) ; pen color line_width fill_mode
  ; Cycle through colors per every 100 frames
  (cond
    [(< i 100)
     (send dc set-brush (nice-make-color 0 (random 125 150) (random 100 125)) 'solid)] ; Full green/blue
    [(< i 200)
     (send dc set-brush (nice-make-color 0 (random 125 150) (random 75 100)) 'solid)] ; Cut down on blue
    [(< i 300)
     (send dc set-brush (nice-make-color (random 25) (random 125 150) (random 25 50)) 'solid)] ; Intro red
    [(< i 400)
     (send dc set-brush (nice-make-color (random 25 50) (random 100 125) 0) 'solid)]
    [(< i 500)
     (send dc set-brush (nice-make-color (random 50 75) (random 75 100) 0) 'solid)]
    [(< i numFrames)
     (send dc set-brush (nice-make-color (random 75 100) (random 50 75) 0) 'solid)])
  
  ; Track the window size
  (printf "Iteration #~v: Width = ~v, Height = ~v\n" (+ i 1) (- xMax xMin) (- yMax yMin))
  
  ; Draw the fractal
  (for* ([a (in-range numAngles)]) ; Varies the angles
    (define angle (* a (/ (/ pi 2) numAngles))) ; Calculate the current angle
    (define newPoly (dupe-poly leaf)) ; Duplicate the initial leaf

    ; Call make-image with the updated bounds
    (make-image depth scaleAmount angle newPoly xMin xMax yMin yMax))

  ; Get target coords
  (when (> i 4)
    (define centerCoords (unbox smallestPolyCoords))
    (set! centerX (car centerCoords))
    (set! centerY (cdr centerCoords)))
  
; Compute new bounds based on this center
(define newHalfWidth (* zoomAmount (/ (- xMax xMin) 2)))
(define newHalfHeight (* zoomAmount (/ (- yMax yMin) 2)))

(define newXMin (- centerX newHalfWidth))
(define newXMax (+ centerX newHalfWidth))
(define newYMin (- centerY newHalfHeight))
(define newYMax (+ centerY newHalfHeight))

; Update bounds for the next iteration
(set! xMin newXMin)
(set! xMax newXMax)
(set! yMin newYMin)
(set! yMax newYMax)

; Save the image with a numbered filename
(define outName (make-output-name (+ i 1) "testImage"))
(send target save-file outName 'png))

(printf "All done!")