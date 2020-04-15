;;; richii.el --- Implementation of Richii (Japanese) Mahjong for Emacs. -*- lexical-binding: t; -*-

;; TODO: license here

;;; Commentary:

;;; Code:
(require 'esxml)

;; TODO: make this a defcustom
(defconst richii/richii-buffer-name "*richii*"
  "The name of the buffer created when starting richii mode.")

(defun richii/make-numeric-tileset (name)
  "Return a mahjong tileset of the symbol NAME."
  (loop
   for i from 1 upto 9
   collect `(,name . ,i)))

(defconst richii/sou-tiles (richii/make-numeric-tileset 'sou))
(defconst richii/pin-tiles (richii/make-numeric-tileset 'pin))
(defconst richii/man-tiles (richii/make-numeric-tileset 'man))

(defconst richii/wind-tiles '((wind . north)
			      (wind . south)
			      (wind . east)
			      (wind . west)))

(defconst richii/dragon-tiles '((dragon . green)
				(dragon . red)
				(dragon . white)))

(defconst richii/tileset
  ;; Splice all five tilesets together, then duplicate twice.
  (let ((l (append richii/sou-tiles
		   richii/pin-tiles
		   richii/man-tiles
		   richii/wind-tiles
		   richii/dragon-tiles)))
    (append l l l l))
  "Create the full set of richii tiles.
This includes all three sets of simple tiles: Sou, Pin, and Man,
and the honor sets: Winds and Dragons.")

(defun richii/kshuffle (list)
  "Perform a Knuth/Fisher-Yates shuffle on LIST.
Taken from https://stackoverflow.com/a/49505968."
  (loop for i from (length list) downto 2
	do (rotatef (elt list (random i))
		    (elt list (1- i))))
  list)

(defun richii/make-hands ()
  "Return a list of 4 shuffled hands (themselves lists).
First a full tileset is made, then shuffled, then split into four."
  (let* ((tileset (richii/kshuffle richii/tileset))
	 (hand-size (/ (length tileset) 4)))
    (seq-partition tileset hand-size)))

(defun richii/draw-tile (tile offset)
  "Return an svg representation of TILE, drawn at OFFSET."
  `(rect ((x . ,(* offset 12))
	  (y . 0)
	  (width . 10)
	  (height . 20))))

(defun richii/draw-closed-hand (hand side)
  "Draw a closed hand HAND on SIDE. Called tiles will be shown,
but all others drawn upright and obscured.")

(defun richii/draw-open-hand (hand side)
  "Draw a single HAND on screen, where SIDE is one of 'top,
'bottom, 'left, 'right. (NEWS are not used to avoid confusion.)"
       ;; Draw each hand at incrementing offsets
  (loop for tile in hand
	for x from 0
	do (richii/draw-tile tile x)))

(defun richii/draw-hands (hands)
  "Draw four HANDS on screen, where HANDS is a list of four hand lists."
  '())

(defun richii ()
  "Start a game of richii mahjong."
  ;; Make new buffer/clear restart current
  (switch-to-buffer richii/richii-buffer-name)
  (erase-buffer)
  (setq svg (svg-create (window-pixel-width) (window-pixel-height)))
  
  ;; Go richii-mode
  
  ;; Setup hands
  
  
  ;; Draw em
  (svg-insert-image (append svg (draw-hands )))
  
  ;; Start game
  ;; At end of each turn, redraw hands.
  ;; Player turn, set hand.
  ;; compute AI turns, setting hand.
  ;; 
  )

(provide 'richii)
;;; richii.el ends here
