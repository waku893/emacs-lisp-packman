;;; pacman.el --- Simple Pac-Man game in Emacs Lisp -*- lexical-binding: t; -*-

;; Author: Codex
;; Version: 0.1

;;; Commentary:
;; This is a very small Pac-Man clone implemented entirely in Emacs Lisp.
;; Use the arrow keys to move Pac-Man around the board. Eat all of the
;; pellets (.) while avoiding ghosts (G). When all pellets are eaten you win.
;; If a ghost collides with you, the game is over.

;;; Code:

(require 'cl-lib)

(defvar pacman-board
  ["#####################"
   "#.........#.........#"
   "#.###.###.#.###.###.#"
   "#G###.###.#.###.###G#"
   "#...................#"
   "#.###.#.#####.#.###.#"
   "#.....#...#...#.....#"
   "#####.### # ###.#####"
   "    #.#   P   #.#    "
   "#####.# ##### #.#####"
   "#.........#.........#"
   "#.###.###.#.###.###.#"
   "#G..#..... .....#..G#"
   "###.#.#.#####.#.#.###"
   "#.....#...#...#.....#"
   "#.#######.#.#######.#"
   "#...................#"
   "#####################"],
  "Initial board layout for Pac-Man.")

(defvar pacman-board-height (length pacman-board))
(defvar pacman-board-width (length (aref pacman-board 0)))

(defvar pacman-player ?P)
(defvar pacman-player-x 0)
(defvar pacman-player-y 0)

(defvar pacman-ghosts nil
  "List of (X . Y) positions for ghosts.")

(defvar pacman-score 0)
(defvar pacman-pellets 0)

(defvar pacman-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'pacman-move-left)
    (define-key map (kbd "<right>") #'pacman-move-right)
    (define-key map (kbd "<up>") #'pacman-move-up)
    (define-key map (kbd "<down>") #'pacman-move-down)
    map)
  "Keymap for Pac-Man mode.")

(defun pacman-init ()
  "Initialize game state from `pacman-board'."
  (setq pacman-score 0)
  (setq pacman-ghosts nil)
  (setq pacman-pellets 0)
  (dotimes (y pacman-board-height)
    (let ((row (copy-sequence (aref pacman-board y))))
      (dotimes (x pacman-board-width)
        (let ((ch (aref row x)))
          (cond
           ((eq ch ?P)
            (setq pacman-player-x x)
            (setq pacman-player-y y)
            (aset row x ? ))
           ((eq ch ?G)
            (push (cons x y) pacman-ghosts)
            (aset row x ?.))
           ((eq ch ?.)
            (cl-incf pacman-pellets)))))
      (aset pacman-board y row))))

(defun pacman-ghost-at (x y)
  "Return a ghost cons cell at X,Y or nil if empty."
  (cl-find-if (lambda (pos) (and (= (car pos) x)
                                 (= (cdr pos) y)))
              pacman-ghosts))

(defun pacman-draw ()
  "Render the Pac-Man board in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (y pacman-board-height)
      (let ((row (copy-sequence (aref pacman-board y))))
        (dotimes (x pacman-board-width)
          (cond
           ((and (= x pacman-player-x) (= y pacman-player-y))
            (aset row x pacman-player))
           ((pacman-ghost-at x y)
            (aset row x ?G))))
        (insert row)
        (insert "\n")))
    (insert (format "Score: %d  Pellets left: %d" pacman-score pacman-pellets))))

(defun pacman-valid-move-p (x y)
  "Return t if position X,Y is not a wall." 
  (and (>= x 0) (< x pacman-board-width)
       (>= y 0) (< y pacman-board-height)
       (let ((ch (aref (aref pacman-board y) x)))
         (not (eq ch ?#)))))

(defun pacman-move-player (dx dy)
  "Move player by DX,DY if possible, then move ghosts." 
  (let ((nx (+ pacman-player-x dx))
        (ny (+ pacman-player-y dy)))
    (when (pacman-valid-move-p nx ny)
      (setq pacman-player-x nx
            pacman-player-y ny)
      (let ((ch (aref (aref pacman-board ny) nx)))
        (when (eq ch ?.)
          (setf (aref (aref pacman-board ny) nx) ? )
          (cl-incf pacman-score)
          (cl-decf pacman-pellets))))
    (pacman-move-ghosts)
    (pacman-draw)
    (pacman-check-game-over)))

(defun pacman-move-left ()
  (interactive) (pacman-move-player -1 0))
(defun pacman-move-right ()
  (interactive) (pacman-move-player 1 0))
(defun pacman-move-up ()
  (interactive) (pacman-move-player 0 -1))
(defun pacman-move-down ()
  (interactive) (pacman-move-player 0 1))

(defun pacman-move-ghosts ()
  "Move each ghost randomly." 
  (setq pacman-ghosts
        (mapcar (lambda (pos)
                  (let ((gx (car pos))
                        (gy (cdr pos))
                        (dirs '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                        moved)
                    (while dirs
                      (let* ((d (nth (random (length dirs)) dirs))
                             (nx (+ gx (car d)))
                             (ny (+ gy (cdr d))))
                        (setq dirs (delete d dirs))
                        (when (pacman-valid-move-p nx ny)
                          (setq gx nx gy ny dirs nil moved t))))
                    (cons gx gy)))
                pacman-ghosts)))

(defun pacman-check-game-over ()
  "Check if player collided with a ghost or won." 
  (if (cl-find-if (lambda (pos)
                    (and (= (car pos) pacman-player-x)
                         (= (cdr pos) pacman-player-y)))
                  pacman-ghosts)
      (progn
        (message "Game Over! Final score: %d" pacman-score)
        (kill-buffer (current-buffer)))
    (when (<= pacman-pellets 0)
      (message "You win! Final score: %d" pacman-score)
      (kill-buffer (current-buffer)))))


;;;###autoload
(defun pacman-start ()
  "Start playing Pac-Man." 
  (interactive)
  (switch-to-buffer "*Pac-Man*")
  (setq buffer-read-only t)
  (pacman-init)
  (use-local-map pacman-mode-map)
  (pacman-mode)
  (pacman-draw))

(define-derived-mode pacman-mode special-mode "Pac-Man"
  "Major mode for playing Pac-Man.")

(provide 'pacman)

;;; pacman.el ends here

