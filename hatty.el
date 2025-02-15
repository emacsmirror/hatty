;;; hatty.el --- Query positions through hats        -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience
;; Version: 0.3.2
;; Homepage: https://github.com/ErikPrantare/hatty.el
;; Package-Requires: ((emacs "26.1"))
;; Created: 05 Jul 2024

;; hatty.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; hatty.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality for drawing and locating
;; "hats", small colored symbols used for indexing the text in the
;; visible portion of the buffer in the current window.

;; Setup: Call ‘global-hatty-mode’ to start displaying hats.  The
;; position of a hat may then be queried using the function
;; ‘hatty-locate’.

;;; Code:

(require 'subr-x)
(require 'svg)  ; Hat rendering
(require 'term) ; Default colors

(defgroup hatty nil
  "Index buffer locations through character hats."
  :group 'convenience
  :prefix "hatty-"
  :link '(emacs-commentary-link :tag "Commentary" "hatty.el"))

(defcustom hatty-colors
  (let ((default-colors
         `((default . ,(face-attribute 'term :foreground nil t))
           (yellow . ,(face-attribute 'term-color-yellow :foreground nil t))
           (red . ,(face-attribute 'term-color-red :foreground nil t))
           (blue . ,(face-attribute 'term-color-blue :foreground nil t))
           (pink . ,(face-attribute 'term-color-magenta :foreground nil t))
           (green . ,(face-attribute 'term-color-green :foreground nil t)))))

    (if (fboundp 'modus-themes-color) ; Guard to make byte compiler happy
        (cond
         ((memq 'modus-vivendi custom-enabled-themes)
          `((default . ,(modus-themes-color 'fg-dim))
            (yellow . ,(modus-themes-color 'yellow-graph-0-bg))
            (red . ,(modus-themes-color 'red-intense))
            (blue .  ,(modus-themes-color 'blue-graph-0-bg))
            (pink . ,(modus-themes-color 'magenta-graph-0-bg))
            (green . ,(modus-themes-color 'green))))

         ;; No yellow here, as it is not very visible.
         ((memq 'modus-operandi custom-enabled-themes)
          `((default . ,(modus-themes-color 'fg-main))
            (red . ,(modus-themes-color 'red-intense))
            (blue .  ,(modus-themes-color 'blue-intense-bg))
            (pink . ,(modus-themes-color 'magenta-graph-0-bg))
            (green . ,(modus-themes-color 'green-graph-0-bg))))

         (t default-colors))
      default-colors))
  "Alist of colors used in rendering hats, indexed by identifier.

Identifiers must to be symbols.

The identifier symbol `default' indicates the default color."
  :type '(alist :key-type symbol :value-type color)
  :group 'hatty)

;; TODO: Use proper svgs here?

;; NOTE: "cross" does not render with the original opening in the
;; middle.  However, I quite like it filled in, so I might try to keep
;; it that way if I implement the above TODO.
(defcustom hatty-shapes
  '((default  . "M6 9C9.31371 9 12 6.98528 12 4.5C12 2.01472 9.31371 0 6 0C2.68629 0 0 2.01472 0 4.5C0 6.98528 2.68629 9 6 9Z")
    (bolt  . "M 12,4 V 0 C 12,0 9,4 8,4 7,4 3,0 3,0 L 0,5 V 9 C 0,9 3,5 4,5 5,5 9,9 9,9 Z")
    (curve  . "M6.00016 3.5C10 3.5 12 7.07378 12 9C12 4 10.5 0 6.00016 0C1.50032 0 0 4 0 9C0 7.07378 2.00032 3.5 6.00016 3.5Z")
    (fox . "M6.00001 9L0 0C0 0 3.71818 2.5 6 2.5C8.28182 2.5 12 0 12 0L6.00001 9Z")
    (frame . "M0 0.000115976V8.99988H12V0L0 0.000115976ZM9.5 6.5H6H2.5V4.5V2.5H6H9.5V4.5V6.5Z")
    (play . "M12 4.49999L0 9C0 9 3 6.2746 3 4.49999C3 2.72537 0 0 0 0L12 4.49999Z")
    (wing . "M6 0C6 0 7 3 8.5 4.5C10 6 12 7 12 7V9C12 9 8.5 7 6 7C3.5 7 0 9 0 9V7C0 7 2 6 3.5 4.5C5 3 6 0 6 0Z")
    (hole . "M1.5 4.5L0 7H2.5L3.5 9L6 7.5L8.5 9L9.5 7H12L10.5 4.5L12 2H9.5L8.5 0L6 1.5L3.5 0L2.5 2H0Z M6 5.5L4 6.5L3 4.5L4 2.5L6 3.5L8 2.5L9 4.5L8 6.5L6 5.5Z")
    (ex . "M1.5 4.5L0 7H2.5L3.5 9L6 7.5L8.5 9L9.5 7H12L10.5 4.5L12 2H9.5L8.5 0L6 1.5L3.5 0L2.5 2H0Z M6 5.5L4 6.5L3 4.5L4 2.5L6 3.5L8 2.5L9 4.5L8 6.5L6 5.5Z")
    (cross ."M5.25 0C5.25 0 4.5 1.5 3.5 2.5C2.49483 3.50517 0 3.75 0 3.75V5.25C0 5.25 2.49483 5.49483 3.5 6.5C4.5 7.5 5.25 9 5.25 9H6.75C6.75 9 7.5 7.5 8.5 6.5C9.50517 5.49483 12 5.25 12 5.25V3.75C12 3.75 9.50517 3.50517 8.5 2.5C7.5 1.5 6.75 0 6.75 0H5.25ZM5.75 6.5H6.25C6.25 6.5 6.58435 5.25599 7 5C7.41565 4.74401 8.75 4.75 8.75 4.75V4.25C8.75 4.25 7.41565 4.25599 7 4C6.58435 3.74401 6.25 2.5 6.25 2.5H5.75C5.75 2.5 5.41565 3.74401 5 4C4.58435 4.25599 3.25 4.25 3.25 4.25V4.75C3.25 4.75 4.58435 4.74401 5 5C5.41565 5.25599 5.75 6.5 5.75 6.5Z" )
    (eye . "M12 4L6.5 0H5.5L0 4V5L5.5 9H6.5L12 5V4ZM6 7.5C6 7.5 4.5 6.5 4.5 4.5C4.5 2.5 6.01103 1.5 6 1.5C6 1.5 7.5 2.5 7.5 4.5C7.5 6.5 6 7.5 6 7.5Z"))
  "Alist of shapes used in rendering hats, indexed by identifier.

Identifiers must be symbols.  The shapes must specify valid svg
paths.

The identifier symbol `default' indicates the default ."
  :type '(alist :key-type symbol :value-type string)
  :group 'hatty
;; The following notice is included for compliance with the license of
;; the cursorless project, from which the above hat designs were
;; copied.  The "bolt" path has been modified to be symmetric.
;;
;; MIT License
;;
;; Copyright (c) 2021 Brandon Virgil Rule
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
)

(defvar hatty--hat-styles nil
  "List of hat styles to choose from.

This is recalculated at the beginning of ‘hatty-reallocate’ to
create all combinations from ‘hatty-colors’ and ‘hatty-shapes’")

;; TODO: Define the structure with EIEIO instead?  For constructors.
(cl-defstruct hatty--hat
  "A hat with COLOR and SHAPE at MARKER over CHARACTER."
  marker
  character
  color
  shape
  token-region
  (on-final-visual-line t))

(defvar-local hatty--hats '()
  "All hats located in the current buffer.")

(defun hatty--normalize-character (character)
  "Return normalized version of CHARACTER.
This function is equivalent to ‘downcase’."
  (downcase character))

(defun hatty--locate-hat (character &optional color shape)
  "Get  the hat over CHARACTER matching COLOR and SHAPE."
  (setq color (or color 'default))
  (setq shape (or shape 'default))
  (seq-find (lambda (hat) (and (eq color (hatty--hat-color hat))
                               (eq (hatty--normalize-character character)
                                   (hatty--hat-character hat))
                               (eq shape (hatty--hat-shape hat))))
            hatty--hats))

(defun hatty-locate (character &optional color shape)
  "Get position of the hat over CHARACTER matching COLOR and SHAPE.

COLOR and SHAPE should be identifiers as they occur in
‘hatty-colors’ and ‘hatty-shapes’.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used."
  (hatty--hat-marker
   (hatty--locate-hat character color shape)))

(defun hatty-locate-token-region (character &optional color shape)
  "Get token region of the hat over CHARACTER with COLOR and SHAPE.

COLOR and SHAPE should be identifiers as they occur in
‘hatty-colors’ and ‘hatty-shapes’.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used."
  (hatty--hat-token-region
   (hatty--locate-hat character color shape)))

(cl-defun hatty--make-hat (position token-region &key color shape)
  "Create a hat at POSITION with color COLOR and shape SHAPE.

TOKEN-REGION denotes the region of the token that the hat
indicates.

If COLOR or SHAPE is nil or unspecified, the default color or
shape will be used."
  (unless color (setq color 'default))
  (unless shape (setq shape 'default))
  (make-hatty--hat
   :character (hatty--normalize-character (char-after position))
   :color color
   :marker (set-marker (make-marker) position (window-buffer))
   :shape shape
   :token-region (cons
                  (set-marker (make-marker) (car token-region))
                  (set-marker (make-marker) (cdr token-region)))))

(defvar hatty--next-styles '()
  "Alist mapping characters to index of next free style.")

(defun hatty--next-style-index (character)
  "Get index of the next style applicable to CHARACTER."
  (let* ((normalized (hatty--normalize-character character))
         (entry (assq normalized hatty--next-styles)))
    (if entry
        (cdr entry)
      (add-to-list 'hatty--next-styles (cons normalized 0))
      0)))

(defun hatty--request-style (character)
  "Get the next applicable style of CHARACTER.
Return nil if none is applicable."
  (let* ((normalized (hatty--normalize-character character))
        (entry (assq normalized hatty--next-styles)))
    (unless entry
      (add-to-list 'hatty--next-styles (cons normalized 0))
      (setq entry (car hatty--next-styles)))
    (let ((index (cdr entry)))
      (if (>= index (length hatty--hat-styles))
          nil
        (setf (cdr entry) (1+ index))
        (elt hatty--hat-styles index)))))

(defun hatty--reset-styles ()
  "Clear ‘hatty--next-styles’.
Done before hat reallocation is made."
  (setq hatty--next-styles '()))

(defun hatty--select-hat-character (characters)
  "Return the character with highest style priority of CHARACTERS."
  (car (seq-sort-by #'hatty--next-style-index #'< characters)))

(defun hatty--create-hat (token)
  "Create a hat for TOKEN.
Return the hat if successful, otherwise return nil.

TOKEN is a cons cell of the bounds of the token."
  (if-let* ((characters
             (thread-last
               (buffer-substring (car token) (cdr token))
               string-to-list
               seq-uniq
               ;; Remove control characters.  In ASCII, they are before #x20.
               (seq-filter (lambda (c) (>= c #x20)))))
            (selected-character
             (hatty--select-hat-character characters))
            (requested-style
             (hatty--request-style selected-character))
            (position
             (cl-loop
              with position = (car token)
              until (eq (char-after position) selected-character)
              do (setq position (1+ position))
              finally return position)))
      (hatty--make-hat position token
                       :color (car requested-style)
                       :shape (cdr requested-style))))

(defun hatty--get-tokens ()
  "Return bounds of tokens in the visible buffer.
Order tokens by importance."
  (let* ((previous-point (point))
         token
         (tokens '())
         (next-token
          (lambda ()
            (skip-chars-forward "[:space:]\n")
            (let ((start (point)))
              (if (zerop (skip-syntax-forward "w_'"))
                  (skip-syntax-forward "^w_' " (1+ (point))))
              (setq token
                    (if (= start (point))
                        nil
                      (cons start (point))))))))
    (save-excursion
      (goto-char (window-start))
      (while (and (<= (point) (window-end))
                  (funcall next-token))
        (push token tokens))

      ;; TODO: Move to hat assignment algorithm?
      (setq tokens
            (seq-filter
             (lambda (token) (not (or (invisible-p (car token))
                                      (invisible-p (1- (cdr token))))))
             tokens))

      (seq-sort-by (lambda (token)
                       (abs (- previous-point (car token))))
                     #'<
                     tokens))))

(defun hatty--create-hats ()
  "Create hats in the buffer given by `window-buffer'.
Set `hatty--hats' to the created hats and return them.

Tokens are queried from `hatty--get-tokens'"
  (hatty--reset-styles)
  (setq hatty--hats
        (with-current-buffer (window-buffer)
          (let ((tokens (hatty--get-tokens)))
            (seq-filter
             #'identity
             (seq-map #'hatty--create-hat tokens)))))

  ;; To determine whether a hat is on the last visual line of the
  ;; logical line, we want to do it in one pass for all hats to
  ;; minimize the amount of visual line movements performed.  This is
  ;; because visual line movements functions are slow, but needed to
  ;; determine whether we are on the last visual line.  We need to
  ;; determine this to correctly determine the line height of the
  ;; visual line later (in hatty--draw-svg-hat).

  ;; First, we sort the hats in order of occurrence.
  (setq hatty--hats
        (seq-sort (lambda (h1 h2) (< (hatty--hat-marker h1) (hatty--hat-marker h2)))
                  hatty--hats))

  ;; Truncated lines are not handled as of now.
  (unless truncate-lines
    (save-excursion
      (goto-char (window-start))
      (end-of-line)
      (let ((worklist hatty--hats))
        (while worklist
          (let (last-visual-begin
                last-visual-end)
            (end-of-line)
            (setq last-visual-end (point))
            (beginning-of-visual-line)
            (setq last-visual-begin (point))
            ;; Mark all hats that occur before the last visual line...
            (while (and worklist
                        (< (hatty--hat-marker (car worklist)) last-visual-begin))
              (setf (hatty--hat-on-final-visual-line (car worklist)) nil)
              (pop worklist))
            ;; ... and all hats within it.
            (while (and worklist
                        (< (hatty--hat-marker (car worklist)) last-visual-end))
              (setf (hatty--hat-on-final-visual-line (car worklist)) t)
              (pop worklist)))
          (vertical-motion 1)))))

  hatty--hats)

(defun hatty--get-raise-display-property (position)
  "Get value of the `raise' display property at POSITION.

This function looks at both text properties and overlay
properties."
  (let ((display-property (get-char-property position 'display)))
    (cl-flet ((raise-property-p (property) (eq (car-safe property) 'raise)))
      (pcase display-property
        ((pred raise-property-p) (cadr display-property))
        ((pred seqp)
         (let ((property (seq-find #'raise-property-p display-property)))
           (if property (cadr property) 0.0)))
        (_ 0.0)))))


(defun hatty--draw-svg-hat (hat)
  "Overlay character of HAT with with image of it having the hat."

  (let* ((position (marker-position (hatty--hat-marker hat)))
         (text (buffer-substring position (1+ position)))
         ;; I will pretend that get-char-property yields all the faces
         ;; used in the deduction of the face properties for display.
         ;; I will also pretend that anything not a face or list of
         ;; faces does not contribute to the display.  These
         ;; assumptions might not be true; Consult Properties with
         ;; Special Meanings in the emacs manual.
         (faces (append (let ((face-spec (get-char-property position 'face)))
                          (cond
                           ((facep face-spec) (list face-spec))
                           ((consp face-spec)
                            ;; Only handle named faces for now
                            (seq-filter #'facep face-spec))
                           (t '())))
                        (list 'default)))
         (family (face-attribute (car faces) :family nil (cdr faces)))
         (weight (face-attribute (car faces) :weight nil (cdr faces)))

         (font (font-at position))
         (font-metrics (query-font font))
         (glyph-metrics (elt (font-get-glyphs font position (1+ position)) 0))

         (font-size (elt font-metrics 2))
         (ascent (elt font-metrics 4))
         (descent (elt font-metrics 5))
         (char-width (elt glyph-metrics 4))
         (char-height (+ ascent descent))
         (raise (round
                 (* char-height
                    (hatty--get-raise-display-property position))))

         ;; Should probably look at the final newline for this property
         (line-height (get-char-property position 'line-height))
         (default-char-height (frame-char-height))
         (default-line-height
          (cond
           ;; Lines that are wrapped do not profit of the additional
           ;; line height of the final newline
           ((not (hatty--hat-on-final-visual-line hat)) default-char-height)
           ((integerp line-height) (max default-char-height line-height))
           ((floatp line-height) (* default-char-height line-height))
           (t default-char-height)))

         (svg-height (max default-line-height char-height))
         (svg-width char-width)
         (svg (svg-create svg-width svg-height))

         ;; Convert from emacs color to 6 letter svg hexcode.
         (svg-hat-color
          (let ((color
                 (color-values
                  (alist-get (hatty--hat-color hat) hatty-colors))))
            (format "#%02X%02X%02X"
                    (/ (nth 0 color) 256)
                    (/ (nth 1 color) 256)
                    (/ (nth 2 color) 256))))

         (overlay (make-overlay position (1+ position) nil t nil)))

    (svg-text svg text
              :stroke-width 0
              :font-family family
              :font-size font-size
              :font-weight weight
              :x 0
              :y (- svg-height descent))

    (svg-node svg 'path
              ;; Transformations are applied in reverse order
              :transform (format "translate(%s,0) scale(%s) translate(%s,0)"
                                 (/ svg-width 2)
                                 0.6
                                 (- 6))
              :fill svg-hat-color
              :d (alist-get (hatty--hat-shape hat) hatty-shapes))

    (with-silent-modifications
      (overlay-put overlay
                   'display
                   (svg-image svg
                              :ascent
                              (ceiling (* 100 (- svg-height descent (- raise)))
                                       svg-height)
                              :scale 1.0)))
    (overlay-put overlay 'hatty t)
    (overlay-put overlay 'hatty-hat t)))

(defun hatty--increase-line-height ()
  "Create more space for hats to render in current buffer."
  (remove-overlays (point-min) (point-max) 'hatty-modified-line-height t)
  (let ((modify-line-height (make-overlay (point-min) (point-max) nil nil t)))
    (overlay-put modify-line-height 'line-height  1.2)
    (overlay-put modify-line-height 'evaporate nil)
    (overlay-put modify-line-height 'hatty t)
    (overlay-put modify-line-height 'hatty-modified-line-height t)))

(defun hatty--render-hats (hats)
  "Display HATS."
  (dolist (hat hats)
    (let ((display-property
           (get-char-property (hatty--hat-marker hat) 'display)))
      ;; Do not render if there already is a string or image display
      ;; property
      (unless (or (stringp display-property)
                  (and (consp display-property)
                       (or (eq (car display-property) 'image)
                           (assq 'image display-property))))
        (hatty--draw-svg-hat hat)))))

;; Declare now, will be set later along the minor mode.
(defvar hatty-mode)

(defun hatty-reallocate ()
  "Reallocate hats."
  (interactive)
  (with-current-buffer (window-buffer)
    (when hatty-mode
      (setq hatty--hat-styles
            (cl-loop
             for shape in (seq-uniq (mapcar #'car hatty-shapes))
             append (cl-loop
                     for color in (seq-uniq (mapcar #'car hatty-colors))
                     collect (cons color shape))))
      (hatty--clear)
      (hatty--increase-line-height)
      (hatty--render-hats (hatty--create-hats)))))

(defun hatty--clear ()
  "Clean up all resources of hatty.

This should restore the buffer state as it was before hatty was enabled."
  (remove-overlays nil nil 'hatty t)
  (dolist (hat hatty--hats)
    (set-marker (hatty--hat-marker hat) nil))
  (setq hatty--hats nil))

(defvar hatty--hat-reallocate-timer (timer-create))
;; Not sure what the best way to disable this whenever hatty-mode is
;; disabled in all buffers.  Currently I just let it always run, as
;; hatty-reallocate bails if it is not enabled anyway.
(defvar hatty--hat-reallocate-idle-timer
  (run-with-idle-timer 0.2 t #'hatty-reallocate))

(defun hatty-request-reallocation ()
  "Signal that the current buffer will need hat reallocation.

The function will try to avoid rapid consecutive reallocations by
deferring reallocation by a small amount of time and counselling
any previously unperformed reallocations.

To reallocate immediately, use `hatty-reallocate' instead."
  (cancel-timer hatty--hat-reallocate-timer)
  (setq hatty--hat-reallocate-timer
        (run-with-timer 0.1 nil #'hatty-reallocate)))

;;;###autoload
(define-minor-mode hatty-mode
  "Minor mode for querying buffer positions through hats."
  :init-value nil
  :lighter nil
  :group 'hatty
  :after-hook
  (if hatty-mode
      (hatty--increase-line-height)
    (hatty--clear)))

;;;###autoload
(define-globalized-minor-mode global-hatty-mode hatty-mode hatty-mode
  :group 'hatty)

(provide 'hatty)
;;; hatty.el ends here
