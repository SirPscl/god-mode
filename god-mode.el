;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns
;; Copyright (C) 2013 Fabián Ezequiel Gallina

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.15.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README.md.

;;; Code:

(require 'cl-lib)

(defcustom god-mod-alist
  '((nil . "C-")
    ("g" . "M-")
    ("G" . "C-M-"))
  "List of keys and their associated modifer."
  :group 'god
  :type '(alist))

(defcustom god-literal-key
  "SPC"
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'god-mode-self-insert)
        (setq i (1+ i))))
    (define-key map (kbd "DEL") nil)
    (define-key map (kbd "C-g") 'god-local-mode)
    map))

(evil-make-intercept-map god-local-mode-map)

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (progn
        (evil-normalize-keymaps)
        (run-hooks 'god-mode-enabled-hook))
    (run-hooks 'god-mode-disabled-hook)))

(defvar god-literal-sequence nil
  "Activated after space is pressed in a command sequence.")

;;;###autoload
(defun god-mode ()
  "Toggle global God mode."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

(defun god-mode-maybe-universal-argument-more ()
  "If god mode is enabled, call `universal-argument-more'."
  (interactive)
  (if god-local-mode
      (call-interactively #'universal-argument-more)
    (let ((binding (god-mode-lookup-command "u")))
      (if (commandp binding t)
          (call-interactively binding)
        (execute-kbd-macro binding)))))

(define-key universal-argument-map (kbd "u")
  #'god-mode-maybe-universal-argument-more)

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (god-mode-lookup-key-sequence initial-key)))
    (when (god-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq god-literal-sequence nil)
    (call-interactively 'god-local-mode)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun god-mode-upper-p (char)
  "Is the given char upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given `key' (or the next keypress,
if `key' is nil). This function sometimes
recurses. `key-string-so-far' should be nil for the first call in
the sequence."
  (interactive)
  (let ((sanitized-key
         (god-mode-sanitized-key-string
          (or key (read-event key-string-so-far)))))
    (god-mode-lookup-command
     (key-string-after-consuming-key sanitized-key key-string-so-far))))

(defun god-mode-sanitized-key-string (key)
  "Convert any special events to textual."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (S-left "S-<left>")
    (S-right "S-<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (t (char-to-string key))))

(defun key-string-after-consuming-key (key key-string-so-far)
  "Interpret god-mode special keys for key (consumes more keys if
appropriate). Append to keysequence."
  (let ((key-consumed t) (next-modifier "") next-key)
    (message key-string-so-far)
    (cond
     ;; Don't check for god-literal-key with the first key
     ((and key-string-so-far (string= key god-literal-key))
      (setq god-literal-sequence t))
     (god-literal-sequence
      (setq key-consumed nil))
     ((and (stringp key) (assoc key god-mod-alist))
      (setq next-modifier (cdr (assoc key god-mod-alist))))
     (t
      (setq key-consumed nil
            next-modifier (cdr (assoc nil god-mod-alist)))))
    (setq next-key
          (if key-consumed
              (god-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (when (and (= (length next-key) 1)
               (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
               ;; If C- is part of the modifier, S- needs to be given
               ;; in order to distinguish the uppercase from the
               ;; lowercase bindings. If C- is not in the modifier,
               ;; then emacs natively treats uppercase differently
               ;; from lowercase, and the S- modifier should not be
               ;; given
               (string-prefix-p "C-" next-modifier))
      (setq next-modifier (concat next-modifier "S-")))
    (if key-string-so-far
        (concat key-string-so-far " " next-modifier next-key)
      (concat next-modifier next-key))))

(defun god-mode-lookup-command (key-string)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           binding)
          ((keymapp binding)
           (god-mode-lookup-key-sequence nil key-string))
          (:else
           (error "God: Unknown key binding for `%s`" key-string)))))

(provide 'god-mode)

;;; god-mode.el ends here
