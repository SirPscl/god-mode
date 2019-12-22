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


; TODO: check if we need cl-lib?
(require 'cl-lib)

(defcustom god-literal-key
  "SPC"
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-meta-key
  "g"
  "The key used for meta interpretation (M-)."
  :group 'god
  :type 'string)

(defcustom god-control-meta-key
  "G"
  "The key used for control-meta interpretation (C-M-)."
  :group 'god
  :type 'string)

(defun god-modifier-key-p (key)
  (or (string= key god-literal-key)
      (string= key god-meta-key)
      (string= key god-control-meta-key)))

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

(add-hook 'god-local-mode-hook #'(lambda () (message "Goddess Mode")))

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

;; (defun god-execute ()
;;   "Start god-mode without mode and keymap"
;;   (interactive)
;;   (setq binding (god-mode-lookup-command))
;;   (if (commandp binding t)
;;       (progn
;;         (unless (eq binding 'digit-awhenrgument)
;;           (call-interactively 'god-local-mode))
;;         (call-interactively binding))
;;     (execute-kbd-macro binding)))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  ;; (message "self-insert")
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
    ;(setq god-literal-sequence nil)
    ;; (message "executing: %s" binding)
    (if (commandp binding t)
        (progn
          (unless (eq binding 'digit-awhenrgument)
            (call-interactively 'god-local-mode))
          (call-interactively binding))
      (execute-kbd-macro binding))))

(defun god-mode-upper-p (char)
  "Is the given char upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun god-get-modifier-string (key)
  "Returns the modifier string for event key."
  (cond
   ((string= key god-literal-key) "%s")
   ((string= key god-meta-key) "M-%s")
   ((string= key god-control-meta-key) "C-M-%s")))

(defun god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given `key' (or the next keypress,
if `key' is nil). This function sometimes
recurses. `key-string-so-far' should be nil for the first call in
the sequence."
  (interactive)
  ;; (message "------------------------")
  (let* ((event (or key (read-event key-string-so-far)))
         (sanitized-key (god-mode-sanitized-key-string event))
         next-binding)
    ;; (message ">> sanitized-key:%s event:%s" sanitized-key event)
    (cond

     ;; C-h -> show help
     ((string= sanitized-key (kbd "C-h"))
      ;; (message ">> which-key help (%s, %s)" sanitized-key key-string-so-far)
      (which-key-C-h-dispatch)
      (setq next-binding key-string-so-far)
      (setq key-string-so-far nil))

     ;; ditch C-? bindings (after which-key dispatched)
     ((<= event 26)
      (setq next-binding key-string-so-far)
      (setq key-string-so-far nil))

     ;; SPC, g, G -> read another key and format with the correct modifier
     ((god-modifier-key-p sanitized-key)
      (let ((second-event (read-event key-string-so-far)))
        ;; (message ">> special binding with second-event:%s" (char-to-string second-event))
        (setq next-binding
              (format (god-get-modifier-string sanitized-key)
                      (god-mode-sanitized-key-string second-event)))))

     ;; other -> C-<sanitzied-key> or C-S-<sanitized-key>
     (t
      ;; (message ">> default -> next-binding:%s sanitizied-key:%s" (format "C-%s" sanitized-key) sanitized-key)
      (setq next-binding
            ;; or this? (god-mode-get-default-modifier-string sanitized-key)
            (format "C-%s" sanitized-key))))

    ;; format new key-string and call next round of keys.
    (if key-string-so-far
        (progn
          ;; (message ">> combined key-string  %s %s" key-string-so-far next-binding)
          (god-mode-lookup-command (format "%s %s" key-string-so-far next-binding)))
      ;; (message ">> first binding: %s" nil next-binding)
      (god-mode-lookup-command next-binding))
    ))

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


    ;; (when (and (= (length next-key) 1)
    ;;            (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
    ;;            ;; If C- is part of the modifier, S- needs to be given
    ;;            ;; in order to distinguish the uppercase from the
    ;;            ;; lowercase bindings. If C- is not in the modifier,
    ;;            ;; then emacs natively treats uppercase differently
    ;;            ;; from lowercase, and the S- modifier should not be
    ;;            ;; given
    ;;            (string-prefix-p "C-" next-modifier))
    ;;   (setq next-modifier (concat next-modifier "S-")))

;; (defun god-mode-get-default-modifier-string (key)
;;   "Returns the modifier string for default event"
;;   (if (and (= (length key) 1)
;;            (string= (get-char-code-property (aref key 0) 'general-category) "Lu"))
;;       "C-S-%s" ; is that right?
;;     "C-%s"))


;; (defun key-string-after-consuming-key (key key-string-so-far)
;;   "Interpret god-mode special keys for key (consumes more keys if
;; appropriate). Append to keysequence."
;;   (let ((key-consumed t) (next-modifier "") next-key)
;;     (cond
;;      ;; Don't check for god-literal-key with the first key
;;      ((and key-string-so-far (string= key god-literal-key))
;;       ;; SPC -> next is key without god-mod-alist
;;       (message ">> case literal sequence")
;;       (setq god-literal-sequence t))
;;      (god-literal-sequence
;;       ;; not god-literate-alist -> no key consumed
;;       (message ">> case no literal sequence")
;;       (setq key-consumed nil))
;;      ((and (stringp key) (assoc key god-mod-alist))
;;       ;; with god-literate-alist -> get appropriate modifier
;;       (setq next-modifier (cdr (assoc key god-mod-alist)))
;;       (message ">> case next is modifier (%s)" next-modifier))
;;      (t
;;       ;; ???
;;       (message ">> case C- (key:%s)" key)
;;       (setq key-consumed nil
;;             next-modifier (cdr (assoc nil god-mod-alist)))))
;;     (setq next-key
;;           (if key-consumed
;;               (god-mode-sanitized-key-string (read-event key-string-so-far))
;;             key))
;;     (message ">> next-key:%s, next-modifier:%s" next-key next-modifier)
;;     (when (and (= (length next-key) 1)
;;                (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
;;                ;; If C- is part of the modifier, S- needs to be given
;;                ;; in order to distinguish the uppercase from the
;;                ;; lowercase bindings. If C- is not in the modifier,
;;                ;; then emacs natively treats uppercase differently
;;                ;; from lowercase, and the S- modifier should not be
;;                ;; given
;;                (string-prefix-p "C-" next-modifier))
;;       (setq next-modifier (concat next-modifier "S-")))
;;     (if key-string-so-far
;;         (concat key-string-so-far " " next-modifier next-key)
;;       (concat next-modifier next-key))))

;; (defvar god-literal-sequence nil
;;   "Activated after space is pressed in a command sequence.")

;; ;;;###autoload
;; (defun god-mode ()
;;   "Toggle global God mode."
;;   (interactive)
;;   (setq god-global-mode (not god-global-mode))
;;   (if god-global-mode
;;       (god-local-mode 1)
;;     (god-local-mode -1)))



;;; god-mode.el ends here
