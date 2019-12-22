# Evil God Mode

This is a fork of the (archived) god-mode inteded to work with evil-mode.

Differences:
 - Fix Keybindings for Evil Mode
 - Remove unneeded functionality (pause/resume, global-mode, activaters)

Things to do or analyse:
 - Fix `which-key` commands (e.g. paging)
 - Simplify interface - make `evil-god-execute` the sole function to call.
 - Handle command repeat (with evil-repeat?)
 - Universal argument (is it needed?)
 - Rename project and definitions to `evil-god-...` without losing `which-key`
   support (possible?)

## Comparisson to other packages

### Evil God State

Evil God State aims to do the same thing as Evil God Mode. I used it for quite
some time and am forever grateful. The difference lies in its implementation.
Adding a new evil state for god-mode introduces new problems like handling calls
from evil-visual-state (which does not work well with evil-god-state). In
addition, some functionality of (the archived/abandoned) god-mode are not used
and don't make sense for Evil God State. For those reasons I decided to fork
god-mode and make Evil God Mode.

### Evil Leader

Evil Leader allows to bind functions under a variable prefix key. Evil God Mode
allows to conveniently access all functions (with a keybinding). For example you
could define `flycheck-list-errors (C-c ! l)` to be called via `<leader> e` with
Evil-Leader. In Evil God Mode you can call it with `<leader>xf` (without having to.

## Usage

This library defines the following mapping:

* All commands are assumed to be `C-<something>` unless otherwise
   indicated. Examples:

   * `a`    → `C-a`
   * `s`    → `C-s`
   * `akny` → `C-a C-k C-n C-y`
   * `xs`   → `C-x C-s`
   * `x s`  → `C-x s`

   Note the use of space to produce `C-x s`.

* `g` is a special key to indicate `M-<something>`. This means that
   there is no way to write `C-g` in this mode, you must therefore
   type `C-g` directly. Examples:

   * `gf` → `M-f`
   * `gx` → `M-x`

* `G` is a special key to indicate `C-M-<something>`. Example:

   * `Gx` → `C-M-x`

* Digit arguments:

  * `12f` → `C-u 12 C-f`

## Cursor style to indicate mode

You can change the cursor style indicate whether you're in God mode or
not.

``` lisp
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
```

## Change modeline color

You can use the following function to switch the entire modeline's foreground and background:

``` lisp
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
```

## Overwrite mode

You can pause `god-mode` when `overwrite-mode` is enabled and resume
when `overwrite-mode` is disabled.

``` lisp
(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
```

## isearch integration

There is a small module for providing god-mode-like behaviour for
isearch: You can hit <escape> while in isearch, for example:

    s hello <escape> s s s RET

For

    C-s hello C-s C-s C-s RET

Activate and configure with the following:

``` lisp
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
```

Configure `god-mode-isearch-map` for additional keybindings.

## Using with org-mode fast keys

If you want to use god-mode with fast-keys, you can use a rebinding of
self-insert like this:

``` lisp
(define-key god-local-mode-map [remap self-insert-command] 'my-god-mode-self-insert)

(defun my-god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))
```

## Nice keybindings

The following customizations are popular:

``` lisp
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
```

Although I personally prefer:

``` lisp
(define-key god-local-mode-map (kbd ".") 'repeat)
```

Feel free to alter and customize as you prefer.

Also handy are these:

``` lisp
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
```

So that you can run `x1`/`x2`/`x3`/`x0` in god-mode.

## Global god-mode and exempt major modes

**Note:** This is less necessary in recent god-mode, as god-mode
  overrides all printable single byte keys, so it will override
  dired-mode or magit-mode.

If you do `M-x god-mode`, then all buffers will be started in God
mode. If you don't like that behavior, just use the `god-local-mode`
toggler with a keybinding.

Sometimes `god-mode` is enabled in buffers where it makes no sense. In
that case you can add the major mode to `god-exempt-major-modes`:

``` lisp
(add-to-list 'god-exempt-major-modes 'dired-mode)
```

Since `dired-mode` is already in the list, that's a noop, but you get
the idea. Consider opening an issue or pull request if you find a
major mode that should be on the official list.

Another option to control god-mode's global behavior is to provide a
function with no arguments that must return non-nil if god-mode should
be disabled for the current buffer. See the `god-exempt-predicates`
variable and its default members `god-exempt-mode-p`,
`god-comint-mode-p`, `god-view-mode-p` and `god-special-mode-p` for
further details.
