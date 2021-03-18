(require 'package)
(setq package-quickstart t)

;; This can fix some troubles with native compilation
;; (setq load-no-native t)

;;; Before everything else
(defvar old--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda () (setq ;; gc-cons-threshold 16777216 ; 16mb
                             gc-cons-threshold 104857600 ; 100MB
                             gc-cons-percentage 0.1
                             file-name-handler-alist old--file-name-handler-alist)
              ;; (my/set-font)
              ))

;;; Package setup

; If we run package-initialize, then add-to-list melpa, the
; package-install invocation will fail. We need the package-archives
; list setup before calling package-initialize.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; (package-initialize)

; (require 'use-package)
(eval-when-compile
  (add-to-list 'load-path "~/src/use-package")
  (require 'use-package))
;; (require 'diminish)
(require 'bind-key)

;; Show a message whenever a package takes longer than 0.1s to load
;; (setq use-package-verbose t)
;; (setq use-package-compute-statistics t)

;;; benchmark-init
(use-package benchmark-init
  :disabled
  :config
  (require 'benchmark-init)
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook (lambda ()
                               (benchmark-init/deactivate)
                               (require 'benchmark-init-modes)))
  (benchmark-init/activate))
;;; Font setup
;; It would be nice to include this logic in early-init.el, but
;; `window-system' is not set up by the time that file is run during
;; emacs startup.
(defun my/set-font ()
  (if (and nil (memq window-system '(mac ns)))
      (set-frame-font "Monaco 14")
    (if (or (memq window-system '(mac ns))
            (file-exists-p "/etc/lsb-release"))
        (set-frame-font "Victor Mono-15:weight=demi")
      (set-frame-font "Victor Mono-11:weight=demi"))))
(my/set-font)

(defvar yanone-font-name (if (or t (memq window-system '(mac ns)))
                             "Yanone Kaffeesatz"
                           "Yanonne Kaffeesatz Light:style=Light,Regular"))

(use-package info
  :commands (info info-apropos)
  :config
  ;; (set-face-attribute 'info-title-1 nil :family "Yanone Kaffeesatz" :weight 'light :height 200 :foreground "#E1BEE7")
  ;; (set-face-attribute 'info-title-2 nil :family "Yanone Kaffeesatz" :weight 'light :height 175)
  ;; (set-face-attribute 'info-title-3 nil :family "Yanone Kaffeesatz" :weight 'light :height 160)
  ;; (set-face-attribute 'info-title-4 nil :family "Yanone Kaffeesatz" :weight 'light :height 150)
  ;; (set-face-attribute 'info-menu-header nil :family "Yanone Kaffeesatz" :weight 'light :height 175 :foreground "#E1BEE7")

  (set-face-attribute 'info-title-1 nil :font yanone-font-name :weight 'light :height 200 :foreground "#E1BEE7")
  (set-face-attribute 'info-title-2 nil :font yanone-font-name :weight 'light :height 175)
  (set-face-attribute 'info-title-3 nil :font yanone-font-name :weight 'light :height 160)
  (set-face-attribute 'info-title-4 nil :font yanone-font-name :weight 'light :height 150)
  (set-face-attribute 'info-menu-header nil :font yanone-font-name :weight 'light :height 175 :foreground "#E1BEE7"))

(use-package hl-line
  :commands (hl-line-mode)
  :custom-face
  (hl-line ((t (:background "gray20")))))

;;;; Support ligatures

;; See https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#user-content-using-composition-char-table
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; This has to be very early in initialization.
(defvar outline-minor-mode-prefix "\M-#")

;; (add-to-list 'load-path "/Users/acowley/.nix-profile/share/emacs/site-lisp")

;;; General emacs configuration

;;;; Elisp Helpers
(require 'subr-x)

(defun my/eval-last-sexp (raw-prefix)
  "A wrapper around `eval-last-sexp' that modifies the behavior when called with a prefix argument to insert the result of evaluating the sexp before point after inserting an arrow. The result is the original sexp is left in the buffer, followed by an arrow, followed by the result of evaluation. If no prefix is given, the result is shown in the minibuffer as with `eval-last-sexp'."
  (interactive "P")
  (if (null raw-prefix)
      (eval-last-sexp raw-prefix)
    (let ((val (eval (macroexpand-all
                      (eval-sexp-add-defvars (elisp--preceding-sexp)))
                     lexical-binding)))
      (insert (format " ⇒ %s" val)))))

(global-set-key (kbd "C-x C-e") 'my/eval-last-sexp)

(defun backward-skip-alpha (&optional pt)
  "Move point backward until the last contiguous alpha character

Used as part of yas-key-syntaxes to expand snippets immediately
preceded by a dollar sign character `$' as encountered when
entering LaTeX math mode."
  (re-search-backward (rx (not (any alpha))))
  (when (not (null (match-beginning 0)))
    (right-char)))

(defun split-third ()
  "Split the frame into two windows split vertically with the one
on the left taking up 2/3rds of the width."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (delete-window))

(defun go-fullscreen ()
  "If the current frame is not already full screen, disable the
menu bar, set the frame to full screen, and vertically split the
window into a 2:1 ratio."
  (interactive)
  (unless (eq (frame-parameter nil 'fullscreen) 'fullboth)
    (menu-bar-mode -1)
    (toggle-frame-fullscreen)
    (split-third)))

(defun increment-number-aux (offset)
  "Increment the number point is in or adjacent to. If a prefix
argument is given, its numeric value is added to the number
rather than the default of 1."
  (interactive "P")
  (let ((n (number-at-point)))
    (when n
      (replace-match (format "%d" (+ n (or offset 1)))))))

(defun increment-number (offset)
  "Increment the number point is in or adjacent to. If a prefix
argument is given, its numeric value is added to the number
rather than the default of 1. This is a wrapper for
`increment-number-aux' that is multiple-cursor aware.

If you are using helm, ensure that `helm-M-x' is in your
`mc/cmds-to-run-once' list (often set in ~/.mc-lists.el)."
  (interactive "P")
  (if (> (or (mc/num-cursors) 1) 1)
      (mc/execute-command-for-all-cursors #'increment-number-aux)
    (funcall #'increment-number-aux offset)))

(defun insert-after (x y xs)
  "`(insert-after x y list)` inserts `y` after `x` in `list`. If
`x` is not found, `list` is returned unchanged. This is a
non-destructive operation."
  (let ((rest xs)
        (result))
    (while rest
      (cond
       ((eq (car rest) x)
        (setq result (append (reverse (cons (car rest) result))
                             (cons y (cdr rest))))
        (setq rest nil))
       ((null (cdr rest))
        (setq rest nil)
        (setq result xs))
       (t (setq result (cons (car rest) result))
          (setq rest (cdr rest)))))
    result))

(defun fill-list (xs &optional separator prefix suffix)
  "Format a list to respect the fill column.

List elements are separated by SEPARATOR. The formatted list is
prefixed by PREFIX, and terminated by SUFFIX. If the list is
wrapped across multiple lines, lines after the first are indented
by a number of spaces equal to the length of PREFIX."
  (require 'cl-lib)
  (let ((sep (or separator ", "))
        (prefix-len (if prefix (length prefix) 0)))
    (with-temp-buffer
      (when prefix (insert prefix))
      (insert (string-join xs sep))
      (when suffix (insert suffix))
      (goto-char (point-min))
      (setq fill-prefix (cl-loop repeat prefix-len concat " "))
      (fill-paragraph)
      (buffer-string))))

(defun parse-time-span (s)
"Parse a time span string representing hours, minutes and seconds
of the form \"3h2m48.293s\" into a number of seconds."
  (let* ((hours (pcase (split-string s "h")
                  (`(,h ,rest) (cons (* 60 60 (string-to-number h)) rest))
                  (_ (cons 0 s))))
         (mins (pcase (split-string (cdr hours) "m")
                 (`(,m ,rest) (cons (* 60 (string-to-number m)) rest))
                 (_ `(0 . ,(cdr hours)))))
         (secs (pcase (split-string (cdr mins) "s")
                 (`(,s ,_) (string-to-number s))
                 (_ (error "No seconds component")))))
    (+ (car hours) (car mins) secs)))

;; Based on http://emacs.stackexchange.com/a/11067/6537
(defun my-transpose-sexps ()
  "If point is at or just after certain chars (comma, space, or
dash) transpose chunks around that. Otherwise transpose sexps."
  (interactive "*")
  (if (not (or (looking-at "[, -]*")
               (looking-back "[, -]*" (point-at-bol))))
      (progn (transpose-sexps 1) (forward-sexp -1))
    (while (looking-at "[, -]") (forward-char))
    (let ((beg (point)) end rhs lhs)
      (while (and (not (eobp))
                  (not (looking-at "\\s-*\\([,]\\|\\s)\\)")))
        (forward-sexp 1))
      (setq rhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (re-search-backward "[,]\\s-*" nil t)
      (setq beg (point))
      (while (and (not (bobp))
                  (not (looking-back "\\([,]\\|\\s(\\)\\s-*" (point-at-bol))))
        (forward-sexp -1))
      (setq lhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (insert rhs)
      (re-search-forward "[,]\\s-*" nil t)
      (save-excursion
        (insert lhs)))))

(defun insert-include-guard ()
  "Insert a C/C++-style ‘#ifndef‘ include guard in the current buffer."
  (interactive)
  (let* ((fname (buffer-file-name))
         (ext (upcase (file-name-extension fname)))
         (base (upcase (file-name-sans-extension (file-name-nondirectory fname))))
         (guard (concat "__" base "_" ext)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "#ifndef " guard "\n#define " guard "\n\n\n"))
      (goto-char (point-max))
      (insert "\n#endif"))
    (forward-line 4)))

(defun quote-shell-string (str)
    "Safely embed a string in single-quotes.

We can pass single-quoted strings to shell commands, but single
quotes within those strings need to be escaped. We use the
technique of ending the quoted string, concatenating a literal
single-quote character, and concatenating the remaining
single-quoted string."
    (concat "'" (replace-regexp-in-string "'" "'\\\\''" str) "'"))

(defun parenthesize-negatives ()
"Wrap negative numeric literals in parentheses.

Parentheses are required with negative numeric literals Haskell.
This helper makes it slightly easier to paste numbers into
Haskell programs as it may be applied to all numbers in region."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((regexp (rx "-" (+ (or digit ?.))))
              (start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (while (re-search-forward regexp end t)
            (message "Found match: %s" (match-string 0))
            (replace-match "(\\&)" nil nil)
            (set 'end (+ end 2)))))
    (message "Select a region first")))

;;;; Miscellaneous Settings

;; A short mode line that is going to be tweaked with moody
;; (setq-default mode-line-format
;;       '("%e"
;;         mode-line-modified
;;         mode-line-buffer-identification
;;         "   "
;;         mode-line-position
;;         (vc-mode vc-mode)
;;         "  "
;;         mode-line-modes
;;         mode-line-misc-info
;;         mode-line-end-spaces))

(setq confirm-kill-emacs #'y-or-n-p)

(setq warning-suppress-types '((comp)))

(put 'narrow-to-region 'disabled nil)

(setq inhibit-compacting-font-caches t)

;; (tool-bar-mode -1)

;; (when (and window-system (not (memq window-system '(mac ns))))
;;   (set-frame-size (selected-frame) 80 56))

;; Enable ligatures for fonts that provide them (e.g. hæck)
;; This may cause slowdown
;; (add-hook 'prog-mode-hook #'mac-auto-operator-composition-mode)
;; (add-hook 'prog-mode-hook (lambda () (auto-composition-mode -1)))
;; (add-hook 'text-mode-hook (lambda () (auto-composition-mode -1)))

(setq display-line-numbers-type 'relative)

;; Disable electric-quote-mode everywhere
(add-hook 'after-change-major-mode-hook
          (lambda () (electric-quote-mode -1)))
(electric-quote-mode -1)

;; Cause use-package to install packages automatically if not already
;; present
; (setq use-package-always-ensure t)

;; Clean trailing whitespace when saving a buffer.

;; This is too dangerous: it makes producing minimal diffs harder than
;; necessary, and can break things that expect a trailing whitespace
;; (e.g. with a regex). It may be fine for my own code, but judgment
;; should be applied before invoking it.
;; (setq before-save-hook #'whitespace-cleanup)

;; Keep ediff UI in a single frame
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Disable ligatures in ediff buffers
(add-hook 'ediff-mode-hook
            (lambda ()
              (setq auto-composition-mode nil)))

;; Use the exec-path-from-shell package to set the PATH
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns))
;;   :config
;;   (setq exec-path-from-shell-arguments (list "-l"))
;;   (exec-path-from-shell-initialize))

;; Move point to farthest possible position when scrolling the window
;; has reached the beginning or end of the buffer
(setq scroll-error-top-bottom t)

;; Support Cmd-up/down for top/bottom of buffer
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

(global-set-key (kbd "C-x C-k") #'kill-buffer)

;; Make bookmark jumping easier
(global-set-key (kbd "C-c b") #'bookmark-jump)

;; Don't undo undo operations by default
(global-set-key (kbd "C-/") #'undo-only)
(global-set-key (kbd "C-_") #'undo-redo)

;; Use Shift+ArrowKey to move the cursor between windows.
;; This means you lose shift select.
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Display PDFs inline
(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "pdf")
(setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
(add-to-list 'imagemagick-enabled-types 'PDF)
(add-hook 'emacs-startup-hook (lambda ()
                                (imagemagick-register-types)))

;; (use-package session
;;   :commands (session-initialize)
;;   :init
;;   ;; Preserve history between sessions
;;   (add-hook 'after-init-hook 'session-initialize)

;;   ;; Don't interfere with helm-show-kill-ring
;;   (setq session-save-print-spec '(t nil 40000)))

; yank will replace the active region's contents
(delete-selection-mode 1)

(setq c-default-style "bsd"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq default-directory "~/")

(setq mac-option-modifier 'meta)

;; Keyboard shortcut for aligning a region on a regexp
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Start the emacs server if possible
;; (when (fboundp 'server-mode) (funcall 'server-mode 1))

;; Revert buffers whose files have changed on disk
(global-auto-revert-mode t)

;; Disable the alarm bell on Quit (C-g)
(setq ring-bell-function 'ignore)

;; Turn off electric-indent-mode everywhere
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(put 'downcase-region 'disabled nil)

;; (setq TeX-command-extra-options "-shell-escape")

(put 'dired-find-alternate-file 'disabled nil)
(setq wdired-allow-to-change-permissions t)
(setq dired-listing-switches "-alh"
      dired-du-size-format t)

;; When `'which-function` output is too long, it can interfere with
;; modeline rendering
(defun truncate-function-name (s)
"Truncates a string to 20 characters. If the name has one or more
double colons (\"::\") in it, the part of the string after the
last double colon is truncated to 20 characters."
  (unless (null s)
    (if (> (length s) 20)
        (truncate-string-to-width
         (car (last (split-string s "::")))
         20 nil nil "...")
      s)))
(advice-add 'which-function :filter-return #'truncate-function-name)

;; From https://emacs.stackexchange.com/a/24658
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;(load-theme 'monokai t)
;(load-theme 'darktooth t)

(use-package recentf
  :init
  (setq
   ;; This is an attempt to prevent recentf (that keeps track of recent
   ;; files) from stat'ing remote files.
   recentf-keep '(file-remote-p file-readable-p)
   recentf-exclude
   `("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'"
     ,(regexp-quote "/.emacs.d/elpa/")
     ,(regexp-quote "/var/folders/")
     ,(regexp-quote "/.emacs.d/bookmarks")
     ,(regexp-quote "/.emacs.d/recentf")))
  (defun recent-buffer (b &rest _)
    (let ((file (buffer-file-name (get-buffer b))))
      (unless (null file) (recentf-add-file file))))
  (advice-add 'switch-to-buffer :after #'recent-buffer)
  :config
  (setq recentf-max-saved-items 100))

;; Another options is
;; (require 'recentf)
;; (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
;; (recentf-mode 1)

;; John Wiegley's ANSI colors hook for compiler output
(defun compilation-ansi-color-process-output ()
  (ansi-color-process-output nil)
  (set (make-local-variable 'comint-last-output-start)
       (point-marker)))

(add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)

(defun sort-words ()
  (interactive)
  (sort-regexp-fields nil "\\w+" "\\&" (region-beginning) (region-end)))

(defun browse-url-safari (uri &args)
"Open a URI in Safari using AppleScript. This preserves anchors."
  (let ((script (format "
tell application \"Safari\"
  open location \"%s\"
  activate
end tell" uri)))
    (do-applescript script)))

(if (memq window-system '(mac ns))
    (setq browse-url-browser-function #'browse-url-safari)
  (setq browse-url-browser-function #'browse-url-firefox))

;; From http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;(setq visual-line-fringe-indicators '(left-curly-arrow nil))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; From Joao Tavora http://stackoverflow.com/a/18034042/277078
;; Kill a process in the *Process List* buffer created by
;; `list-processes`.
(defun joaot/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(define-key process-menu-mode-map (kbd "C-k") #'joaot/delete-process-at-point)

(use-package visual-fill-column :defer t)

;; Put the visited file name in the frame title
;; (setq-default frame-title-format '("%f [%m]"))
(setq-default frame-title-format
              '((:eval (file-name-nondirectory (or (buffer-file-name) "")))
                " [%m]"))

;; Add Byte as a unit to `calc`
;; From /u/politza https://www.reddit.com/r/emacs/comments/31xezm/common_byte_units_in_calc/cq6ef06?utm_source=share&utm_medium=web2x
(setq math-additional-units '(
  (GiB "1024 * MiB" "Giga Byte")
  (MiB "1024 * KiB" "Mega Byte")
  (KiB "1024 * B" "Kilo Byte")
  (B nil "Byte")
  (Gib "1024 * Mib" "Giga Bit")
  (Mib "1024 * Kib" "Mega Bit")
  (Kib "1024 * b" "Kilo Bit")
  (b "B / 8" "Bit")
  (FLOP nil "FLOP")))

;; Reset calc's cache
(setq math-units-table nil)

;;;; variable-pitch-mode
(defun my/text-mode-hook ()
  (flyspell-mode)
  (turn-on-visual-line-mode)
  (variable-pitch-mode)
  (setq left-margin-width 2
        right-margin-width 2)
  ;; (setq buffer-face-mode-face '(:family "Helvetica Neue" :weight thin))
  ;; (setq buffer-face-mode-face '(:family "Avenir Next"))
  ;; (setq variable-pitch-face '(:family "Avenir Next"))
  ;; (setq buffer-face-mode-face '(:family "Montserrat"))
  ;; (buffer-face-mode)
  ;; (text-scale-adjust 1.5)
  ;; (text-mode-hook-identify)
  )
(add-hook 'text-mode-hook #'my/text-mode-hook)

;;;; Ignored extensions
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
;; (add-hook 'ido-setup-hook (setq ido-ignore-extensions t))
;; (add-hook 'ido-setup-hook (lambda ()
;;                            (add-to-list 'ido-ignore-files "\\.hi")
;;                            (add-to-list 'ido-ignore-files "\\.o")))

;;;; Spell checking

;; brew install hunspell

;; Download the OpenOffice dictionary for the language you want. The
;; `.oxt' file is a zip archive. Put the `.aff' and `.dic' files from
;; that archive in `~/Library/Spelling/'. Then create symlinks from,
;; for example, `en_US.aff' to `default.aff' in that directory, and
;; likewise for the `.dic' file. Try running `hunspell -D' to see what
;; dictionaries hunspell is using. The "personal dictionary" is just a
;; word list.
(when (memq window-system '(mac ns))
    (progn
      (setq ispell-program-name "hunspell")
      (setq ispell-local-dictionary "en_US")
      (setq ispell-personal-dictionary "~/.hunspell_en_US")))
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
;;          ("-d" "en_US"))
;;         nil utf-8))
;(setq ispell-extra-args '("-a" "-i" "utf-8"))

;; hunspell hacking to get ispell to actually use utf-8
;; See: http://stackoverflow.com/questions/3961119/working-setup-for-hunspell-in-emacs

(add-hook 'emacs-startup-hook
          (lambda ()
            (eval-after-load "ispell" '(defun ispell-get-coding-system () 'utf-8))
            (add-hook 'prog-mode-hook 'flyspell-prog-mode)
            (add-hook 'text-mode-hook 'flyspell-mode)))

;;;; Copy and comment
(defun copy-and-comment ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (comment-dwim nil)
  (goto-char (region-end))
  (end-of-line)
  (newline-and-indent))

;;;; Tramp with sudo

;; We hardly ever want to actually ssh into a host as root. Instead,
;; we want to ssh into the host using your account name, and then
;; switches to root on the host. This lets us use paths like
;; `/sudo:remotehost:/etc/hdparm.conf`. Remember that there has been a
;; history of projectile-global-mode interfering with such file
;; operations, so you may need to disable that temporarly.
;; See the manual: https://www.gnu.org/software/tramp/#Multi_002dhops
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))

;;;; cparens
(defun cparens ()
  "Parenthesize the C expression in region"
  (interactive)
  (if (region-active-p)
      (let* ((exp (buffer-substring (region-beginning) (region-end)))
             (tmp (make-temp-file "cparens-input")))
        (unwind-protect
            (progn
              (write-region (region-beginning) (region-end) tmp)
              (let ((res (with-temp-buffer
                           (cons
                            (call-process "cparens" tmp (current-buffer) nil)
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))
                (if (eq (car res) 0)
                    (progn
                      (delete-region (region-beginning) (region-end))
                      (insert (cdr res)))
                  (error (cdr res)))))
            (delete-file tmp)))))


;;;; Firefox (or Chromium) session save/load
;; From https://acidwords.com/posts/2019-12-04-handle-chromium-and-firefox-sessions-with-org-mode.html
;; with more discussion on reddit https://www.reddit.com/r/emacs/comments/e6fxrf/handle_chromium_firefox_sessions_with_orgmode/

(require 'cl-lib)
(defun save-firefox-session ()
  "Reads chromium current session and generates an org-mode heading with items."
  (interactive)
  (save-excursion
    (let* ((dir "~/.mozilla/firefox/")
           (dirs (directory-files dir))
           (unique-dir (cl-find-if (lambda (x)
                                     (and (string-match "\\.default" x)
                                          (file-accessible-directory-p (concat dir x))))
                                   dirs))
           (path (concat dir unique-dir "/sessionstore-backups/recovery.jsonlz4"))
           (cmd (concat "nix run nixpkgs.lz4json -c lz4jsoncat " path
                        " | nix run nixpkgs.jq -c jq '.windows[].tabs[] | .entries[-1] | .url'"
                        " | sed 's/\"//g' | sort | uniq"))
           (ret (shell-command-to-string cmd)))
      (insert
       (concat "* "
               (format-time-string "[%Y-%m-%d %H:%M:%S]")
               "\n"
               (mapconcat (lambda (x) (concat "  - " x))
                          (cl-remove-if (lambda (x) (or (null x)
                                                        (string-blank-p x)
                                                        (string= "null" x)))
                                        (split-string ret "\n"))
                          "\n"))))))

(defun restore-firefox-session ()
  "Restore web browser session by opening each link with `browse-url`.

Make sure to put cursor on date heading that contains a list of urls."

  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*")
      (forward-line 1)
      (while (looking-at "^[ ]+-[ ]+\\(http.?+\\)$")
        (let* ((ln (thing-at-point 'line t))
               (ln (replace-regexp-in-string "^[ ]+-[ ]+" "" ln))
               (ln (replace-regexp-in-string "\n" "" ln)))
          (browse-url ln))
        (forward-line 1)))))
;;; Diminish
(use-package diminish :disabled t)
;;; Themes
;; (use-package darkokai-theme :defer t)
;; (use-package monokai-theme :defer t)
(use-package apropospriate-theme
  :config
  (load-theme 'apropospriate-dark t)
  :custom-face
  (helm-match ((t (:foreground "gold"))))
  (company-tooltip-selection ((t (:background "SteelBlue"))))
  (company-tooltip-annotation-selection ((t (:background "SteelBlue"))))
  (company-tooltip-common-selection ((t (:foreground "black" :background "DeepSkyBlue"))))
  (company-tooltip-common ((t (:foreground "DeepSkyBlue"))))
  (lsp-ui-peek-selection ((t (:background "DeepSkyBlue"))))
  (lsp-ui-peek-highlight ((t (:foreground "gold")))))

;;; emacs server
(use-package server
  :config (and (fboundp 'server-mode)
               (add-hook 'emacs-startup-hook
                         (lambda ()
                           (or (server-running-p) (server-mode))))))
;;; company-mode
(use-package company
  ;; :defer nil
  :commands (company-mode)
  :hook (prog-mode . company-mode)
  ;; :init
  ;; (add-hook 'prog-mode-hook 'company-mode)
  :custom (company-minimum-prefix-length 2)
  :config
  ;; (setq company-idle-delay 0.1)
  (setq company-idle-delay 0.0)
  ;; (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-:") 'helm-company)

  (defun ac/company-text-mode ()
    ;; (add-to-list 'company-backends 'company-ispell)
    )
  (add-hook 'text-mode-hook #'ac/company-text-mode))
;;; prescient
(use-package prescient
  :custom (company-prescient-sort-length-enable nil)
  :commands (prescient-persist-mode)
  :hook (company-mode . prescient-persist-mode))
;;; company-prescient
(use-package company-prescient
  :commands (company-prescient-mode)
  :hook (company-mode . company-prescient-mode)
  :config
  (setq company-prescient-sort-length-enable nil))
;;; company-box
(use-package company-box
  :disabled
  :defer t
  :hook (company-mode . company-box-mode)
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons))
;;; Projectile
(use-package projectile
  :commands (projectile-mode projectile-switch-project projectile-find-file)
  :custom
  (projectile-project-root-files-functions
   '(projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring))
  (projectile-completion-system 'helm)
  :init
  (add-hook 'emacs-startup-hook #'projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-other-file-alist
        (append '(("hpp" "h" "ipp" "cpp" "cc" "cu") ("cu" "h" "hpp"))
                projectile-other-file-alist))
  (defun my/projectile-mode-line ()
    "Simplified projectile mode line that does not determine the
project's type."
    (format "%s[%s]" projectile-mode-line-prefix (projectile-project-name)))

  (setq projectile-enable-caching t
        projectile-global-mode t
        projectile-mode-line-function #'my/projectile-mode-line
        projectile-ignored-projects '("~/")
        projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".cabal-sandbox" ".cabbages" ".stack-work" "build")
        projectile-project-root-files
        '("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "compile_commands.json" "shell.nix" "*.cabal")

        ; This causes problems when a parent project has a submodule
        projectile-git-submodule-command nil))

;;; yasnippet
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-minor-mode-on)
  :hook (haskell-mode . yas/minor-mode)
  :bind (("C-<tab>" . yas-next-field))
  :config
  (add-to-list 'yas-key-syntaxes #'backward-skip-alpha)

  ;; Without this, `company-complete-selection' causes yasnippet
  ;; placeholders be inserted so that yas-next-field is no longer
  ;; available.
  (setq yas-inhibit-overlay-modification-protection t))

;;; Dashboard
(use-package dashboard
  ;; :load-path "~/src/emacs-dashboard"
  ;; :commands dashboard-insert-startupify-lists
  :custom-face
  ;; (dashboard-heading ((t (:family "Yanone Kaffeesatz" :weight light :height 200 :foreground "#E1BEE7"))))
  ;; (dashboard-banner-logo-title ((t (:family "Yanone Kaffeesatz" :weight light :height 250))))
  ;; (dashboard-heading ((t (:font "Yanone Kaffeesatz Light:style=Light,Regular" :weight light :height 200 :foreground "#E1BEE7"))))
  ;; (dashboard-banner-logo-title ((t (:font "Yanone Kaffeesatz Light:style=Light,Regular" :weight light :height 250))))
  ;; (dashboard-heading ((t (:font yanone-font-name :weight light :height 200 :foreground "#E1BEE7"))))
  ;; (dashboard-banner-logo-title ((t (:font yanone-font-name :weight light :height 250))))
  :config
  (set-face-attribute 'dashboard-heading nil :font yanone-font-name :weight 'light :height 200 :foreground "#E1BEE7")
  (set-face-attribute 'dashboard-banner-logo-title nil :font yanone-font-name :weight 'light :height 250)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(use-package page-break-lines
  ;; Used by the dashboard package
  :commands (page-break-lines-mode))

;;; impatient-mode
(use-package impatient-mode
  :disabled
  :defer t
  :config
  ;; Use with `impatient-mode' by running `M-x imp-set-user-filter' in a
  ;; markdown buffer, and supplying `markdown-html' as the argument.
  (defun markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))

  (defun impatient-markdown ()
    "Serve markdown text as formatted HTML.

   Start up the simple HTTP server if it isn't running, enable
   `impatient-mode', and set the user filter to embed buffer
   contents in an HTML skeleton that invokes a Markdown processor
   on the text."
    (interactive)
    (unless (get-buffer "*httpd*") (httpd-start))
    (impatient-mode)
    (imp-set-user-filter #'markdown-html)))


;;; esup
;; (use-package esup
;;   :defer t
;;   :commands esup
;;   :config (setq esup-user-init-file (file-truename "~/dotfiles/emacs")))
;;; Org-mode

;;;; General Org Configuration
(use-package org
  ; :ensure org-plus-contrib
  ;; :pin org
  :defer 1
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ;; Don't fight the bindings that use
         ;; shift-arrow to move focus between windows.
         :map org-mode-map
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil))
  :custom-face
  ;; (org-level-1 ((t (:foundry "UKWN" :family "Yanone Kaffeesatz" :weight light :height 250))))
  ;; (org-level-1 ((t (:font "Yanone Kaffeesatz Light:style=Light,Regular" :weight light :height 250))))
  ;; (org-level-1 ((t (:font yanone-font-name :weight light :height 250))))
  :config
  ;; (set-face-font 'org-level-1 "Yanone Kaffeesatz Light:style=Light,Regular")
  ;; (set-face-attribute 'org-level-1 nil yanone-font-name :weight 'light :height 250)
  (set-face-font 'org-level-1 yanone-font-name)
  (if (memq window-system '(mac ns))
      (set-face-attribute 'org-level-1 nil :height 400 :weight 'light)
    (set-face-attribute 'org-level-1 nil :height 250 :weight 'light))
  ;; (set-face-attribute 'org-todo nil :weight 'bold)
  ;; (set-face-attribute 'org-done nil :weight 'bold)

  (setq org-src-fontify-natively 't
        org-use-speed-commands 't
        org-html-doctype "html5"
        org-directory "~/org"
        org-default-notes-file "~/org/home.org"
        org-agenda-dim-blocked-tasks 'invisible
        org-enforce-todo-dependencies 't
        org-hide-emphasis-markers t
        org-startup-folded t
        org-return-follows-link t
        ;; Don't let you edit invisible areas (i.e. after ellipsis)
        org-catch-invisible-edits 'show-and-error
        ;; Hide blank lines between headings in collapsed view
        org-cycle-separator-lines 0
        org-priority-faces '((?A . (:foreground "forest green" :weight bold))
                             (?B . (:foreground "slate gray"))
                             (?C . (:foreground "dim gray")))

        org-format-latex-options `(:foreground default
                                   :background default
                                   :scale ,(if (memq window-system '(mac ns))
                                               1.0 2.0)
                                   :html-foreground "Black"
                                   :html-background "Transparent"
                                   :html-scale 1.0
                                   :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))

        org-latex-packages-alist '(("" "fontspec" nil))

        ;; For leuven-theme
        ;; Fontify the whole line for headings (with a background color).
        org-fontify-whole-heading-line 't

        ;; Don't indent text to align with the headline
        org-adapt-indentation nil

        ;; Display of the first data row of the table at point in the
        ;; window header line
        org-table-header-line-p t

        ;; Use NAME affiliated keyword to generate anchor IDs if
        ;; CUSTOM_ID is not set
        org-html-prefer-user-labels t

        org-todo-keywords '((sequence "TODO" "|" "DONE(d!)" "ABANDONED"))

        ;; Speed up org fontification
        org-priority-regexp "^\\*+.*\\(\\[#\\([A-Z0-9]+\\)\\] ?\\)"
        )

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (use-package ox-reveal
    :defer t)

  (use-package ox-md :commands (org-md-export-to-markdown))
  (use-package ox-rss :commands (org-rss-publish-to-rss))

  ;(set-alist 'org-preview-latex-process-alist 'imagemagick (append '(:programs ("latex" "convert")) (alist-get 'imagemagick org-preview-latex-process-alist)))

  ;; From https://emacs.stackexchange.com/a/33077/6537
  (defun color-org-header (tag backcolor forecolor)
    "Apply coloring to lines based on presence of a tag"
    (goto-char (point-min))
    (while (re-search-forward tag nil t)
      (message "Applying color for %s" tag)
      (add-text-properties (point-at-bol) (point-at-eol) `(face (:foreground ,forecolor)))))

  (defun my-agenda-colors ()
    (save-excursion
      (color-org-header (rx "[#B]") "black" "slate gray")
      (color-org-header (rx "[#C]") "black" "dim gray")))

  (add-hook 'org-agenda-finalize-hook #'my-agenda-colors)

  (setq org-image-actual-width '(800))
  (setq org-latex-prefer-user-labels t)

  (use-package org-superstar
    :commands (org-superstar-mode)
    :config
    (setq ;; org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
          org-superstar-headline-bullets-list '("»" "◉" "○" "✸" "✿")
          org-hide-leading-stars t))
  ;; (use-package org-table-sticky-header
  ;;   :diminish org-table-sticky-header-mode)
  ;; (use-package org-sticky-header)


  ;; This is slow to load
  (use-package ox-tufte
    :load-path "~/src/ox-tufte"
    :defer t)

  ;; This is also slow to load
  (use-package ox-gfm
    :commands (org-gfm-export-to-markdown org-gfm-export-as-markdown))

  (use-package ox-koma-letter
     :defer t
     :commands (org-koma-letter-export-to-pdf
                org-koma-letter-export-as-latex
                org-koma-letter-export-to-latex))

  ;; Adapated from https://www.reddit.com/r/orgmode/comments/43uuck/temporarily_show_emphasis_markers_when_the_cursor/czmtn29/
  (defun org-show-emphasis-markers-at-point ()
    "Show emphasis and verbatim markers around point. This makes it easier to edit arounds the ends of markup when using `org-hide-emphasis-markers'."
    (unless org-roam-backlinks-mode
      (save-match-data
        (if (and (or (org-in-regexp org-emph-re 2)
                     (org-in-regexp org-verbatim-re))
                 (>= (point) (match-beginning 3))
                 (<= (point) (match-end 4))
                 (member (match-string 3) (mapcar 'car org-emphasis-alist)))
            (with-silent-modifications
              (remove-text-properties
               (match-beginning 3) (match-beginning 5)
               '(invisible org-link)))
          (apply 'font-lock-flush (list (match-beginning 3) (match-beginning 5)))))))

  (defun my-org-hook ()
    (setq line-spacing 0.2)
    (setq header-line-format " ")
    (yas-global-mode)
    (org-superstar-mode 1)

    (add-hook 'post-command-hook
              'org-show-emphasis-markers-at-point nil t)

    ;; (org-table-sticky-header-mode)
    ;; (org-sticky-header-mode)

    ;; (require 'ox-extra)
    ;; (ox-extras-activate '(ignore-headlines))

    ;; electric quotes turn single quotes (') into smart single quotes
    ;; that can break things in src blocks
    (electric-quote-local-mode -1)

    ; Encryption
    ;; (require 'org-crypt)
    ;; (org-crypt-use-before-save-magic)
    ;; (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; ;; GPG key to use for encryption
    ;; ;; Either the Key ID or set to nil to use symmetric encryption.
    ;; (setq org-crypt-key "D50A574B")
)

  (use-package org-crypt
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    :custom (org-crypt-key "D50A574B")
    :commands (org-crypt-use-before-save-magic)
    :hook (org-mode . org-crypt-use-before-save-magic))
  (use-package org-ref
    :defer t
    :bind ("C-c C-r" . org-ref-helm-insert-cite-link)
    :config
    (setq ;; org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
          org-ref-default-bibliography '("~/Documents/MyPapers/mybib/mybib.bib")
          org-ref-pdf-directory "~/org/roam/references")

    (use-package doi-utils
      :commands (doi-utils-add-bibtex-entry-from-doi
                 doi-utils-insert-bibtex-entry-from-doi
                 doi-utils-get-bibtex-entry-pdf
                 doi-utils-update-bibtex-entry-from-doi))
    (use-package org-ref-arxiv
      :commands (arxiv-add-bibtex-entry
                 arxiv-get-pdf
                 arxiv-get-pdf-add-bibtex-entry)))

  (add-hook 'org-mode-hook #'my-org-hook)

  (use-package ob-ipython
    :defer t
    :config
    (defun kill-ihaskell ()
      "IHaskell dies after a few evaluations of a big notebook due
to keeping too many files open. This cleans things up so
evaluation may begin anew."
      (interactive)
      (mapc #'kill-buffer '("*Python*" "*ob-ipython-client-driver*"
                            "*ob-ipython-kernel-default*"))))


  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((haskell . t)
  ;;    ;; (ditaa . t)
  ;;    (shell . t)
  ;;    (emacs-lisp . t)
  ;;    (octave . t)
  ;;    ;; (C . t)
  ;;    ;; (js . t)
  ;;    ;; (maxima . t)
  ;;    (latex . t)
  ;;    (dot . t)))
  (use-package ob-shell
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:sh
               org-babel-expand-body:sh
               org-babel-execute:bash
               org-babel-expand-body:bash))
  (use-package ob-haskell
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:haskell org-babel-expand-body:haskell))

  (use-package ob-js
    :defer t
    :commands (org-babel-execute:js org-babel-expand-body:js))

  (defun org-babel-execute:runhaskell (body params)
    (org-babel-eval "runhaskell"
                    (org-babel-expand-body:generic body params)))
  (add-to-list 'org-src-lang-modes '("runhaskell" . haskell))

  (defun org-babel-execute:nix-shell (body params)
    (let* ((pkgs (if (null (assq :packages params))
                     ""
                   (concat " -p " (cdr (assq :packages params)))))
           (cmd (concat "nix-shell" pkgs " --run")))
      (message "Going to run: %s %s" cmd (quote-shell-string (org-babel-expand-body:generic body params)))
      (org-babel-eval (concat "nix-shell" pkgs " --run bash")
                       (org-babel-expand-body:generic body params))))

  (add-to-list 'org-src-lang-modes '("nix-shell" . sh))

  (add-to-list 'org-src-lang-modes '("dhall" . dhall))

  (use-package ob-emacs-lisp
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:elisp
               org-babel-expand-body:elisp
               org-babel-execute:emacs-lisp
               org-babel-expand-body:emacs_lisp))
  (use-package ob-org
    :defer t
    :commands (org-babel-execute:org org-babel-expand-body:org))
  (use-package ob-octave
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:octave))
  (use-package ob-latex
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:latex org-babel-expand-body:latex))
  (use-package ob-dot
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:dot org-babel-expand-body:dot))
  (use-package ob-R
    :defer t
    :commands (org-babel-execute:R org-babel-expand-body:R))
  (use-package ob-plantuml
    :defer t
    :commands (org-babel-execute:plantuml org-babel-expand-body:plantuml)
    :config
      :config
  (defun org-babel-execute:plantuml (body params)
    "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
    (let* ((out-file (or (cdr (assq :file params))
                         (error "PlantUML requires a \":file\" header argument")))
           (cmdline (cdr (assq :cmdline params)))
           (in-file (org-babel-temp-file "plantuml-"))
           (java (or (cdr (assq :java params)) ""))
           (full-body (org-babel-plantuml-make-body body params))
           (cmd (concat "plantuml "
                        (if (string= (file-name-extension out-file) "png")
                            " -tpng" "")
                        (if (string= (file-name-extension out-file) "svg")
                            " -tsvg" "")
                        (if (string= (file-name-extension out-file) "eps")
                            " -teps" "")
                        (if (string= (file-name-extension out-file) "pdf")
                            " -tpdf" "")
                        (if (string= (file-name-extension out-file) "tex")
                            " -tlatex" "")
                        (if (string= (file-name-extension out-file) "vdx")
                            " -tvdx" "")
                        (if (string= (file-name-extension out-file) "xmi")
                            " -txmi" "")
                        (if (string= (file-name-extension out-file) "scxml")
                            " -tscxml" "")
                        (if (string= (file-name-extension out-file) "html")
                            " -thtml" "")
                        (if (string= (file-name-extension out-file) "txt")
                            " -ttxt" "")
                        (if (string= (file-name-extension out-file) "utxt")
                            " -utxt" "")
                        " -p " cmdline " < "
                        (org-babel-process-file-name in-file)
                        " > "
                        (org-babel-process-file-name out-file))))
      (with-temp-file in-file (insert full-body))
      (message "%s" cmd) (org-babel-eval cmd "")
      nil)))
  (use-package ob-calc
    :defer t
    :commands (org-babel-execute:calc org-babel-expand-body:calc))
  (use-package ob-maxima
    :defer t
    :commands (org-babel-execute:maxima org-babel-expand-body:maxima))
  (use-package ob-C
    :defer t
    ; :ensure org-plus-contrib
    :commands (org-babel-execute:C org-babel-expand-body:C
               org-babel-execute:C++ org-babel-expand-body:C++)
    :config
    (when (memq window-system '(mac ns))
      (setq org-babel-C++-compiler "clang++")))

  ;; Syntax highlight dot source blocks
  ; (set-alist 'org-src-lang-modes "dot" 'graphviz-dot)

  ;; Org sets the block face to a hard-to-read gray color by default.
  ;; (set-face-attribute
  ;;  'org-block nil :foreground (face-attribute 'default :foreground))

  ;; Disable variable-pitch-mode in tables. We used to be
  ;; able to disable this in src blocks, but this no
  ;; longer works.
  ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images
            'append)

  (defun my/org-babel-next-src-block ()
    "Move point to the next babel src block. Returns the new point
location if there is a next src block; returns nil otherwise."
    (condition-case nil
        (org-babel-next-src-block)
      (user-error nil)))

  (defun my/org-execute-up-to-point ()
    "Execute every ipython source block whose :results are set to
silent between the beginning of the file and the current point
location."
    (interactive)
    (save-excursion
      (let ((stop (point)))
        (progn
          (goto-char (point-min))
          (let ((pt (my/org-babel-next-src-block)))
            (cl-loop until (or (null pt) (> pt stop)) do
                  (let ((info (org-babel-get-src-block-info 't)))
                    (when (and (string-equal "ipython" (nth 0 info)) ; language
                               (string-equal "silent"
                                             (assoc-default
                                              :results
                                              (nth 2 info) ; header-arguments-alist
                                              nil ""))
                               (not (string-match "^module " (nth 1 info))))
                      (progn (message "Executing %s block at line %s"
                                      (nth 0 info)
                                      (line-number-at-pos pt))
                             (org-babel-execute-src-block))))
                  (setq pt (my/org-babel-next-src-block))))))))

  (defun my/org-execute-session ()
    "Execute every org src block with a :session header up to point."
    (interactive)
    (save-excursion
      (let ((stop (point)))
        (goto-char (point-min))
        (let ((pt (my/org-babel-next-src-block)))
          (cl-loop until (or (null pt) (> pt stop)) do
                   (let* ((info (org-babel-get-src-block-info 't))
                          (sess (assoc :session (nth 2 info))))
                     (when (and (consp sess)
                                (or (not (stringp (cdr sess))) 
                                    (not (string-equal "none" (cdr sess)))))
                       (progn
                         (message "Executing %s block at line %s"
                                  (nth 0 info) (line-number-at-pos pt))
                         (org-babel-execute-src-block))))
                (setq pt (my/org-babel-next-src-block)))))))

  ;; Adapted from
  ;; http://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme
  (defun my/org-inline-css-hook (exporter)
    "Insert custom inline css to automatically set the background
of code to whatever theme I'm using's background"
    (when (eq exporter 'html)
      (let* ((my-pre-bg (face-background 'default))
             (my-pre-fg (face-foreground 'default)))
        (setq
         org-html-head-extra
         (concat
          org-html-head-extra
          (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s; overflow: scroll;} body { max-width: 600pt; margin: auto} </style>\n"
                  my-pre-bg my-pre-fg))))))

  ;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

  ;; Use fixed-width fonts where appropriate
  ;; From: https://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
  ;; (defun adjoin-to-list-or-symbol (element list-or-symbol)
  ;;   (require 'cl)
  ;;   (adjoin element (if (not (listp list-or-symbol))
  ;;                       (list list-or-symbol)
  ;;                     list-or-symbol)))
  ;; (mapc (lambda (face)
  ;;          (set-face-attribute face nil
  ;;                              :inherit (adjoin-to-list-or-symbol
  ;;                                         'fixed-pitch
  ;;                                         (face-attribute face :inherit))))
  ;;        ;(list 'org-code 'org-block 'org-table 'org-block-background)))
  ;;        (list 'org-code 'org-block 'org-table))

  ;; LaTeX export
  ;; (require 'ox-latex)
  (setq org-latex-pdf-process '("latexmk -g -xelatex -shell-escape %f -outdir=$(echo '%o' | sed 's|\\(.*\\)/$|\\1|g')"))
  ;; (setq org-latex-pdf-process '("latexmk -g -lualatex -shell-escape %f -outdir=$(echo '%o' | sed 's|\\(.*\\)/$|\\1|g')"))

  ;; Remove the grffile package as it broke image inclusion for me
  (setq org-latex-default-packages-alist '(("AUTO" "inputenc" t)
                                           ("T1" "fontenc" t)
                                           ;; ("" "fixltx2e" nil)
                                           ("" "graphicx" t)
                                           ("" "longtable" nil)
                                           ("" "wrapfig" nil)
                                           ("" "rotating" nil)
                                           ("normalem" "ulem" t)
                                           ("" "amsmath" t)
                                           ("" "textcomp" t)
                                           ("" "amssymb" t)
                                           ("" "capt-of" nil)
                                           ;; ("colorlinks=true" "hyperref" nil)
                                           ("" "hyperref" nil)
                                           ))

  ;; (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (add-to-list 'org-latex-minted-langs '(haskell "haskell"))

  ;; mathescape permits math-mode in comments, while escapeinside
  ;; specifies brackets within which math-mode can be enabled in code
  ;; listings.
  (setq org-latex-minted-options
        '(("escapeinside" "||") ("mathescape=true")))
  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))

  ;; (require 'org-clock)
  ;; (add-to-list
  ;;  'org-clock-clocktable-language-setup
  ;;  '("en" "File"     "L"  "Timestamp"  "Task" "Time"  "ALL"   "Total time"   "File time" "Time Sheet at"))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda . " %i %-12:c%?-12t% s")
  ;;         (timeline . "  % s")
  ;;         (todo . " %i %b%-12:c")
  ;;         (tags . " %i %-12:c")
  ;;         (search . " %i %-12:c")))

;;;; Blog Publishing
  (defun org-custom-link-blog-follow (path)
    (org-open-file-with-emacs path))

  (defun org-custom-link-blog-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<a href=\"https://www.arcadianvisions.com/%s.html\">%s</a>"
              (file-name-sans-extension path)
              desc))))

  (defun my/blog-get-article-slug ()
    "Return an Org item based on the current buffer suitable for
insertion into the blog's index."
    (if (not (eq major-mode 'org-mode))
        (message "Only works in an Org buffer")
      (let* ((title (substring-no-properties (car (plist-get (org-export-get-environment) :title))))
             (rel-path (progn
                         (string-match ".*?/articles/\\(.*?\\)$" (buffer-file-name))
                         (match-string 1 (buffer-file-name)))))
        (concat "* " title "\n"
                ":PROPERTIES:\n:RSS_PERMALINK: " rel-path "\n:END:"
                "#+include: \"" rel-path "\"::Slug\n\n"
                "[[blog:" rel-path "][read more]]"))))

  (defun buffer-for-file (filename)
    "Return an existing buffer visiting `FILENAME`, or open a new
buffer visiting `FILENAME` if none exists."
    (or (get-file-buffer filename) (find-file filename)))

  (defun my/blog-add-article-to-index ()
    "Add the current Org buffer as an entry in the blog's index."
    (interactive)
    (save-current-buffer
      (let* ((entry (my/blog-get-article-slug))
             (index-dir (locate-dominating-file (buffer-file-name) "index.org"))
             (index-buf (buffer-for-file (concat index-dir "index.org"))))
        (with-current-buffer index-buf
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp "^*")
            (forward-line -1)
            (insert "\n" entry "\n"))))))

  (defun new-blog-article (arg title)
    "Produce the boilerplate at the start of every blog article after
prompting for the article's title."
    (interactive "P\nMtitle: ")
    (let* ((date (format-time-string "%Y-%m-%d" (current-time)))
           (txt (mapconcat #'identity
                           (list (concat "#+TITLE: " title)
                                 (concat "#+DATE: <" date ">")
                                 "#+OPTIONS: html-postamble:nil num:nil toc:nil"
                                 ""
                                 "#+BEGIN_EXPORT html"
                                 (concat "<p class=\"date\">" date "</p>")
                                 "#+END_EXPORT"
                                 ""
                                 "* Slug :ignore:"
                                 "")
                           "\n")))
      (insert txt)))

  (org-link-set-parameters
   "blog"
   :follow #'org-custom-link-blog-follow
   :export #'org-custom-link-blog-export)

  (setq org-rss-use-entry-url-as-guid nil)
  (defvar my/blog-directory
    (let ((d1 "~/Documents/Projects/Blog")
          (d2 "~/Projects/Blog"))
      (if (file-exists-p d1) d1 d2))
    "Local directory for blog files")

  (defun my/blog-copy-index-to-rss (_)
    (shell-command (format "(cd %s/blog && cp index.xml rss.xml && nix-shell -p gnused --run \"sed 's/index.xml/rss.xml/' -i ./rss.xml\")" my/blog-directory)))
  (defun my/blog-sync-assets (_)
    (shell-command (format "rsync -a %s/blog/assets/basedir/ %s/blog" my/blog-directory my/blog-directory)))
  (setq org-publish-project-alist
        `(("blog-content"
           :base-directory ,(concat my/blog-directory "/articles/")
           :publishing-directory ,(concat my/blog-directory "/blog/")
           :publishing-function org-html-publish-to-html
           :export-babel-evaluate nil
           :exclude "2019/subt"
           :recursive t
           ;; :auto-sitemap t
           ;; :sitemap-filename "index.html"
           ;; :sitemap-title "Arcadian Visions Blog"
           ;; :makeindex t
           :htmlized-source t
           :with-author "Anthony Cowley"
           :email "acowley@gmail.com"
           :with-creator nil
           :with-date nil
           :with-email "acowley@gmail.com"
           :with-timestamps nil
           :with-toc nil
           :section-numbers nil
           :html-head-include-default-style nil
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../core-style.css\" />"
           :html-postamble "")
          ("blog-tufte"
           :base-directory ,(concat my/blog-directory "/articles")
           :publishing-directory ,(concat my/blog-directory "/blog/")
           :publishing-function org-html-publish-to-tufte-html
           :org-tufte-include-footnotes-at-bottom nil
           :recursive t
           :exclude ,(rx line-start "201" (not (any ?9)) "/" (* not-newline))
           :htmlized-source t
           :with-creator nil
           :with-date nil
           :with-email nil
           :with-timestamps nil
           :with-toc nil
           :section-numbers nil
           :html-head-include-default-style nil
           ;; :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../core-style.css\" />"
           ;; :html-postamble ""
           )
          ("blog-rss"
           :base-directory ,(concat my/blog-directory "/articles/")
           :publishing-directory ,(concat my/blog-directory "/blog")
           :publishing-function (org-rss-publish-to-rss)
           :html-link-home "http://www.arcadianvisions.com/"
           :completion-function my/blog-copy-index-to-rss
           :html-link-use-abs-url t
           :exclude ".*"
           :include ("index.org")
           :author "Anthony Cowley"
           :with-toc nil
           :with-creator nil
           :with-author "Anthony Cowley"
           :email "acowley@gmail.com"
           :with-email "acowley@gmail.com"
           :section-numbers nil)
          ("blog-assets"
           ;; Static content like images and CSS
           :base-directory ,(concat my/blog-directory "/assets")
           :base-extension any
           :include ("basedir/.htaccess")
           :recursive t
           :publishing-directory ,(concat my/blog-directory "/blog/assets")
           :publishing-function org-publish-attachment
           :completion-function my/blog-sync-assets)
          ("blog" :components ("blog-content" "blog-rss" "blog-assets"))))

  ;;; org-noter
  (use-package org-noter :defer t :commands (org-noter))

  ;;; org-noter-pdftools
  (use-package org-noter-pdftools
    :after org-noter
    :config
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

  ;;; org-pdftools
  (use-package org-pdftools
    :after org-noter
    :config
    (with-eval-after-load 'org (org-pdftools-setup-link)))

;;;; org-clock

(defun my-org-clocktable-notodo (ipos tables params)
  "Remove the TODO and DONE keywords from clock table
entries. From
http://emacs.stackexchange.com/questions/8228/remove-task-state-keywords-todo-done-from-clocktable-reports"
  (cl-loop for tbl in tables
           for entries = (nth 2 tbl)
           do (cl-loop for entry in entries
                       for headline = (nth 1 entry)
                       do (setq headline (replace-regexp-in-string "TODO \\|DONE " "" headline))
                       do (setcar (nthcdr 1 entry) headline)))
  (org-clocktable-write-default ipos tables params))

;;;; Project Task Capture

;; I use a convention where projects have a ProjectName-notes.org file
;; in the project root directory. This file is used for design notes
;; and task lists. It can be good to setq `org-agenda-files' to
;; include all your active projects so that the tasks show up in the
;; org agenda view. I set this value in the
;; [[%3B%3B%3B%20Private%20Configuration][Private Configuration]]
;; section of this file.

;; With the given configuration "C-c c p" adds a TODO item to the
;; current project's notes file.
(defun find-project-notes ()
  "A project's notes file is defined as ProjectName-notes.org in
  the project root directory."
  (concat (projectile-project-root) "/" (projectile-project-name) "-notes.org"))

  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  SCHEDULED: %t\n%i\n")
          ("p" "Project Task" entry (file+headline (find-project-notes) "Tasks")
           "* TODO %?\n  %i\n  %a")))
  )
;;;; org-roam
(use-package org-roam
  :defer 5
  :after org
  :commands (org-roam-mode org-roam org-roam-find-file org-roam-show-graph)
  :custom
  (org-roam-directory "~/org/roam")
  ;; (org-roam-link-representation 'title)
  :bind
  (("C-c r l" . org-roam)
   ("C-c r f" . org-roam-find-file)
   ("C-c r g" . org-roam-show-graph)
   :map org-mode-map (("C-c r i" . org-roam-insert)))
  ;; :bind (:map org-roam-mode-map
  ;;             (("C-c n l" . org-roam)
  ;;              ("C-c n f" . org-roam-find-file)
  ;;              ("C-c n g" . org-roam-show-graph))
  ;;       :map org-mode-map
  ;;            (("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-graphviz-executable "dot")
  (require 'org-roam-protocol)
  (org-roam-mode))

;;;; org-roam-bibtex
(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :commands (org-roam-bibtex-notes-fn
             org-roam-bibtex-edit-notes-ad
             org-roam-bibtex-process-file-field
             org-roam-bibtex-edit-notes
             org-roam-bibtex-find-non-ref-file
             org-roam-bibtex-insert-non-ref))

(use-package bibtex-completion
  :after org-ref
  :config
  (setq bibtex-completion-bibliography '("~/Documents/MyPapers/mybib/mybib.bib")))

;;;; outorg
(use-package outorg
  :defer t
  :commands (outorg-edit-as-org)
  :config
  (defun outorg-whole-file ()
  "Call `outorg-edit-as-org` with a prefix argument so that the
entire source file is loaded."
    (interactive)
    (outorg-edit-as-org '(4))))

;;;; outshine
(use-package outshine
  :defer t
  :commands (outshine-mode)
  ;; :init
  ;; (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
  ;; (add-hook 'prog-mode-hook #'outline-minor-mode)
  :config
  (setq outshine-preserve-delimiter-whitespace t
        outshine-use-speed-commands t))

;; Copy from an Org buffer to the system clipboard after converting
;; the Org content to rich text format.
;; (use-package ox-clip
;;   :defer t
;;   :commands (ox-clip-formatted-copy))


;;;; org-mime

(use-package org-mime
  :defer t
  :commands (org-mime-org-buffer-htmlize org-mime-org-subtree-htmlize))

;;;; org-journal
(use-package org-journal
  :defer t
  :commands (org-journal-new-entry
             org-journal-mode
             org-journal-new-date-entry
             org-journal-new-scheduled-entry
             org-journal-list-dates
             org-journal-mark-entries
             org-journal-read-entry
             org-journal-display-entry
             org-journal-read-or-display-entry
             org-journal-next-entry
             org-journal-previous-entry
             org-journal-search)
  :bind (("C-c C-j" . org-journal-new-entry)
         ("C-c j" . org-journal-new-entry)))

;;;; org-ql
(use-package org-ql
  :commands (org-ql-query org-ql-search org-ql-select org-ql))
;;; elegant-agenda
(use-package elegant-agenda-mode 
  :hook org-agenda-mode
  :disabled
  :init
  (defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
  (setq org-agenda-custom-commands
      '(("d" "Today"
         (;; (tags-todo "SCHEDULED<\"<+1d>\"&PRIORITY=\"A\""
          ;;            ((org-agenda-skip-function
          ;;              '(org-agenda-skip-entry-if 'todo 'done))
          ;;             (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags-todo "PRIORITY=\"A\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 'day)
                      (org-scheduled-delay-days -14)
                      (org-agenda-overriding-header "Schedule")))
          (tags-todo "SCHEDULED<\"<+1d>\""
                     ((org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'done)
                            (air-org-skip-subtree-if-priority ?A)))
                      (org-agenda-overriding-header "Tasks:"))))))))
;;; org-books
(use-package org-books
  :commands (org-books-add-book
             org-books-rate-book
             org-books-cliplink
             org-books-add-url
             org-books-add-isbn)
  :custom
  (org-books-file "~/org/home.org"))
;;; olivetti-mode
(use-package olivetti
  :commands (olivetti-mode)
  :custom (olivetti-body-width 90)
  :defer t)
;;; pdf-tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (use-package pdf-occur
    :commands (pdf-occur-global-minor-mode))
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-links
    :commands (pdf-links-minor-mode))
  (use-package pdf-outline
    :commands (pdf-outline-minor-mode))
  (use-package pdf-annot
    :commands (pdf-annot-minor-mode))
  (use-package pdf-sync
    :commands (pdf-sync-minor-mode))
  ;; If pdf-tools is installed using emacsWithPackage in nix, then the
  ;; `epdfinfo` binary is installed alongside the elisp package.
  (setq pdf-info-epdfinfo-program
        (concat (file-name-directory (locate-library "pdf-tools"))
                "epdfinfo")
        pdf-info-epdfinfo-error-filename nil)
  (pdf-tools-install))
;;; Helm
(use-package helm
  :defer 1
  ;; :diminish helm-mode
  :commands (helm-find-files helm-mini helm-M-x helm-imenu helm-mode)
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("C-c i" . helm-imenu)
         ("C-c C-i" . helm-imenu)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ;; use TAB for action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB work in terminal
         ("C-i" . helm-execute-persistent-action)
         ;; list actions
         ("C-z" . helm-select-action))
  :config
  ;; Helm locks up when ligatures are enabled
  (add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))
  (setq
   helm-always-two-windows t
   helm-candidate-number-limit 100

   ;; This sometimes looks slick, but the new frame doesn't stand out
   ;; from the existing frame very well, and having the frame pop up
   ;; at point means that you need to be looking at point rather than
   ;; at a window split.
   ;; helm-display-function #'helm-display-buffer-in-own-frame

   helm-quick-update t
   helm-M-x-requires-pattern 3        ; Require at least one character
   helm-ff-file-name-history-use-recentf t
   helm-ff-skip-boring-files t

                                        ; helm-idle-delay 0.0
                                        ; helm-input-idle-delay 0.01

   ;; Use Spotlight on OS X to find files
   helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-buffer-not-found
                               helm-source-locate))
  (setq helm-locate-command
        (cond
         ((memq window-system '(mac ns))
          "mdfind -onlyin $HOME -name %s %s | grep -E -v '/dist/|/Caches/'")
         ((file-exists-p "/run/current-system/sw/bin/locate")
          "/run/current-system/sw/bin/locate %s -e -A --regex %s")
         (t "mlocate %s -e -A --regex %s")))

  (require 'helm-imenu)
  (require 'helm-command)
  (use-package helm-company
    :defer t
    :bind (:map company-mode-map
                ("C-:" . helm-company)))

  (defun my/search-forward (prefix)
    "Calls `helm-swoop` unless a prefix argument is given, in which case it calls `isearch-forward`"
    (interactive "P")
    (if (null prefix)
        (helm-swoop)
      (isearch-forward)))

  (use-package helm-swoop
    :defer t
    :commands helm-swoop
    :bind (("M-i" . helm-swoop)
           ("C-s" . my/search-forward)
           ("M-I" . helm-swoop-back-to-last-point)))

  (use-package helm-dash
    :defer t
    :commands (projectile-helm-dash)
    :config
    (setq helm-dash-browser-func #'eww)
    (defun projectile-helm-dash ()
      "Set the helm-dash docsets path to the 'docsets' directory
under the current project's root directory."
      (interactive)
      (setq helm-dash-docsets-path (concat (projectile-project-root) "docsets"))
      (message (format "Loaded docsets for %s" (projectile-project-name)))))
  ;; (use-package helm-tramp
  ;;   :defer t
  ;;   :config
  ;;   (use-package docker-tramp))
  (use-package helm-projectile
    :defer t)
  (use-package wgrep-helm
    :commands (wgrep-change-to-wgrep-mode wgrep-finish-edit wgrep-helm-setup)
    :hook ((helm-moccur-mode-hook helm-grep-mode-hook) . wgrep-helm-setup)
    :bind (:map wgrep-mode-map
                ("C-x C-s" . my/wgrep-save)
                ("C-c C-e" . my/wgrep-save)
                ("C-c C-k" . my/wgrep-discard))
    :config
    (defun my/wgrep-save ()
      "Kill the buffer if it was a helm-occur result"
      (interactive)
      (wgrep-finish-edit)
      (when (eq major-mode 'helm-occur-mode)
        (kill-this-buffer)))
    (defun my/wgrep-discard ()
      "Kill the buffer if it was a helm-occur result"
      (interactive)
      (wgrep-abort-changes)
      (when (eq major-mode 'helm-occur-mode)
        (kill-this-buffer)))
    (defun my/helm-occur-hook ()
      "Switch the by-default read-only helm-occur buffer to editable"
      (interactive)
      (wgrep-change-to-wgrep-mode))
    (add-hook 'helm-occur-mode-hook #'my/helm-occur-hook))
  (helm-mode 1)
  )
;; (require 'helm)
;; (helm-mode 1)
;; (ido-mode -1)

;;; helm-notmuch
(use-package helm-notmuch
  :commands (helm-notmuch))

;;; helm-org-rifle
(use-package helm-org-rifle
  :defer t
  :commands (helm-org-rifle helm-org-rifle-agenda-files)
  ;; :bind (("C-c r" . helm-org-rifle-agenda-files))
  )

;;; god-mode

(use-package god-mode
  :defer nil
  :commands (god-mode god-mode-all)
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)

;;   (defun ac/god-mode-toggle ()
;;     "Set the mode line to a white background when god-mode is
;; active; black when inactive."
;;     (if god-local-mode
;;         (progn
;;           ;; (set-face-background 'sml/line-number "orange")
;;           ;; (set-face-foreground 'sml/line-number "black")
;;           ;; (set-face-attribute 'mode-line nil :box "orange")
;;           ;; (hl-line-mode 1)
;;           )

;;       ;; (set-face-background 'sml/line-number "black")
;;       ;; (set-face-foreground 'sml/line-number "white")
;;       ;; (set-face-attribute 'mode-line nil :box nil)
;;       ;(unless (eq major-mode 'mu4e-headers-mode) (hl-line-mode -1))
;;       ))

;;   (add-hook 'god-mode-enabled-hook #'ac/god-mode-toggle)
;;   (add-hook 'god-mode-disabled-hook #'ac/god-mode-toggle)
  (defun ac/god-enable-on-new-buffers (&rest buffer-name)
    "Enable god-mode on new buffers if it is enabled globally."
    (when (and god-global-mode
               (not (null buffer-name))
               (string-match "^multiswitch" (car buffer-name)))
      (god-mode-activate 1)))
  (advice-add #'rename-buffer
              :after
              #'ac/god-enable-on-new-buffers
              '((name . "Set god status on new buffers")))

  (defun ac/god-toggle-on-overwrite ()
    "Toggle god-mode on overwrite-mode."
    (if (bound-and-true-p overwrite-mode)
        (god-local-mode-pause)
      (god-local-mode-resume)))

  (add-hook 'overwrite-mode-hook #'ac/god-toggle-on-overwrite)

  :bind (("<escape>" . god-mode-all)
         ("C-x C-o" . other-window) ;; Easier to use with god-mode
         ;; I swap these becuase I use switch-to-buffer much more frequently
         ;; and prefer it to have the simpler binding.
         ("C-x b" . list-buffers)
         ;; ("C-x C-b" . switch-to-buffer)
         ("C-x C-b" . helm-buffers-list)
         :map god-local-mode-map
         ("." . repeat)))

;; (global-set-key (kbd "<f9>") 'god-mode-all)
;; (define-key god-local-mode-map (kbd ".") 'repeat)
;; (global-set-key (kbd "C-x C-o") 'other-window)
;; (global-set-key (kbd "C-s") #'helm-swoop) ; instead of isearch-forward
;; (global-set-key (kbd "C-x b") #'list-buffers)
;; (global-set-key (kbd "C-x C-b") #'switch-to-buffer)


;;; Email (notmuch)
(use-package notmuch
  :commands (notmuch)
  :hook
  (notmuch-show-mode . my/notmuch-show-hook)
  (message-mode . my/message-mode-hook)
  :bind (:map notmuch-show-mode-map
              ("d" .
               (lambda ()
                 "toggle deleted tag for message"
                 (interactive)
                 (if (member "trash" (notmuch-show-get-tags))
                     (notmuch-show-tag '("-trash"))
                   (notmuch-show-tag '("+trash" "-inbox" "-unread" "-new")))
                 (notmuch-show-next-message)))
              ("r" . #'my-notmuch-reply-sender)
              ("R" . #'my-notmuch-reply)
              ("a" .
               (lambda ()
                 (interactive)
                 ;; (if (member "archived" (notmuch-show-get-tags))
                 ;;     (notmuch-show-tag '("-archived"))
                 ;;   (notmuch-show-tag '("+archived" "-inbox" "-unread" "-gmail/Inbox" "-seas/Inbox" "-new")))
                 (if (member "inbox" (notmuch-show-get-tags))
                     (notmuch-show-tag '("-inbox" "-unread"))
                   (notmuch-show-tag '("+inbox")))
                 (notmuch-show-next-thread)))
              :map notmuch-search-mode-map
              ("d" .
               (lambda ()
                 (interactive)
                 (notmuch-search-tag '("+trash" "-inbox" "-unread" "-new"))
                 (notmuch-search-next-thread))
               ;; (lambda ()
               ;;   "toggle deleted tag for message"
               ;;   (interactive)
               ;;   (if (member "trash" (notmuch-search-get-tags))
               ;;       (notmuch-search-tag '("-trash"))
               ;;     (notmuch-search-tag '("+trash" "-inbox" "-unread" "-new")))
               ;;   (notmuch-search-next-thread))
               )
              ("a" .
               (lambda ()
                 (interactive)
                 ;; (if (member "archived" (notmuch-search-get-tags))
                 ;;     (notmuch-search-tag '("-archived"))
                 ;;   (notmuch-search-tag '("+archived" "-inbox" "-unread" "-gmail/Inbox" "-seas/Inbox" "-new")))
                 (if (member "inbox" (notmuch-search-get-tags))
                     (notmuch-search-tag '("-inbox" "-unread"))
                   (notmuch-search-tag '("+inbox")))
                 (notmuch-search-next-thread))))
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-fcc-dirs '(("acowley@gmail.com" . nil;; "gmail/sent"
                       )
                      ("acowley@seas.upenn.edu" . nil;; "seas/sent"
                       )
                      ("acowley@scalableautonomy.com" . nil)))
  (notmuch-poll-script "~/dotfiles/notmuch-sync-new.sh")
  :custom-face
  (notmuch-search-date ((t (:foreground "SteelBlue"))))
  (notmuch-search-count ((t (:foreground "SteelBlue"))))
  ;; (notmuch-search-matching-authors ((t (:foreground "CadetBlue"))))
  (notmuch-search-matching-authors ((t (:foreground "DeepSkyBlue"))))
  ;; (notmuch-search-subject ((t (:foreground "DeepSkyBlue"))))
  (notmuch-search-subject ((t (:foreground "white"))))
  (notmuch-search-unread-face ((t (:foreground "CadetBlue"))))
  (notmuch-search-subject ((t (:family "Montserrat" :weight light :height 120 :foreground "white"))))

  :config
  (set-face-attribute 'notmuch-search-subject nil :font "Montserrat" :weight 'normal :height 120 :foreground "gainsboro")
  (set-face-attribute 'notmuch-tag-face nil :foreground "olive drab")
  (set-face-attribute 'notmuch-search-subject nil :font "Victor Mono" :weight 'normal :height 120 :foreground "white")
  (set-face-attribute 'notmuch-search-unread-face nil :foreground "CadetBlue")
  
  (defun my/notmuch-show-hook ()
    (setq olivetti-body-width 90)
    (set-face-attribute 'variable-pitch nil :height 120 :weight 'normal)
    (variable-pitch-mode)
    (olivetti-mode)
    (set-face-attribute 'header-line nil :font "Montserrat" :height 150 :weight 'light))

  (defun my/message-mode-hook ()
    (turn-off-auto-fill)
    (turn-on-gnus-dired-mode)
    (variable-pitch-mode)
    (olivetti-mode))
  (defun my/notmuch-search-hook ()
    (setq line-spacing 0.2))
  ;; Associate firefox with the `text/html' MIME type so that typing
  ;; ".v" opens an HTML part of an email message in firefox.
  (setq mailcap-user-mime-data '(((viewer . "firefox %s") (type . "text/html"))))
  (defface notmuch-search-deleted-face
    '((t (:foreground "tomato")))
    "Face for the `deleted' tag."
    :group 'notmuch-search :group 'notmuch-faces)
  (add-to-list 'notmuch-search-line-faces '("deleted" . notmuch-search-deleted-face))
  ;; (add-to-list 'notmuch-search-line-faces '("trash" . notmuch-search-deleted-face))
  (add-hook 'notmuch-search-hook #'my/notmuch-search-hook)
  (setq user-full-name  "Anthony Cowley")
  (defun evil-collection-notmuch-toggle-tag (tag mode &optional next-function)
    "Toggle TAG tag for message in MODE."
    (let ((get (intern (format "notmuch-%s-get-tags" mode)))
          (set (intern (format "notmuch-%s-tag" mode)))
          (next (or next-function (intern (format "notmuch-%s-next-message" mode)))))
      (funcall set (list (concat (if (member tag (funcall get))
                                     "-" "+")
                                 tag)))
      (funcall next)))
  (defun evil-collection-notmuch-search-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (evil-collection-notmuch-toggle-tag "deleted" "search" 'notmuch-search-next-thread))

;;;; Additional SMTP Accounts
  ;; From http://varunbpatil.github.io/2013/08/19/eom/#.VQtWSFyCZSU
  (defvar my-notmuch-account-alist
    '(("gmail"
       (user-mail-address "acowley@gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@gmail.com")
       (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "acowley@gmail.com" nil)))
       (smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "acowley@gmail.com" nil))))
      ("upenn"
       (user-mail-address "acowley@seas.upenn.edu")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@seas.upenn.edu")
       (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "acowley@seas.upenn.edu" nil)))
       (smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "acowley@seas.upenn.edu" nil))))
      ("sa"
       (user-mail-address "acowley@scalableautonomy.com")
       (smtpmail-default-smtp-server "mail.privateemail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@scalableautonomy.com")
       (smtpmail-starttls-credentials '(("mail.privateemail.com" 587 "acowley@scalableautonomy.com" nil)))
       (smtpmail-auth-credentials
        '(("mail.privateemail.com" 587 "acowley@scalableautonomy.com" nil))))))

  (defun my-ensure-list (x)
    "If the given value is a list, leave it alone. If it isn't,
cons it to nil."
    (if (listp x) x (cons x nil)))

  (defun my-first (f xs)
    "Returns the first element of the list for which the given
predicate returns true."
    (cond
     ((null xs) nil)
     ((funcall f (car xs)) (car xs))
     (t (my-first f (cdr xs)))))

  (defun my-mem-string (x xs)
    "memq using 'string-equal' for equality."
    (cond
     ((null xs) nil)
     ((string-equal x (car xs)) xs)
     (t (my-mem-string x (cdr xs)))))

  (defun my-switch-email-account (possibles &optional prompt)
    (when possibles
      (let* ((my-addresses (mapcar #'(lambda (account)
                                         (cons (car account)
                                               (cadr (assoc 'user-mail-address
                                                            (cdr account)))))
                                     my-notmuch-account-alist))
             (my-address (progn
                           (my-first #'(lambda (x)
                                         (my-mem-string (cdr x) possibles))
                                     my-addresses)))
             (account (if my-address (car my-address)
                        (when prompt
                          (completing-read
                           (format "Send with account: (%s) "
                                   (mapconcat #'(lambda (var) (car var))
                                              my-notmuch-account-alist "/"))
                           (mapcar #'(lambda (var) (car var))
                                   my-notmuch-account-alist)
                           nil t nil nil (caar my-notmuch-account-alist))))))
        (when account
          (message "Activating email settings for %s" account)
          (mapc #'(lambda (var) (set (car var) (cadr var)))
                (cdr (assoc account my-notmuch-account-alist)))))))

  (defun my-notmuch-set-reply-account ()
    "Set the account for sending a message"
    (let ((recipients
                (split-string (notmuch-show-get-to)
                              (rx (or space ?\, ?\< ?\>))
                              t)))
      (my-switch-email-account recipients t)
      (message-make-from user-full-name user-mail-address)))

  (defun my-notmuch-reply ()
"Reply-All that picks which sending account to use by looking for
a sending account which was a recipient of the email."
    (interactive)
    (notmuch-mua-reply (notmuch-show-get-message-id) (my-notmuch-set-reply-account) t))
  (defun my-notmuch-reply-sender ()
"Reply that picks which sending account to use by looking for a
sending account which was a recipient of the email."
    (interactive)
    (notmuch-mua-reply (notmuch-show-get-message-id) (my-notmuch-set-reply-account) nil))
  (advice-add #'message-make-from :before (lambda (&rest r)
                                            (my-switch-email-account
                                             (if (or (null r)) nil (cdr r)))))

  ;; (add-hook 'notmuch-mua-send-hook #'my-notmuch-set-account)
  )
;;; Email (mu4e)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)

(use-package mu4e
  :load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"
  ; :ensure nil
  :defer t
  :commands (mu4e)
  :bind (:map mu4e-view-mode-map ("<backspace>" . mu4e-view-mark-or-move-to-trash))
  :config
  (setq
   mu4e-maildir "~/.mail"
   ;; mu4e-html2text-command  "/Users/acowley/.nix-profile/bin/w3m -T text/html"
   ;; mu4e-get-mail-command "~/.nix-profile/bin/mbsync gmail-inbox gmail-trash"
   mu4e-html2text-command "w3m -T text/html"
   mu4e-get-mail-command "mbsync gmail-inbox gmail-trash gmail-sent seas-inbox seas-trash seas-sent"

   ;; gmail folder setup
                                        ;mu4e-drafts-folder "/gmail/drafts"
   mu4e-drafts-folder "/mu4e/drafts"
   mu4e-sent-folder   "/gmail/sent"
   ;; mu4e-trash-folder  "/gmail/trash"
   mu4e-trash-folder (lambda (msg)
                       (if (string-prefix-p "/seas"
                                            (mu4e-message-field msg :maildir))
                           "/seas/trash"
                         "/gmail/trash"))

   mu4e-move-to-trash-patterns (list (rx (or "/seas" "/gmail")))

   mu4e-headers-skip-duplicates t
   mu4e-compose-dont-reply-to-self t
   mu4e-view-show-images t
   mu4e-view-scroll-to-next nil
   mu4e-update-interval 600

   ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
   ;; mu4e-sent-messages-behavior 'delete

   ;; setup some handy shortcuts
   ;; you can quickly switch to your Inbox -- press ``ji''
   ;; then, when you want archive some messages, move them to
   ;; the 'All Mail' folder by pressing ``ma''.
   mu4e-maildir-shortcuts '( ("/gmail/Inbox"   . ?i)
                             ;; ("/gmail/sent"    . ?s)
                             ("/gmail/trash"   . ?t)
                             ("/gmail/archive" . ?a)
                             (:maildir "/seas/archive" :key ?s)
                             ("/seas/Inbox"    . ?u))

   ;; something about ourselves
   user-mail-address "acowley@gmail.com"
   user-full-name  "Anthony Cowley"
   mu4e-compose-signature-auto-include nil
   mu4e-compose-signature nil
   mu4e-change-filenames-when-moving t

   ;; alternatively, for emacs-24 you can use:
   message-send-mail-function 'smtpmail-send-it
   smtpmail-stream-type 'starttls
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587

   ;; don't keep message buffers around
   message-kill-buffer-on-exit t
   mu4e-headers-fields
   '( (:human-date    .  12)    ;; alternatively, use :human-date
      (:flags         .   4)
      (:from          .  22)
      (:subject       .  nil)) ;; alternatively, use :thread-subject
   )
  (set-face-foreground 'mu4e-flagged-face "orange")
  (set-face-attribute 'mu4e-view-body-face nil :family "Montserrat" :weight 'light)
  (set-face-attribute 'mu4e-header-face nil :family "Victor Mono" :weight 'demi)
  (set-face-attribute 'mu4e-header-key-face nil :family "Victor Mono" :slant 'italic)
  (set-face-attribute 'mu4e-header-value-face nil :foreground "Green" :weight 'normal)

  ;; Auto-complete contact email addresses
  ;; We don't want line breaks added to emails we compose
  (defun my/mu4e-compose-hook ()
    (company-mode)
    (turn-off-auto-fill)
    ;; (variable-pitch-mode)
    (turn-on-visual-line-mode)
    ;; (setq buffer-face-mode-face (if (memq window-system '(mac ns) )
    ;;                                 '(:family "Avenir Next")
    ;;                               '(:family "Cantarell")))
    (buffer-face-mode))

  (add-hook 'mu4e-compose-mode-hook #'my/mu4e-compose-hook)

  (defun my/mu4e-headers-hook ()
    ;; The mu4e headers view slows down a lot with ligatures
    (setq auto-composition-mode nil))

  (add-hook 'mu4e-headers-mode-hook #'my/mu4e-headers-hook)

  ;; Add a view in browser action. Trigger with "aV"
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; NOTE: deleting a message in Gmail is accomplished by moving to the
  ;; trash folder. "Marking for deletion" actually archives the message.
  ;; (fset 'my-move-to-trash "mt")
  ;; (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  ;; (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
                                        ;(setq mu4e-view-prefer-html t)
                                        ;(setq mu4e-html2text-command "html2text -utf8 -width 72")

  (defun my/mu4e-view-hook ()
    (setq buffer-face-mode-face (if (memq window-system '(mac ns) )
                                    '(:family "Avenir Next")
                                  '(:family "Cantarell")))
    (olivetti-mode 1)
    (buffer-face-mode 1)
    (text-scale-adjust 1))

  (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-hook)

  ;; Mark email attachments in dired with C-c RET C-a
  ;; From http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
  (use-package gnus-dired
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    ; :ensure nil
    :config
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

  ;; Automatically update every 10 minutes and pop up a notification if
  ;; the index changed.
  (defun newest-subject ()
    (let* ((mu-res (concat "(list "
                           (shell-command-to-string "mu find maildir:'/gmail/Inbox' flag:unread --format=sexp 2>/dev/null; mu find maildir:'/seas/Inbox' flag:unread --format=sexp 2>/dev/null")
                           ")"))
           (msgs (last (car (read-from-string mu-res)))))
      (mapconcat (lambda (msg)
                   (concat (caar (plist-get msg :from))
                           ": "
                           (plist-get msg :subject)))
                 msgs
                 "\n")))

  (add-hook 'mu4e-index-updated-hook
            (lambda ()
              (let ((msg (newest-subject)))
                (unless (string-equal ": " msg)

                  (if (memq window-system '(mac ns))
                      (shell-command
                       (concat "terminal-notifier -title \"mu4e\" -sender \"org.gnu.Emacs\" -message "
                               (quote-shell-string msg)))
                    (start-process "notify-send"
                                   "*notify-send*"
                                   "notify-send"
                                   "mu4e"
                                   (quote-shell-string (format "%s" msg))))))))

;;;; Additional SMTP Accounts
  ;; From http://varunbpatil.github.io/2013/08/19/eom/#.VQtWSFyCZSU
  (defvar my-mu4e-account-alist
    '(("gmail"
       (user-mail-address "acowley@gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@gmail.com")
       (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "acowley@gmail.com" nil)))
       (mu4e-sent-folder "/gmail/sent")
       (smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "acowley@gmail.com" nil)))
       ;; (smtpmail-auth-supported (login))
       )
      ("upenn"
       (user-mail-address "acowley@seas.upenn.edu")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@seas.upenn.edu")
       (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "acowley@seas.upenn.edu" nil)))
       (mu4e-sent-folder "/seas/sent")
       (smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "acowley@seas.upenn.edu" nil))))
      ("sa"
       (user-mail-address "acowley@scalableautonomy.com")
       (smtpmail-default-smtp-server "mail.privateemail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-user "acowley@scalableautonomy.com")
       (smtpmail-starttls-credentials '(("mail.privateemail.com" 587 "acowley@scalableautonomy.com" nil)))
       (smtpmail-auth-credentials
        '(("mail.privateemail.com" 587 "acowley@scalableautonomy.com" nil))))

      ;; ("upenn"
      ;;  (user-mail-address "acowley@seas.upenn.edu")
      ;;  (smtpmail-default-smtp-server "smtp.seas.upenn.edu")
      ;;  (smtpmail-smtp-server "smtp.seas.upenn.edu")
      ;;                                   ;(smtpmail-smtp-service 578)
      ;;  (smtpmail-smtp-service 465)
      ;;  (smtpmail-stream-type ssl)
      ;;  (smtpmail-auth-supported (login)))
      ))

  (defun my-ensure-list (x)
    "If the given value is a list, leave it alone. If it isn't,
cons it to nil."
    (if (listp x) x (cons x nil)))

  (defun my-first (f xs)
    "Returns the first element of the list for which the given
predicate returns true."
    (cond
     ((null xs) nil)
     ((funcall f (car xs)) (car xs))
     (t (my-first f (cdr xs)))))

  (defun my-mem-string (x xs)
    "memq using 'string-equal' for equality."
    (cond
     ((null xs) nil)
     ((string-equal x (car xs)) xs)
     (t (my-mem-string x (cdr xs)))))

  (defun my-mu4e-set-account ()
    "Set the account for sending a message"
    (let*
        ((recip-account
          (when mu4e-compose-parent-message
            (let*
                ((my-addresses (mapcar #'(lambda (account)
                                           (cons (car account)
                                                 (cadr (assoc 'user-mail-address
                                                              (cdr account)))))
                                       my-mu4e-account-alist))
                 (recipients (append (my-ensure-list
                                      (plist-get mu4e-compose-parent-message :to))
                                     (plist-get mu4e-compose-parent-message :cc)))
                 (all-addresses (mapcar #'(lambda (var)
                                            (if (consp var) (cdr var) var))
                                        recipients))
                 (my-address (my-first #'(lambda (x)
                                           (my-mem-string (cdr x) all-addresses))
                                       my-addresses)))
              (when my-address (car my-address)))))
         (account
          (if recip-account
              recip-account
            (completing-read
             (format "Compose with account: (%s) "
                     (mapconcat #'(lambda (var) (car var))
                                my-mu4e-account-alist "/"))
             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var) (set (car var) (cadr var))) account-vars)
        (error "No email account found"))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  (setq mu4e-user-mail-address-list
        (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
                my-mu4e-account-alist))
  ;; (global-mu4e-conversation-mode)
)
(use-package mu4e-conversation
  :after mu4e
  :disabled
  :config
  (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)
  (global-mu4e-conversation-mode)

;;     :defer t
;;     ;; :disabled
;;     :commands global-mu4e-conversation-mode
;;     ;; :custom-face
;;     ;; (mu4e-conversation-sender-1 ((t (:foreground "SandyBrown"))))
;;     ;; (mu4e-conversation-sender-2 ((t (:foreground "DeepSkyBlue"))))
;;     ;; (mu4e-conversation-sender-3 ((t (:foreground "LightSalmon"))))
;;     ;; (mu4e-conversation-sender-4 ((t (:foreground "DarkKhaki"))))

;;     :config
;; (defun my/mu4e-conversation-hook ()
;;   (unless (eq major-mode 'org-mode)
;;     (mu4e-conversation-toggle-view))
;;   ;; (olivetti-mode 1)
;;   )

;; (defun my/mu4e-conversation-after (thread &optional print-function)
;; "Expand all trees, but collapse all drawers (e.g. PROPERTIES)."
;;   (outline-show-all)
;;   (run-hook-with-args 'org-cycle-hook 'all))

;; (setq mu4e-conversation-hook #'my/mu4e-conversation-hook)
;; (advice-add #'mu4e-conversation--print :after #'my/mu4e-conversation-after)

;; (defun contextual-time (ts-msg)
;; "Format a date-time string emphasizing information relative to
;; the curren time. If the date of the given time stamp is today,
;; then only the time is included. If it is another day of this
;; year, only the month and day are included. If it is another year,
;; the year, month, and day are included."
;;   (let ((ts-now (current-time)))
;;     (pcase (cons (decode-time ts-msg) (decode-time ts-now))
;;       (`((,_ ,minute ,hour ,day ,month ,year . ,_) . (,_ ,_ ,_ ,today ,this-month ,this-year . ,_))
;;        (format-time-string
;;         (if (= this-year year)
;;             (if (and (= this-month month) (= today day))
;;                 "%l:%M %p"
;;               "%b %e")
;;           "%F")
;;         ts-msg))
;;       (_ "unparsed date"))))

;; (defun strip-email-address (sender)
;;   "Remove the email address part of an email sender string,
;; leaving only the sender's name."
;;   (let ((pre-address (string-match " <" sender)))
;;     (if (and pre-address (> pre-address 0))
;;         (substring sender 0 pre-address)
;;       sender)))

;; (defun mu4e-conversation-print-tree (index thread-content thread-headers)
;;   "Insert Org-formatted message found at INDEX in THREAD-CONTENT."
;;   (let* ((msg (nth index thread-content))
;;          (msg-header (nth index thread-headers))
;;          (level (plist-get (mu4e-message-field msg-header :thread) :level))
;;          (org-level (make-string (1+ level) ?*))
;;          body-start)
;;     ;; Header.
;;     (insert (format "%s %s%s, %s %s\n"
;;                     org-level
;;                     (if (memq 'unread (mu4e-message-field msg :flags))
;;                         "UNREAD "
;;                       "")
;;                     (strip-email-address (mu4e-conversation--from-name msg))
;;                     (contextual-time (mu4e-message-field msg :date))
;;                     (mu4e-message-field msg :flags)))
;;     ;; Body
;;     (goto-char (point-max))
;;     (setq body-start (point))
;;     (insert (mu4e-message-body-text msg))
;;     ;; Turn shr-url into Org links.
;;     (goto-char body-start)
;;     (let (begin end url text)
;;       (while (and (not (eobp))
;;                   (setq begin (next-single-char-property-change (point) 'shr-url))
;;                   (get-text-property begin 'shr-url))
;;         (goto-char begin)
;;         (setq url (get-text-property (point) 'shr-url)
;;               end (next-single-char-property-change (point) 'shr-url)
;;               text (buffer-substring-no-properties begin end))
;;         (delete-region begin end)
;;         (insert (format "[[%s][%s]]" url text))))
;;     ;; Prefix "*" at the beginning of lines with a space to prevent them
;;     ;; from being interpreted as Org sections.
;;     (goto-char body-start)
;;     (while (re-search-forward (rx line-start "*") nil t) (replace-match " *"))
;;     (goto-char body-start)
;;     (if mu4e-conversation--use-org-quote-blocks
;;         (mu4e-conversation--format-org-quote-blocks body-start)
;;       (while (re-search-forward (rx line-start ">" (?  blank)) nil t) (replace-match ": ")))
;;     (goto-char body-start)
;;     (while (re-search-forward (concat "^" message-mark-insert-begin) nil t)
;;       (replace-match "#+begin_src
;; "))
;;     (goto-char body-start)
;;     (while (re-search-forward (concat "^" message-mark-insert-end) nil t)
;;       (replace-match "#+end_src
;; "))
;;     (goto-char (point-max))
;;     (org-set-property "Maildir" (mu4e-message-field msg :maildir))
;;     (org-set-property "To" (mu4e-conversation--format-address-list
;;                             (mu4e-message-field msg :to)))
;;     (when (mu4e-message-field msg :cc)
;;       (org-set-property "CC" (mu4e-conversation--format-address-list
;;                               (mu4e-message-field msg :cc))))
;;     (let ((attachments (mu4e~view-construct-attachments-header msg)))
;;       ;; TODO: Propertize attachments.
;;       (when attachments
;;         (org-set-property "Attachments" (replace-regexp-in-string "\n$" "" attachments)))
;;       (when (and (< (length (mu4e-message-field msg :to)) 2)
;;                  (not (mu4e-message-field msg :cc))
;;                  (not attachments))
;;         (save-excursion
;;           (goto-char (car (org-get-property-block)))
;;           (forward-line -1)
;;           (org-cycle))))))
)

;;; minions
(use-package minions
  :disabled
  :config (minions-mode 1))

;;; moody
(use-package moody
  :disabled
  :config
  (defun my-moody-wrap (string &optional width direction type)
    "A copy of moody-wrap that colors the left slant of the
element based on the god-local-mode predicate."
    (unless type
      (setq type 'tab))
    (unless direction
      (setq direction 'down))
    (let* ((base  (if (moody-window-active-p)
                      'mode-line
                    'mode-line-inactive))
           (outer (face-attribute base :background))
           (line  (face-attribute base :underline))
           (line  (if (eq line 'unspecified) outer line))
           (inner (if (eq type 'ribbon)
                      (face-attribute base :underline)
                    (face-attribute 'default :background)))
           (slant (if (eq direction 'down)
                      (list outer line inner)
                    (list inner line outer)))
           (god-bg (if (and (moody-window-active-p)
                            (bound-and-true-p god-local-mode))
                       "#dddd00"
                     (face-attribute 'mode-line :background)))
           (face-left (if (eq direction 'down)
                          (list :overline (and (eq type 'ribbon) line)
                                :underline god-bg
                                :foreground god-bg
                                :background god-bg)
                        (list :overline line
                              :underline (and (or (eq type 'ribbon)
                                                  (not (window-at-side-p nil 'bottom)))
                                              line)
                              :background inner)))
           (face  (if (eq direction 'down)
                      (list :overline (and (eq type 'ribbon) line)
                            :underline line
                            :background inner)
                    (list :overline line
                          :underline (and (or (eq type 'ribbon)
                                              (not (window-at-side-p nil 'bottom)))
                                          line)
                          :background inner)))
           (pad   (max (- (or width 0) (length string)) 2)))
      (setq string
            (concat (make-string (ceiling pad 2) ?\s)
                    (substring string 0)
                    (make-string (floor pad 2) ?\s)))
      (add-face-text-property 0 (length string) face nil string)
      (list
       (propertize " " 'face face-left 'display
                   (apply moody-slant-function
                          (if (eq direction 'down) 'down 'up)
                          (list god-bg god-bg inner)))
       string
       (propertize " " 'face face 'display
                   (apply moody-slant-function
                          (pcase (list type direction)
                            (`(tab    down) (cons 'up   slant))
                            (`(tab    up)   (cons 'down slant))
                            (`(ribbon down) (cons 'down (reverse slant)))
                            (`(ribbon up)   (cons 'up   (reverse slant)))))))))

  (defvar god-moody-buffer-identification
    '(:eval
      (my-moody-wrap
       (format-mode-line (propertized-buffer-identification "%b"))
       20 'down 'tab)))

  (put 'god-moody-buffer-identification 'risky-local-variable t)

  (defvar god-modified-mode-line
    '(:eval
      (let* ((string (if (buffer-modified-p) " * " " - "))
             (god-face (if (and (moody-window-active-p)
                                (bound-and-true-p god-local-mode))
                           '(:background "#dddd00" :foreground "#373737")
                         `(:background ,(face-attribute 'mode-line :background)
                                       :foreground ,(face-attribute 'mode-line :foreground)))))
        (list (propertize string 'face god-face))))
    "Mode line format for god-mode and buffer-modified status")

  (put 'god-modified-mode-line 'risky-local-variable t)

  (defun moody-god-replace-mode-line-buffer-identification (&optional reverse)
    (interactive "P")
    (moody-replace-element 'mode-line-buffer-identification
                           'god-moody-buffer-identification
                           reverse))

  (defun moody-god-buffer-modified (&optional reverse)
    (interactive "P")
    (moody-replace-element 'mode-line-modified
                           'god-modified-mode-line
                           reverse))

  ;; ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification moody-mode-line-buffer-identification "   " mode-line-position
  ;;  (vc-mode moody-vc-mode)
  ;;  "  " minions-mode-line-modes mode-line-misc-info mode-line-end-spaces)
  (moody-god-buffer-modified)
  (setq x-underline-at-descent-line t)
  (moody-god-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'truncate-all))
;;; spaceline
(use-package spaceline
  :disabled
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (diminish 'outline-minor-mode)
  (diminish 'projectile-mode)
  (diminish 'helm-mode)
  (diminish 'abbrev-mode)
  (diminish 'buffer-face-mode)
  (diminish 'flyspell-mode)
  (diminish 'auto-revert-mode)
  (diminish 'text-scale-mode)
  (diminish 'flycheck-mode)
  (diminish 'company-mode)
  (diminish 'yas-minor-mode)
  (diminish 'paredit-mode)
  (diminish 'eldoc-mode)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-erc-track-off)

  (defun ac/spaceline-highlight-face-god-state ()
    (if (bound-and-true-p god-local-mode)
        'spaceline-evil-normal          ;'spaceline-evil-visual
      'spaceline-evil-emacs             ;'spaceline-evil-insert
      ))

  ;; spaceline-evil-state-faces is a variable defined in ‘spaceline.el’.
  ;; Its value is ((normal . spaceline-evil-normal)
  ;;  (insert . spaceline-evil-insert)
  ;;  (emacs . spaceline-evil-emacs)
  ;;  (replace . spaceline-evil-replace)
  ;;  (visual . spaceline-evil-visual)
  ;;  (motion . spaceline-evil-motion))
                                        ;(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  ;; (setq spaceline-highlight-face-func #'spaceline-highlight-face-default)
  (setq spaceline-highlight-face-func #'ac/spaceline-highlight-face-god-state)
  )

;;; smart-mode-line (powerline)
;; (use-package smart-mode-line
;;   :config
;;   (use-package smart-mode-line-powerline-theme)
;;   (setq sml/no-confirm-load-theme t)
;;   (sml/setup)

;;   ;; Replace ":Doc:Projects/Foo/blah.hs" with ":Foo:blah.hs"
;;   (add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/\\([^/]*\\)/" ":\\1:") t)

;;   ;(sml/apply-theme 'smart-mode-line-powerline)
;;   (sml/apply-theme 'dark)

;;   ;; Don't show common minor modes
;;   (setq rm-blacklist (mapconcat 'identity '(" Fly" " company" " God" " Helm" " Outl" " ARev" " BufFace" " Wrap" "+1" "Projectile.*" "Abbrev") "\\|"))
;;   (setq sml/mode-width 'full)
;;   ;; (setq sml/mode-width 0)
;;   ;; (setq sml/shorten-modes nil)
;;   ;; (setq sml/name-width '(0 . 44))
;;   (setq sml/name-width '(0 . 25)))

;;; Multiple-cursors

;; multiple-cursors setup
(use-package multiple-cursors
  :commands (multiple-cursors-mode)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (add-hook 'prog-mode-hook #'multiple-cursors-mode)
  :config
  (setq ;mc/always-repeat-command t
        ;mc/always-run-for-all t
        mc/cmds-to-run-once '(god-mode-all))
  (defun ac/insert-numbers1 ()
    "Insert a number at each cursor counting up from 1."
    (interactive)
    (mc/insert-numbers 1)))

;;; Buffer-move

(use-package buffer-move
  :bind (("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)
         ("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)))

;; (require 'buffer-move)
;; (global-set-key (kbd "<C-S-left>") 'buf-move-left)
;; (global-set-key (kbd "<C-S-right>") 'buf-move-right)
;; (global-set-key (kbd "<C-S-up>") 'buf-move-up)
;; (global-set-key (kbd "<C-S-down>") 'buf-move-down)

;;; flycheck
(use-package flycheck
  :bind (:map flycheck-mode-map
         ("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)
         ;; ("M-?" . flycheck-display-error-at-point)
         )
  :config
  (use-package flycheck-color-mode-line
    :commands flycheck-color-mode-line-mode
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
    :config
    ;; (face-spec-set
    ;;  'flycheck-color-mode-line-error-face
    ;;  `((t :background
    ;;       ,(face-attribute 'flycheck-fringe-error :foreground nil 'default)))
    ;;  'face-defface-spec)
    ;; (face-spec-set
    ;;  'flycheck-color-mode-line-warning-face
    ;;  `((t :background
    ;;       ,(face-attribute 'flycheck-fringe-warning :foreground nil 'default)))
    ;;  'face-defface-spec)
    (setq flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
    ;; (setq flycheck-color-mode-line-face-to-color 'mode-line)
    )
  (setq-default mode-line-format
                (insert-after 'mode-line-position
                              'flycheck-mode-line
                              mode-line-format)))
;;; nix

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (require 'nix-update "~/src/nix-update-el/nix-update.el"))

(defun find-nix-shell ()
  "Search for the first shell.nix file to be found in the same directory as the current file or all ancestor directories."
  (let ((fname (or load-file-name buffer-file-name)))
    (when (not (null fname))
      (let ((dir (file-name-directory fname)))
        (while (not (or (file-exists-p (concat dir "shell.nix"))
                        (string-equal dir "/")))
          (setq dir (file-name-directory (directory-file-name dir))))
        (if (string-equal dir "/")
            (error "Couldn't find a shell.nix")
          (concat dir "shell.nix"))))))
;;; haskell

;; Using a source checkout of ghc-mod
; (add-to-list 'load-path "~/src/ghc-mod/elisp/")

(use-package haskell-mode
  :mode ("\\.\\(hs\\|lhs\\)\\'" ("cabal" . haskell-cabal-mode))
  :init
  (setq inferior-haskell-find-project-root nil
        haskell-process-type 'cabal-repl
        ;; haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fno-ghci-sandbox")
        )
  :bind (:map haskell-mode-map
         ("C-x C-d" . nil)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-l" . haskell-process-load-file)
         ("C-c C-b" . haskell-interactive-switch)
         ("C-c C-n C-t" . haskell-process-do-type)
         ("C-c C-n C-i" . haskell-process-do-info)
         ("C-c C-n C-c" . haskell-process-cabal-build)
         ("C-c C-n c" . haskell-process-cabal)
         ("C-c M-." . nil)
         ("C-c C-d" . nil))
  :config
  (setq haskell-hoogle-command "hoogle")
  ;; (electric-indent-local-mode -1)
  (use-package haskell-doc
    :commands (haskell-doc-current-info))
  (set-face-attribute 'haskell-keyword-face nil :slant 'italic)
  (company-mode)
  (use-package shm
    :disabled
    :bind (("M-A" . shm/goto-parent-end))
    :config
    ;; Appropriate for light themes
    ;; (set-face-background 'shm-current-face "#eee8d5")
    ;; (set-face-background 'shm-quarantine-face "lemonchiffon")

    ;; For dark themes
    (set-face-background 'shm-current-face "#303030")
    (set-face-background 'shm-quarantine-face "#550505")
    )
  (use-package intero
    :disabled
    ;; :load-path "~/src/intero/elisp"
    :commands (intero-mode intero-global-mode)
    :bind (("M-n" . flycheck-next-error)
           ("M-p" . flycheck-previous-error)
           ;; :map intero-mode-map
           ;; ("C-c [\\C-?\t]" . haskell-indentation-indent-line)
           ;; ("M-?" . flycheck-display-error-at-point)
           )
    :config

    (define-key intero-mode-map (kbd "C-c <C-tab>") #'haskell-indentation-indent-line)
    (defun my-intero-repl-hook ()
      (company-mode -1))
    (add-hook 'intero-repl-mode-hook #'my-intero-repl-hook)
    (setq intero-whitelist (append (mapcar (lambda (p) (concat "~/Documents/Projects/" p))
                                           '("VinylRecords"
                                             "concurrent-machines"
                                             "ffmpeg-light"
                                             "GLUtil"
                                             "CLUtil"
                                             "llvm-hs"
                                             "llvm-hs-pure"
                                             "HoclSuite"
                                             "yaml-light-lens"
                                             "hpp"))
                                   (mapcar (lambda (p) (concat "~/Projects/" p))
                                           '("MotionCT" "hpp" "Frames" "Vinyl"))
                                   )))
  ;; (use-package hindent)

  (use-package lsp-haskell
    ;; :defer t
    ;; :commands lsp-haskell-enable
    :config
    ;; (setq lsp-haskell-process-path-hie "hie")
    ;; (setq lsp-haskell-process-path-hie "ghcide")
    (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
    (setq lsp-haskell-process-args-hie '())
    (setq lsp-haskell-formatting-provider "brittany")
    )

  (defun my-haskell-mode-hook ()
    ;; (structured-haskell-mode)
    ;; (electric-indent-local-mode -1)
    ;; (electric-pair-local-mode -1)
    ;; (electric-quote-local-mode -1)
    ;; (intero-global-mode)
    )
  (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)
  (defun haskell-find-pragmas ()
    "Return a sorted list of Haskell language pragmas specified
in the buffer. Leaves point after the last language pragma."
    (goto-char (point-min))
    (let ((pragmas nil))
      (while (search-forward-regexp
              (rx line-start "{-#" (1+ space)
                  (or "LANGUAGE" "language")
                  (1+ space))
              (point-max) t)
        (search-forward-regexp (rx (group
                                    (zero-or-more
                                     (one-or-more alnum)
                                     (zero-or-one ?\,)
                                     (zero-or-more (or space ?\n))))
                                   "#-}")
                               (point-max)
                               t)
        (setq pragmas (append pragmas
                              (mapcar #'string-trim
                                      (split-string (match-string 1) ",")))))
      (sort pragmas #'string-lessp)))

  (defun haskell-cleanup-pragmas ()
    "Merge all GHC LANGUAGE pragmas into a single alphabetically
sorted block."
    (interactive)
    (save-excursion
      (let ((pragmas (haskell-find-pragmas)))
        (delete-char (-(1- (point))))
        (insert (fill-list pragmas ", " "{-# LANGUAGE " " #-}"))))))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :config
  (defun my-dante-hook ()
    (intero-mode -1)
    (flycheck-mode)
    ;(flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
    )
  (add-hook 'dante-mode-hook #'my-dante-hook))

;; (add-to-list 'load-path "~/.emacs.d/misc")
;; (add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
;; (autoload 'haskell-latex-mode "haskell-latex")


;;; Language Server Protocol (LSP)
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c C-l")
  (lsp-configure-hook nil)

  ;; The default `lv-message' tends to leave stale windows down in the
  ;; minibuffer area for me.
  (lsp-signature-function #'message)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil)
  :bind (:map lsp-mode-map
              ("C-c C-l C-= C-=" . lsp-format-buffer)
              ("C-c C-l C-f" . lsp-format-buffer))
  :custom-face
  ;; Make the symbol-at-point highlight a bit dimmer than the default
  ;; (lsp-face-highlight-textual ((t (:background "#757500"))))
  (lsp-face-highlight-textual ((t (:background "gold4"))))
  :config
  (setq lsp-prefer-flymake nil
        ;; lsp-prefer-capf t
        read-process-output-max (* 1024 1024))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$")
  ;; (use-package company-lsp
  ;;   :config
  ;;   (setq company-lsp-enable-snippet t)
  ;;   (add-to-list 'company-backends 'company-lsp))
  (use-package lsp-ui
    :commands lsp-ui-mode
    :bind (:map lsp-ui-mode-map
           ("C-c C-s" . lsp-ui-sideline-toggle-symbols-info)
           ("M-." . lsp-ui-peek-find-definitions)
           ("M-?" . lsp-ui-peek-find-references)
           ("C-c C-l s a" . lsp-ui-sideline-apply-code-actions)
           ("C-c C-l C-s C-a" . lsp-ui-sideline-apply-code-actions)
           ("C-c C-l C-a" . lsp-ui-sideline-apply-code-actions)
           :map lsp-ui-peek-mode-map
           ("M-n" . lsp-ui-peek--select-next)
           ("M-p" . lsp-ui-peek--select-prev))
    :config
    (setq lsp-ui-sideline-delay 0.2
          lsp-ui-sideline-ignore-duplicate t
          ;; The child frame disappears almost immediately for me
          ;; lsp-ui-doc-use-childframe nil
          lsp-ui-doc-position 'top

          lsp-ui-doc-include-signature t

          ;; Without this, the search term is not highlighted in the
          ;; peek popup
          lsp-ui-peek-fontify 'always
          )

    ;; Make the lsp-ui-peek overlay stand out a bit better
    (set-face-background 'lsp-ui-peek-peek "dark slate gray")

    ;; Highlight the symbol we're looking for in yellow
    (set-face-foreground 'lsp-ui-peek-highlight "yellow"))
  (use-package lsp-treemacs
    :commands (lsp-treemacs-symbols
               lsp-treemacs-errors-list
               lsp-treemacs-references
               lsp-treemacs-implementations
               lsp-treemacs-call-hierarchy
               lsp-treemacs-type-hierarchy))
  (require 'lsp-diagnostics)
  (require 'lsp-completion)
  (require 'lsp-ui)
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'lsp-diagnostics-mode)
  (add-hook 'lsp-mode-hook 'lsp-completion-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (helm-gtags-mode -1)
  (setq company-lsp-cache-candidates nil
        company-transformers nil
        company-lsp-async t
        lsp-enable-indentation nil)
  (flymake-mode -1)
  (defun lsp-format-defun ()
    "Reformat the current function."
    (interactive)
    (save-mark-and-excursion
      (mark-defun)
      (call-interactively 'lsp-format-region))))

;;; ccls
(use-package ccls
  :defer t
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  (bind-key "C-c C-n"
            (lambda () (interactive) (ccls-navigate "D")) lsp-ui-mode-map)
  (bind-key "C-c C-p"
            (lambda () (interactive) (ccls-navigate "U")) lsp-ui-mode-map)
  (bind-key "C-c C-b"
            (lambda () (interactive) (ccls-navigate "L")) lsp-ui-mode-map)
  (bind-key "C-c C-f"
            (lambda () (interactive) (ccls-navigate "R")) lsp-ui-mode-map)

  (setq ccls-sem-highlight-method 'overlay)
  ;; (defun project-ccls ()
  ;;   (flycheck-mode)
  ;;   (yas-minor-mode)
  ;;   (helm-gtags-mode -1)
  ;;   ;; (setq ccls-extra-args '("--log-file=/tmp/cc.log"))
  ;;   (setq company-lsp-cache-candidates nil
  ;;         company-transformers nil
  ;;         company-lsp-async t)
  ;;   (setq-local ccls-executable
  ;;             (let ((nix-shell (concat
  ;;                               (locate-dominating-file (or load-file-name
  ;;                                                           buffer-file-name)
  ;;                                                       "shell.nix")
  ;;                                     "shell.nix")))
  ;;               (string-trim
  ;;                (shell-command-to-string
  ;;                 (concat
  ;;                  "nix-shell " nix-shell " --run 'which ccls'"))))))
  ;; (advice-add 'lsp-ccls-enable :before #'project-ccls)
  )

(defun lsp-ccls ()
  "Enable LSP with the CCLS backend."
  (interactive)
  (require 'ccls)
  (require 'lsp-ui-flycheck)
  (flycheck-mode)
  (yas-minor-mode)
  (helm-gtags-mode -1)
  ;(setq ccls-extra-args '("--log-file=/tmp/cc.log"))
  (when (file-exists-p "/etc/lsb-release")
    (setq ccls-executable "/home/acowley/src/ccls/Release/ccls"))
  (setq company-transformers nil
        ccls-args '("-v=3" "-log-file=/tmp/cc.log")
        lsp-enable-indentation nil)
  (lsp)
  (lsp-completion-mode)
  (flymake-mode -1))

(defun docker-ccls ()
  (require 'lsp-mode)
  (require 'ccls)
  ;; (setq lsp-session-file "/home/acowley/.emacs.d/.lsp-session-v1")
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection '("/home/acowley/src/ccls/Release/ccls" "-v=3" "-log-file=/tmp/cc.log"))
    :major-modes '(c-mode c++-mode cuda-mode)
    :remote? t
    :server-id 'rocm-ros-3.8
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :multi-root nil
    :library-folders-fn nil
    :initialization-options (lambda () ccls-initialization-options))))

;;; cquery
;; (use-package cquery
;;   :load-path "~/Projects/emacs-cquery"
;;   :commands lsp-cquery-enable
;;   :init
;;   (setq cquery-sem-highlight-method 'overlay)
;;   ;; (setq cquery-sem-highlight-method 'font-lock)
;;   ;; (setq cquery-sem-highlight-method nil)
;;   (setq-local cquery-extra-init-params
;;               '(:indexBlacklist '("GPATH" "GRTAGS" "GTAGS")
;;                                 :cacheFormat "msgpack"))
;;   :config
;;   (setq xref-prompt-for-identifier (append xref-prompt-for-identifier '(xref-find-references))))

(defun in-docker-p ()
  "Returns a non-nil value if we are running in a docker container"
  (eq (call-process-shell-command "grep -q docker /proc/1/cgroup") 0))

;; (defun cquery-mode ()
;;   "Start all cquery-related modes"
;;   (interactive)
;;   (when (let ((ext (file-name-extension (or (buffer-file-name) ""))))
;;           (and (not (null ext))
;;                (or (string-equal ext "cpp")
;;                    (string-equal ext "cc")
;;                    (string-equal ext "hpp"))))

;;     (flycheck-mode)
;;     (lsp-cquery-enable)
;;     (yas-minor-mode)
;;     (helm-gtags-mode -1)
;;     (local-set-key (kbd "M-.") #'xref-find-definitions)))

;; (defun cquery-nix-shell ()
;;   "Find a cquery executable in a nix-shell associated with the
;; directory containig the current file if that file’s extension is
;; `cpp` or `hpp`. Use the location of that executable in the nix
;; store to load and configure the cquery lsp client."
;;   (when (let ((ext (file-name-extension (or (buffer-file-name) ""))))
;;           (and (not (null ext))
;;                (or (string-equal ext "cpp")
;;                    (string-equal ext "cc")
;;                    (string-equal ext "hpp"))))
;;     (if (in-docker-p)
;;         (progn
;;           (message "Using locally-built cquery in docker container")
;;           (setq-local cquery-executable "/home/acowley/src/cquery/docker-build/cquery"))
;;       (let ((nix-shell
;;            (concat (locate-dominating-file (or load-file-name buffer-file-name)
;;                                            "shell.nix")
;;                    "shell.nix")))
;;       (when nix-shell
;;         (let* ((exes
;;                 (split-string
;;                  (string-trim
;;                   (shell-command-to-string
;;                    (concat "nix-shell " nix-shell
;;                            " --run 'which cquery; which clang-format'")))
;;                  "\n" t))
;;                (cquery-exe (car exes))
;;                (clang-format-exe (cadr exes))
;;                (cquery-root (file-name-directory
;;                              (directory-file-name
;;                               (file-name-directory cquery-exe)))))
;;           (message "cquery-root: %s" cquery-root)
;;           ;; (require 'cquery)
;;           (setq clang-format-executable clang-format-exe)
;;           (setq-local cquery-executable cquery-exe)))) )


;;     ;; General setup

;;     ;; (setq cquery-extra-args '("--log-all-to-stderr" "--log-file" "cquery.log"))
;;     (setq cquery-extra-args '("--log-all-to-stderr"))
;;     ;; (flycheck-mode)
;;     ;; (lsp-cquery-enable)
;;     ;; (yas-minor-mode)
;;     ;; (helm-gtags-mode -1)
;;     ;; (diminish 'company-mode)
;;     ;; (diminish 'flyspell-mode)
;;     ;; (local-set-key (kbd "M-.") #'xref-find-definitions)
;;     (cquery-mode)))

;;; C++

;; I get this from melpa because the nix package ends up wanting to
;; build cmake itself.
(use-package cmake-mode
  ;; :ensure t
  :commands (cmake-mode)
  :mode "CMakeLists.txt\\'")

(use-package helm-gtags
  :defer t
  ;; :diminish helm-gtags-mode
  :commands (helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
         ("M-." . helm-gtags-find-tag)
         ("M-*" . helm-gtags-pop-stack))
  :config
  (setq helm-gtags-direct-helm-completing t
        helm-gtags-auto-update t
        helm-gtags-ignore-case t))

(use-package clang-format
  :commands (clang-format-buffer clang-format-region))

;; From https://stackoverflow.com/a/21656063/277078
(defun my/merged-imenu ()
  "Use both `imenu-default-create-index-function' and `imenu-generic-expression' for generating an imenu list"
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(defun my/c++-mode-hook ()
  (electric-indent-mode t)
  (electric-pair-mode t)
  (setq company-backends (delete 'company-dabbrev (delete 'company-clang (delete 'company-semantic company-backends))))
  (which-function-mode)
  (set-face-foreground 'which-func "LightSkyBlue")
  (yas-minor-mode-on)
  (c-toggle-auto-newline -1)
  ;; Make imenu work with OpenCL, CUDA, and HIP compute kernels
  (add-to-list 'imenu-generic-expression
               `("Kernels"
                 ,(concat
                   ;"^[a-zA-Z0-9_]+[ \t]?"		; Type specs; there can be no
                   (rx (or "__global__" "__device__" "__host__" "__kernel") space)
                   "\\([a-zA-Z0-9_*]+[ \t]+\\)"	; more than 3 tokens, right?
                   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
                   "\\([*&]+[ \t]*\\)?"			; Pointer.
                   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; Name.
                   ) 4))
  (setq imenu-create-index-function #'my/merged-imenu)
  (helm-gtags-mode 1))

(add-hook 'c++-mode-hook #'my/c++-mode-hook)
(add-hook 'c-mode-hook #'my/c++-mode-hook)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

(use-package opencl-mode 
  :mode "\\.cl\\'"
  :config
  (add-hook 'opencl-mode-hook #'electric-indent-local-mode))

;;; mixed-pitch
(use-package mixed-pitch
  :commands (mixed-pitch-mode)
  ;; :diminish mixed-pitch-mode
  :init
  ;; If you want it in all text modes:
  (add-hook 'text-mode-hook #'mixed-pitch-mode)
  (add-hook 'org-mode-hook #'mixed-pitch-mode)
  :config
  ;; Depending on your specific setup, you may want to adjust the height of
  ;; variable pitch fonts:
  (set-face-attribute 'variable-pitch nil
                      :height (if (memq window-system '(mac ns)) 170 140)
                      :family "Montserrat"
                      :weight (if (memq window-system '(mac ns)) 'normal 'light))


  ;;(set-face-attribute 'variable-pitch nil :height 100 :family "Libre Baskerville")

  )

;;; python
(defun ac/python-hook ()
  (setq python-indent-offset 2))

(add-hook 'python-mode-hook #'ac/python-hook)

;;; File mode associtions

;; OpenCL code uses c-mode
;; (add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.machine\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-mode))

;;; imaxima
;; imaxima installation
;; (autoload 'imaxima "imaxima" "Image support for Maxima." t)
;; (autoload 'imath-mode "imath" "Interactive Math minor mode." t)
(use-package maxima
  :load-path "~/.nix-profile/share/emacs/site-lisp"
  :mode (rx ".ma" (or ?c ?x) eos)
  :defer t
  :commands (maxima-mode imaxima maxima imath-mode)
  ;; :custom
  ;; (imaxima-latex-preamble "\usepackage{pagecolor}")
  :config
  (require 'imaxima)
  (setq imaxima-use-maxima-mode-flag t))

;;; QML mode
(use-package qml-mode
  :defer t
  :mode "\\.qml\\'")

;;; git
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(use-package magit
  :defer t
  :commands (magit-status)
  :config
  (add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil))



;;; gpg
(defun pinentry-emacs (desc prompt ok error)
  "Interface for entering a password into gpg-agent."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
;;; erc (IRC)
(use-package erc
  :defer t
  :config
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 18
        erc-fill-column 71)
  ;; (setq erc-fill-function 'erc-fill-variable)

  (use-package erc-terminal-notifier
    :if (memq window-system '(mac ns))
    :config
    (setq erc-terminal-notifier-command "~/.nix-profile/bin/terminal-notifier")
    (add-hook 'erc-mode-hook (lambda() (require 'erc-terminal-notifier))))
  (use-package ercn
    :if (not (memq window-system '(mac ns)))
    :config
    (require 's)
    (defun do-notify (nickname message)
      (let* ((channel (buffer-name))
             (title (if (string-match-p (concat "^" nickname) channel)
                        nickname
                      (concat nickname " (" channel ")")))
             (msg (s-trim (s-collapse-whitespace message))))
        (start-process "notify-send"
                       "*notify-send*"
                       "notify-send"
                       (concat "ERC - " title)
                       (quote-shell-string (format "%s" msg)))))
    (add-hook 'ercn-notify-hook #'do-notify))
  (use-package erc-hl-nicks
    :config
    (add-to-list 'erc-modules 'hl-nicks)))
;;; znc
(use-package znc
  :defer t
  :commands (znc-all znc-erc)
  :config
  (let ((password (let ((auth (auth-source-search :host "rasznc.local")))
                    (cond
                     ((null auth) (error "Couldn't find rasznc authinfo"))
                     (t (funcall (plist-get (car auth) :secret)))))))
    (set-variable
     'znc-servers
     `(("raspberrypi.local" 1234 t ((rasznc "acowley/freenode" ,password)))))))

;;; twittering-mode
(use-package twittering-mode
  :defer t
  :commands (twittering-mode twit)
  :config
  (add-hook 'twittering-mode-hook
            (lambda ()
              (variable-pitch-mode)
              (turn-on-visual-line-mode)
              (setq buffer-face-mode-face (if (memq window-system '(mac ns))
                                              '(:family "Avenir Next")
                                            '(:family "Cantarell")))
              (buffer-face-mode)
              (text-scale-adjust 1)))
  (add-hook 'twittering-edit-mode-hook 'flyspell-mode)

  (set-variable 'twittering-use-master-password t))

;;; corral
(use-package corral
  :defer t
  :bind (("M-9" . corral-parentheses-backward)
         ("M-0" . corral-parentheses-forward)
         ("M-\"" .  corral-double-quotes-backward)
         ("M-{" . corral-braces-backward)
         ("M-}" . corral-braces-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward))
  :config
  (setq corral-preserve-point t))
;;; rust
(use-package rustic
  :defer t
  :mode (("\\.rs\\'" . rustic-mode))
  :config
  (defun my/rustic-hook ()
    (electric-indent-mode 1)
    (yas-minor-mode))
  (setq rustic-analyzer-command '("rust-analyzer")
        lsp-rust-analyzer-server-command '("rust-analyzer"))
  (add-hook 'rustic-mode-hook #'my/rustic-hook))

(use-package rust-mode
  :disabled
  :defer t
  :commands (rust-mode)
  :mode "\\.rs\\'"
  :config
  (setq rust-indent-offset 2)
  (use-package cargo)
  ;; (use-package flycheck-rust
  ;;   :config
  ;;   (flycheck-rust-setup))
  ;; (use-package racer
  ;;   :commands (racer-mode)
  ;;   :bind (:map rust-mode-map
  ;;          ("TAB" . company-indent-or-complete-common))
  ;;   :config
  ;;   (defun my/racer-hook ()
  ;;     (eldoc-mode)
  ;;     (company-mode)
  ;;     (setq company-tooltip-align-annotations t))
  ;;   (add-hook 'racer-mode-hook #'eldoc-mode))
  (require 'lsp-mode)
  (require 'lsp-rust)
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-server-command '("rust-analyzer")
        lsp-rust-unstable-features t

        ;; If this feature is enabled, we get a lot of spurious
        ;; dependency crate rebuilds from both RA and cargo itself.
        ;; lsp-rust-analyzer-cargo-watch-enable nil
        )
  (defun my/rust-hook ()
    ;; (flycheck-rust-setup)
    ;; (flycheck-mode)
    ;; (racer-mode)
    )
  (add-hook 'rust-mode-hook #'my/rust-hook))

;;; purescript
(use-package purescript-mode
  :defer t
  :mode "\\.purs\\'"
  :config
  ;(use-package flycheck-purescript)
  ;; (add-hook 'purescript-mode-hook #'flycheck-mode)
  (use-package psc-ide
    :config
    (setq psc-ide-use-npm-bin t))
  (defun my/purescript-hook ()
    (psc-ide-mode)
    (company-mode t)
    (flycheck-mode t)
    (turn-on-purescript-indentation))
  (add-hook 'purescript-mode-hook #'my/purescript-hook))
;;; elisp
(use-package paredit
  :commands paredit-mode
  ;; :bind (:map paredit-mode-map
  ;;             ("M-s" . avy-goto-word-1))
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode))

;;; PlatformIO
;; From github user @pashky
(add-hook 'projectile-mode-hook
          (lambda ()
            (projectile-register-project-type 'platformio '("platformio.ini")
                                              :compile "platformio run"
                                              :run "platformio run -t upload")))

;;; yaml
(use-package yaml-mode
  :defer t
  :mode "\\.yaml\\'"
  :config
  ;; From https://github.com/yoshiki/yaml-mode/issues/25#issuecomment-250440342
  (add-hook 'yaml-mode-hook
            (lambda ()
              (outline-minor-mode)
              ;; (define-key yaml-mode-map (kbd "TAB") 'outline-toggle-children)
              (define-key yaml-mode-map (kbd "TAB") 'yaml-indent-line)
              ;(setq outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)")
              )))
;;; dictionary
(use-package osx-dictionary
  :if (memq window-system '(mac ns))
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (setq osx-dictionary-dictionary-choice '("Dictionary" "Thesaurus")))

;;;; dict
(use-package dict-lookup
  :if (not (memq window-system '(mac ns)))
  :load-path "~/Projects/dict-lookup"
  :bind (("C-c d" . dict-lookup-search-pointer))
  ;; :custom (dict-lookup-dictionary "dict-db-wordnet-542")
  )

;;; graphviz-dot-mode
(use-package graphviz-dot-mode
  :defer t
  :mode "\\.dot\\'")
;;; direnv
(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))
;;; dired-du
(use-package dired-du
  :commands (dired-du-mode))
;;; nix-buffer
(use-package nix-buffer
  :defer t
  :commands (nix-buffer))
;;; toml
(use-package toml-mode
  :defer t
  :mode "\\.toml\\'")
;;; markdown-mode
(use-package markdown-mode :defer t
  :mode "\\.md\\'"
  :config
  (set-face-attribute 'markdown-code-face nil :family "Victor Mono"))
;;; web-mode
(use-package web-mode
  :mode "\\.html\\'"
  :commands (web-mode))
;;; glsl
(use-package glsl-mode
  :mode "\\.\\(vert\\|frag\\|geom\\|comp\\)\\'")
;;; wgsl
(use-package wgsl-mode
  :load-path "~/Projects/wgsl-mode"
  :mode "\\.wgsl\\'")
;;; smartparens-mode
(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config))

;;; logview
(use-package logview
  :defer t
  :commands (logview-mode)
  :config
  (setq datetime-timezone 'UTC
        logview-additional-level-mappings '(("ICP"
                                             (error       "ERROR")
                                             (warning     "WARNING")
                                             (information "INFO")
                                             (debug       "DEBUG")
                                             (trace       "TRACE")))
        logview-additional-timestamp-formats '(("easyloggingpp" (java-pattern . "HH:mm:ss,SSS")))
        logview-additional-submodes '(("ICP" . ((format . "TIMESTAMP LEVEL ")
                                                (levels . "ICP")
                                                (timestamp . ("easyloggingpp")))))))
;;; ag
(use-package ag :defer t)
;;; ripgrep
(use-package ripgrep :defer t)
;;; xterm-color
(use-package xterm-color
  :commands xterm-color-filter
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook 'comint-preoutput-filter-functions
                        'xterm-color-filter nil t))))

;;; highlight-indent-guides
(use-package highlight-indent-guides
  :defer t
  :commands highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character))
;;; emacs-libvterm
(use-package vterm
  :defer t
  :commands (vterm vterm-other-window)
  :bind (:map vterm-mode-map
              ("<escape>" . nil)
              ("M-w" . nil)
              ("C-y" . nil)
              ("C-n" . nil)
              ("C-p" . nil)
              ("C-a" . nil)
              ("C-e" . nil)
              ("C-f" . nil)
              ("C-b" . nil)
              ("M-b" . nil)
              ("M-f" . nil)
              ("C-SPC" . nil)
              ("M->" . nil)
              ("M-s" . nil)))
;;; dhall-mode
(use-package dhall-mode
  :defer t
  :mode "\\.dhall\\'"
  :commands (dhall-mode))
;;; plantuml
(use-package plantuml-mode
  :defer t
  :mode (rx ".plantuml" eos)
  :commands (plantuml-mode))
;;; ESS (R)
(use-package ess-site :mode (("\\.[rR]\\'" . R-mode)) :commands R)
;;; docker
;; (use-package docker :defer t)
(use-package dockerfile-mode
  :defer t
  :mode "\\Dockerfile\\'")

(use-package docker-tramp
  :defer 1
  :config
  (eval-after-load 'tramp
    '(progn
       (docker-tramp-add-method)
       (tramp-set-completion-function
        docker-tramp-method
        docker-tramp-completion-function-alist))))

;;; emojify
(use-package emojify
  :defer t
  :commands (emojify-mode global-emojify-mode)
  :config
  (emojify-set-emoji-styles '(github unicode)))
;;; disk-usage
(use-package disk-usage
  :defer t
  :commands (disk-usage disk-usage-here disk-usage-dired-at-point))

;;; speed-type
(use-package speed-type :commands (speed-type-text))
;;; pomidor
(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1)
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          (set-window-buffer nil (current-buffer)))))
;;; synosaurus
(use-package synosaurus
  :commands (synosaurus-mode
             synosaurus-lookup
             synosaurus-choose-and-replace
             synosaurus-choose-and-insert)
  :config
  (require 'synosaurus-wordnet))
;;; ace-window
(use-package ace-window
  :defer t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;;; avy
(use-package avy
  :bind (("M-s" . avy-goto-word-1)
         ("M-S" . avy-goto-char-2)))
;;; proofgeneral
;; (use-package proof-general
;;   :mode (rx ".v" eos))
;;; literate-calc-mode
(use-package literate-calc-mode
  :commands (literate-calc-mode literate-calc-minor-mode)
  :config
  ; From https://www.reddit.com/r/emacs/comments/gn8bzo/literatecalcmode/fr9pf01?utm_source=share&utm_medium=web2x
  (with-eval-after-load 'calc
    (defalias 'calcFunc-uconv 'math-convert-units)
    (defalias 'calcFunc-usimp 'math-simplify-units)
    (defalias 'calcFunc-norm 'calc-abs)))
;;; dired-rsync
(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))
;;; restclient
(use-package restclient
  :commands (restclient-mode))

;;; auctex
(use-package auctex
  :mode (("\\.tex\\'" . TeX-latex-mode))
  :init (add-hook 'LaTeX-mode-hook (lambda ()
                                     (require 'auctex-latexmk)
                                     (auctex-latexmk-setup)))
  :custom
  (TeX-engine 'xetex)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (reftex-default-bibliography '("~/Documents/MyPapers/mybib/mybib.bib")))
;;; gif-screencast
(use-package gif-screencast
  :commands (gif-screencast))
;;; Private Configuration
;; Set up paths for org files, etc.
(when (file-exists-p "~/.emacsPrivate.el")
  (load "~/.emacsPrivate.el"))

;;; Mode-line cleanup
;; (setq mode-line-position
;;       '((line-number-mode ("%l" (column-number-mode ":%2c")))))

;; (setq-default mode-line-format
;;       (cl-reduce #'cl-remove
;;                  (list 'mode-line-front-space
;;                        'mode-line-mule-info
;;                        'mode-line-client
;;                        'mode-line-remote
;;                        'mode-line-frame-identification)
;;                  :initial-value mode-line-format
;;                  :from-end t))

;;; ediff
(use-package ediff
  :custom-face
  (ediff-current-diff-A ((t :background "dark red")))
  (ediff-fine-diff-A ((t :background "indian red")))
  (ediff-current-diff-B ((t (:background "dark goldenrod"))))
  (ediff-fine-diff-B ((t (:background "sienna"))))
  :commands (ediff ediff3))
;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212121" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"])
 '(beacon-color "#ec4780")
 '(column-number-mode t)
 '(company-begin-commands
   '(self-insert-command org-self-insert-command outshine-self-insert-command))
 '(company-ghc-autoscan t)
 '(company-ghc-show-info 'oneline)
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "d5cdb20cc31dfd701ee4ac5fed09d0e1898ee828c6036c4ee00fdc1e50eb7558" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "412c25cf35856e191cc2d7394eed3d0ff0f3ee90bacd8db1da23227cdff74ca2" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "0f92b9f1d391caf540ac746bc251ea00a55f29e20a411460eb6d8e49892ddef9" "d94eec01b45c7dc72e324af86fd2858e97c92220c195b5dbae5f8fd926a09cec" "1a53efc62256480d5632c057d9e726b2e64714d871e23e43816735e1b85c144c" "0f98f9c2f1241c3b6227af48dc96e708ec023dd68363edb5d36dc7beaad64c23" "13270e81a07dac4aeb1efefb77b9e61919bb3d69da7253ade632856eed65b8a2" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "0a1a7f64f8785ffbf5b5fbe8bca1ee1d9e1fb5e505ad9a0f184499fe6747c1af" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c810219104d8ff9b37e608e02bbc83c81e5c30036f53cab9fe9a2163a2404057" "d46b5a32439b319eb390f29ae1810d327a2b4ccb348f2018b94ff22f410cb5c4" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "2d20b505e401964bb6675832da2b7e59175143290dc0f187c63ca6aa4af6c6c1" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "d22a6696fd09294c7b1601cb2575d8e5e7271064453d6fa77ab4e05e5e503cee" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default))
 '(default-input-method "TeX")
 '(dired-dwim-target t)
 '(dired-recursive-deletes 'always)
 '(doc-view-resolution 200)
 '(doc-view-scale-internally nil)
 '(electric-pair-mode t)
 '(electric-quote-mode t)
 '(erc-hide-list '("JOIN" "PART" "QUIT"))
 '(erc-server-auto-reconnect nil)
 '(evil-emacs-state-cursor '("#E57373" hbar))
 '(evil-insert-state-cursor '("#E57373" bar))
 '(evil-normal-state-cursor '("#FFEE58" box))
 '(evil-visual-state-cursor '("#C5E1A5" box))
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_AGENT_INFO"))
 '(flycheck-ghc-args '("-Wall"))
 '(flycheck-swift-sdk-path
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
 '(flycheck-swift-target "x86_64-macosx10.11")
 '(ghc-doc-browser-function 'ghc-browse-url-safari)
 '(ghc-use-nix-shell ''t)
 '(global-eldoc-mode t)
 '(haskell-indent-offset 2)
 '(helm-mu-gnu-sed-program "gnused")
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#ed0547ad8099" . 0) ("#212121" . 100)))
 '(hl-sexp-background-color "#efebe9")
 '(magit-popup-use-prefix-argument 'default)
 '(magit-use-overlays nil)
 '(org-footnote-auto-label 'plain)
 '(org-html-validation-link "")
 '(org-imenu-depth 3)
 '(org-mobile-files '("~/org/home.org"))
 '(org-preview-latex-default-process 'imagemagick)
 '(org-reveal-root "reveal.js")
 '(org-src-preserve-indentation t)
 '(org-src-window-setup 'other-window)
 '(package-selected-packages
   '(znc yasnippet yaml-mode xterm-color wgrep-helm web-mode visual-fill-column use-package twittering-mode toml-mode synosaurus speed-type spaceline smartparens recentf-remove-sudo-tramp-prefix racer qml-mode purescript-mode psc-ide pomidor paredit ox-tufte ox-gfm ox-clip outshine osx-dictionary org-table-sticky-header org-sticky-header org-ref org-plus-contrib org-noter org-mime org-journal org-bullets olivetti ob-ipython nix-mode nix-buffer multiple-cursors mu4e-conversation moody mixed-pitch minions magit lsp-ui lsp-haskell logview impatient-mode hindent highlight-indent-guides helm-tramp helm-swoop helm-projectile helm-org-rifle helm-gtags helm-dash helm-company graphviz-dot-mode god-mode glsl-mode flycheck-rust flycheck-color-mode-line esup ercn erc-terminal-notifier erc-hl-nicks emojify doom-modeline dockerfile-mode docker disk-usage direnv dired-du diminish dhall-mode dashboard dante corral company-lsp clang-format ccls cargo buffer-move apropospriate-theme ag cmake-mode))
 '(pop-up-windows nil)
 '(pos-tip-background-color "#197219721972")
 '(pos-tip-foreground-color "#9E9E9E")
 '(projectile-completion-system 'helm)
 '(projectile-project-root-files-functions
   '(projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring))
 '(python-shell-interpreter "python3")
 '(racer-cmd "racer")
 '(safe-local-variable-values
   '((eval require 'org-ref)
     (eval outshine-cycle-buffer)
     (eval org-overview)
     (eval outshine-hook-function)))
 '(show-paren-mode t)
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes))
 '(swift-repl-executable "xcrun swift -target x86_64-macosx10.11")
 '(tabbar-background-color "#353535")
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:slant normal)))))


;;; File Local Variables
;; Local Variables:
;; mode: emacs-lisp
;; eval: (outline-minor-mode)
;; eval: (outshine-mode)
;; eval: (outshine-cycle-buffer)
;; End:
