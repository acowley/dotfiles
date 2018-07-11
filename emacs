(require 'package)

;;; Before everything else

;; This has to be very early in initialization.
(defvar outline-minor-mode-prefix "\M-#")

(add-to-list 'load-path "/Users/acowley/.nix-profile/share/emacs/site-lisp")

;;; Package setup

; If we run package-initialize, then add-to-list melpa, the
; package-install invocation will fail. We need the package-archives
; list setup before calling package-initialize.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

; (require 'use-package)
(eval-when-compile
  (add-to-list 'load-path "~/src/use-package")
  (require 'use-package))
;; (require 'diminish)
(require 'bind-key)

;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)

;;; General emacs configuration

;;;; Elisp Helpers
(require 'subr-x)
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
  (let ((sep (or separator ", "))
        (prefix-len (if prefix (length prefix) 0)))
    (with-temp-buffer
      (when prefix (insert prefix))
      (insert (string-join xs sep))
      (when suffix (insert suffix))
      (goto-char (point-min))
      (setq fill-prefix (loop repeat prefix-len concat " "))
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

;;;; Miscellaneous Settings

;; A short mode line that is going to be tweaked with moody
(setq-default mode-line-format
      '("%e"
        mode-line-modified
        mode-line-buffer-identification
        "   "
        mode-line-position
        (vc-mode vc-mode)
        "  "
        mode-line-modes
        mode-line-misc-info
        mode-line-end-spaces))

;; (set-default-font "Hæck 14")
(when (memq window-system '(mac ns))
  ;; (set-default-font "Monaco 14")
  (set-frame-font "Monaco 14"))

(setq confirm-kill-emacs #'y-or-n-p)

(tool-bar-mode -1)
(when (and window-system (not (memq window-system '(mac ns))))
  (set-frame-size (selected-frame) 80 56))

;; Enable ligatures for fonts that provide them (e.g. hæck)
;; This may cause slowdown
;; (add-hook 'prog-mode-hook #'mac-auto-operator-composition-mode)
;; (add-hook 'prog-mode-hook (lambda () (auto-composition-mode -1)))
;; (add-hook 'text-mode-hook (lambda () (auto-composition-mode -1)))

;; Disable electric-quote-mode everywhere
(add-hook 'after-change-major-mode-hook
          (lambda () (electric-quote-mode -1)))
(electric-quote-mode -1)

;; Cause use-package to install packages automatically if not already
;; present
(setq use-package-always-ensure t)

;; Clean trailing whitespace when saving a buffer
(setq before-save-hook #'whitespace-cleanup)

;; Keep ediff UI in a single frame
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

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

;; Make bookmark jumping easier
(global-set-key (kbd "C-c b") #'bookmark-jump)

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
(imagemagick-register-types)


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

(setq gc-cons-threshold 100000000) ; ie 100mb, default is 800kb

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
  (advice-add 'switch-to-buffer :after #'recent-buffer))

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

;;;; variable-pitch-mode
(defun my/text-mode-hook ()
  (flyspell-mode)
  (turn-on-visual-line-mode)
  (variable-pitch-mode)
  ;; (setq buffer-face-mode-face '(:family "Helvetica Neue" :weight thin))
  ;; (setq buffer-face-mode-face '(:family "Avenir Next"))
  ;; (setq variable-pitch-face '(:family "Avenir Next"))
  ;; (setq buffer-face-mode-face '(:family "Montserrat"))
  ;; (buffer-face-mode)
  (text-scale-adjust 1.5)
  ;; (text-mode-hook-identify)
  )
(add-hook 'text-mode-hook #'my/text-mode-hook)


;;;; Ignored extensions
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".o")
(add-hook 'ido-setup-hook (setq ido-ignore-extensions t))
(add-hook 'ido-setup-hook (lambda ()
                           (add-to-list 'ido-ignore-files "\\.hi")
                           (add-to-list 'ido-ignore-files "\\.o")))


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
(eval-after-load "ispell" '(defun ispell-get-coding-system () 'utf-8))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;;;; Copy and comment
(defun copy-and-comment ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (comment-dwim nil)
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


;;; Diminish
(use-package diminish :ensure t)
;;; Themes
;; (use-package darkokai-theme :defer t)
;; (use-package monokai-theme :defer t)
(use-package apropospriate-theme
  ;; :load-path "~/Documents/Projects/apropospriate-theme"
  :config
  (load-theme 'apropospriate-dark t))

;;; emacs server
(use-package server :config (and (fboundp 'server-mode)
                                 (or (server-running-p) (server-mode))))
;;; company-mode
(use-package company
  ;; :load-path "~/src/company-mode/company-0.9.4"
  :defer nil
  :commands (company-mode)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1)
  ;; (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-:") 'helm-company)
  ;(setq company-backends (remq 'company-eclim (remq 'company-oddmuse company-backends)))
  (defun ac/company-text-mode ()
    ;; (add-to-list 'company-backends 'company-ispell)
    )
  (add-hook 'text-mode-hook #'ac/company-text-mode))

;;; Projectile
(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-global-mode t
        projectile-ignored-projects '("~/")
        projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".cabal-sandbox" ".cabbages" ".stack-work")
        projectile-project-root-files
        '("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "*.cabal" "compile_commands.json"))
  (projectile-mode))

;;; yasnippet
(use-package yasnippet)

;;; Dashboard
(use-package dashboard
  ;; :load-path "~/src/emacs-dashboard"
  ;; :commands dashboard-insert-startupify-lists
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5))))

;;; impatient-mode
(use-package impatient-mode
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
(use-package esup :defer t :commands esup)
;;; Org-mode

;;;; General Org Configuration
(use-package org
  :ensure org-plus-contrib
  :pin org
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
  :config
  (setq org-src-fontify-natively t
        org-use-speed-commands t
        org-html-doctype "html5"
        org-directory "~/org"
        ;; For leuven-theme
        ;; Fontify the whole line for headings (with a background color).
        org-fontify-whole-heading-line t)

  ;(set-alist 'org-preview-latex-process-alist 'imagemagick (append '(:programs ("latex" "convert")) (alist-get 'imagemagick org-preview-latex-process-alist)))

  (setq org-image-actual-width 600)
  (setq org-latex-prefer-user-labels t)

  (use-package org-bullets)
  (use-package org-table-sticky-header
    :diminish org-table-sticky-header-mode)
  (use-package org-sticky-header)


  ;; This is slow to load
  (use-package ox-tufte :defer t)

  (defun my-org-hook ()
    (org-bullets-mode 1)
    (org-table-sticky-header-mode)
    (org-sticky-header-mode)

    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    ;; electric quotes turn single quotes (') into smart single quotes
    ;; that can greak things in src blocks
    (electric-quote-local-mode -1)

    ; Encryption
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key "D50A574B")

    ;; org-ref is very slow to load, so we defer it upon emacs
    ;; startup, but pull it in when org-mode is first started
    (require 'org-ref))

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
    :ensure org-plus-contrib
    :commands (org-babel-execute:sh
               org-babel-expand-body:sh
               org-babel-execute:bash
               org-babel-expand-body:bash))
  (use-package ob-haskell
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:haskell org-babel-expand-body:haskell))
  (use-package ob-emacs-lisp
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:elisp
               org-babel-expand-body:elisp
               org-babel-execute:emacs-lisp
               org-babel-expand-body:emacs_lisp))
  (use-package ob-octave
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:octave org-babel-expand-body:octave))
  (use-package ob-latex
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:latex org-babel-expand-body:latex))
  (use-package ob-dot
    :defer t
    :ensure org-plus-contrib
    :commands (org-babel-execute:dot org-babel-expand-body:dot))

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

  (add-to-list 'org-agenda-files
               "~/Projects/roshask/roshask-notes.org"
               "~/Projects/MAST/mast-iros-notes.org")

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
            (loop until (or (null pt) (> pt stop)) do
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

  (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

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
  (setq org-latex-pdf-process '("latexmk -g -pdf -shell-escape %f -outdir=$(echo '%o' | sed 's|\\(.*\\)/$|\\1|g')"))
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

  (use-package org-ref
    :defer t
    :config
    (helm-delete-action-from-source "Add PDF to library" helm-source-bibtex)
    (setq
     reftex-default-bibliography '("~/Documents/MyPapers/mybib.bib")
     ;; see org-ref for use of these variables
     org-ref-bibliography-notes "~/Documents/MyPapers/bib-notes.org"
     org-ref-default-bibliography '("~/Documents/MyPapers/mybib.bib")
     org-ref-pdf-directory "~/Documents/MyPapers/references/"
     bibtex-completion-bibliography '("~/Documents/MyPapers/mybib.bib")))

;;;; Blog Publishing
  (defun org-custom-link-blog-follow (path)
    (org-open-file-with-emacs path))

  (defun org-custom-link-blog-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<a href=\"https://www.arcadianvisions.com/blog/%s.html\">%s</a>"
              (file-name-sans-extension path)
              desc))))

  (org-link-set-parameters
   "blog"
   :follow #'org-custom-link-blog-follow
   :export #'org-custom-link-blog-export)

  (setq org-rss-use-entry-url-as-guid nil)
  (defun my/blog-copy-index-to-rss (_)
    (shell-command "(cd ~/Documents/Projects/Blog/blog && cp index.xml rss.xml && nix-shell -p gnused --run \"sed 's/index.xml/rss.xml/' -i ./rss.xml\")"))
  (defun my/blog-sync-assets (_)
    (shell-command "rsync -a  ~/Documents/Projects/Blog/blog/assets/basedir/ ~/Documents/Projects/Blog/blog"))
  (setq org-publish-project-alist
        '(("blog-content"
           :base-directory "~/Documents/Projects/Blog/articles/"
           :publishing-directory "~/Documents/Projects/Blog/blog/"
           :publishing-function org-html-publish-to-html
           :export-babel-evaluate nil
           :recursive t
           ;; :auto-sitemap t
           ;; :sitemap-filename "index.html"
           ;; :sitemap-title "Arcadian Visions Blog"
           ;; :makeindex t
           :htmlized-source t
           :with-creator nil
           :with-date nil
           :with-email nil
           :with-timestamps nil
           :with-toc nil
           :section-numbers nil
           :html-head-include-default-style nil
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../core-style.css\" />"
           :html-postamble "")
          ("blog-rss"
           :base-directory "~/Documents/Projects/Blog/articles/"
           :publishing-directory "~/Documents/Projects/Blog/blog"
           :publishing-function (org-rss-publish-to-rss)
           :html-link-home "http://www.arcadianvisions.com/blog/"
           :completion-function my/blog-copy-index-to-rss
           :html-link-use-abs-url t
           :exclude ".*"
           :include ("index.org")
           :with-toc nil
           :with-creator "Anthony"
           :with-author "Anthony Cowley"
           :section-numbers nil)
          ("blog-assets"
           ;; Static content like images and CSS
           :base-directory "~/Documents/Projects/Blog/assets"
           :base-extension any
           :include ("basedir/.htaccess")
           :recursive t
           :publishing-directory "~/Documents/Projects/Blog/blog/assets"
           :publishing-function org-publish-attachment
           :completion-function my/blog-sync-assets)
          ("blog" :components ("blog-content" "blog-rss" "blog-assets"))))

  ;;; org-noter
  (use-package org-noter)
  )

(use-package outorg
  :defer t
  :commands (outorg-edit-as-org)
  :config
  (defun outorg-whole-file ()
  "Call `outorg-edit-as-org` with a prefix argument so that the
entire source file is loaded."
    (interactive)
    (outorg-edit-as-org '(4))))

(use-package outshine
  :defer t
  :commands (outshine-hook-function)
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


;;;; org-mime

(use-package org-mime
  ;; :load-path "~/src/org-mime"
  :commands (org-mime-org-buffer-htmlize org-mime-org-subtree-htmlize))

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


;;; olivetti-mode
(use-package olivetti)
;;; pdf-tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; If pdf-tools is installed using emacsWithPackage in nix, then the
  ;; `epdfinfo` binary is installed alongside the elisp package.
  (setq pdf-info-epdfinfo-program
        (concat (file-name-directory (locate-library "pdf-tools"))
                "epdfinfo")
        pdf-info-epdfinfo-error-filename nil)
  (pdf-tools-install))
;;; Helm
(use-package helm
  :defer t
  :diminish helm-mode
  :commands (helm-find-files helm-mini helm-M-x helm-imenu)
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("C-c i" . helm-imenu)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ;; use TAB for action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB work in terminal
         ("C-i" . helm-execute-persistent-action)
         ;; list actions
         ("C-z" . helm-select-action))
  :config
  (setq
   helm-candidate-number-limit 100
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
  (when (memq window-system '(mac ns))
    (setq helm-locate-command
          "mdfind -onlyin $HOME -name %s %s | grep -E -v '/dist/|/Caches/'"))


  ;; ido offers a nicer UI for switching between open buffers
  ;; (add-hook 'helm-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'helm-completing-read-handlers-alist
  ;;                          '(switch-to-buffer . ido))))
  (helm-mode t)

  (require 'helm-command)
  (use-package helm-company
    :defer t
    :bind (:map company-mode-map
           ("C-:" . helm-company)))
  (use-package helm-swoop
    :defer t
    :commands helm-swoop
    :bind (("M-i" . helm-swoop)
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
  (use-package helm-tramp
    :defer t
    :config
    (use-package docker-tramp))
  (use-package helm-projectile
    :defer t))

(ido-mode -1)

;; (use-package imenu-anywhere
;;   :defer t
;;   :config
;;   (setq imenu-anywhere-buffer-filter-functions (list #'imenu-anywhere-same-project-p)))

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
         ("C-s" . helm-swoop)       ;; instead of isearch-forward

         ;; I swap these becuase I use switch-to-buffer much more frequently
         ;; and prefer it to have the simpler binding.
         ("C-x b" . list-buffers)
         ("C-x C-b" . switch-to-buffer)
         :map god-local-mode-map
         ("." . repeat)))

;; (global-set-key (kbd "<f9>") 'god-mode-all)
;; (define-key god-local-mode-map (kbd ".") 'repeat)
;; (global-set-key (kbd "C-x C-o") 'other-window)
;; (global-set-key (kbd "C-s") #'helm-swoop) ; instead of isearch-forward
;; (global-set-key (kbd "C-x b") #'list-buffers)
;; (global-set-key (kbd "C-x C-b") #'switch-to-buffer)


;;; Email (mu4e)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)

(use-package mu4e
  :load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"
  :ensure nil
  :defer t
  :commands (mu4e)
  :config
  (setq
   mu4e-maildir "~/.mail"
   ;; mu4e-html2text-command  "/Users/acowley/.nix-profile/bin/w3m -T text/html"
   ;; mu4e-get-mail-command "~/.nix-profile/bin/mbsync gmail-inbox gmail-trash"
   mu4e-html2text-command "w3m -T text/html"
   mu4e-get-mail-command "mbsync gmail-inbox gmail-trash"

   ;; gmail folder setup
                                        ;mu4e-drafts-folder "/gmail/drafts"
   mu4e-drafts-folder "/mu4e/drafts"
   mu4e-sent-folder   "/gmail/sent"
   mu4e-trash-folder  "/gmail/trash"

   mu4e-headers-skip-duplicates t
   mu4e-compose-dont-reply-to-self t
   mu4e-view-show-images t
   mu4e-update-interval 600

   ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
   mu4e-sent-messages-behavior 'delete

   ;; setup some handy shortcuts
   ;; you can quickly switch to your Inbox -- press ``ji''
   ;; then, when you want archive some messages, move them to
   ;; the 'All Mail' folder by pressing ``ma''.
   mu4e-maildir-shortcuts '( ("/gmail/Inbox"   . ?i)
                             ("/gmail/sent"    . ?s)
                             ("/gmail/trash"   . ?t)
                             ("/gmail/archive" . ?a))

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

  ;; Auto-complete contact email addresses
  ;; We don't want line breaks added to emails we compose
  (defun my/mu4e-compose-hook ()
    (company-mode)
    (turn-off-auto-fill)
    ;; (variable-pitch-mode)
    (turn-on-visual-line-mode)
    (setq buffer-face-mode-face '(:family "Avenir Next"))
    (buffer-face-mode))

  (add-hook 'mu4e-compose-mode-hook #'my/mu4e-compose-hook)

  ;; Add a view in browser action. Trigger with "aV"
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; NOTE: deleting a message in Gmail is accomplished by moving to the
  ;; trash folder. "Marking for deletion" actually archives the message.
  (fset 'my-move-to-trash "mt")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
                                        ;(setq mu4e-view-prefer-html t)
                                        ;(setq mu4e-html2text-command "html2text -utf8 -width 72")

  (defun my/mu4e-view-hook ()
    ; (turn-on-visual-line-mode)
    ;; (variable-pitch-mode)
    (setq buffer-face-mode-face (if (memq window-system '(mac ns) )
                                    '(:family "Avenir Next")
                                  '(:family "Cantarell")))
    (olivetti-mode)
    (buffer-face-mode)
    (text-scale-adjust 1))

  (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-hook)

  ;; Mark email attachments in dired with C-c RET C-a
  ;; From http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
  (use-package gnus-dired
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    :ensure nil
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
                           (shell-command-to-string "mu find maildir:'/gmail/Inbox' flag:unread --format=sexp")
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
       (smtpmail-auth-supported (login)))
                                        ;(smtpmail-auth-supported '(cram-md5 plain login)))
      ("upenn"
       (user-mail-address "acowley@seas.upenn.edu")
       (smtpmail-default-smtp-server "smtp.seas.upenn.edu")
       (smtpmail-smtp-server "smtp.seas.upenn.edu")
                                        ;(smtpmail-smtp-service 578)
       (smtpmail-smtp-service 465)
       (smtpmail-stream-type ssl)
       (smtpmail-auth-supported (login)))))

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
                my-mu4e-account-alist)))


;;; minions
(use-package minions
  :config (minions-mode 1))

;;; moody
(use-package moody
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
  :defer nil
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
  :init
  (setq inferior-haskell-find-project-root nil
        haskell-process-type 'cabal-repl
        haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fno-ghci-sandbox"))
  :bind (:map haskell-mode-map
         ("C-x C-d" . nil)
         ("C-c C-z" . haskell-interactive-switch)
         ("C-c C-l" . haskell-process-load-or-reload)
         ("C-c C-b" . haskell-interactive-switch)
         ("C-c C-n C-t" . haskell-process-do-type)
         ("C-c C-n C-i" . haskell-process-do-info)
         ("C-c C-n C-c" . haskell-process-cabal-build)
         ("C-c C-n c" . haskell-process-cabal)
         ("C-c M-." . nil)
         ("C-c C-d" . nil))
  :config
  ;; (electric-indent-local-mode -1)
  (company-mode)
  (use-package shm
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
    :load-path "~/src/intero/elisp"
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
  (use-package hindent)

  (defun my-haskell-mode-hook ()
    (structured-haskell-mode)
    (electric-indent-local-mode -1)
    (electric-pair-local-mode -1)
    (electric-quote-local-mode -1)
    (intero-global-mode))
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
  :commands lsp-mode
  :custom-face
  ;; Make the symbol-at-point highlight a bit dimmer than the default
  (lsp-face-highlight-textual ((t (:background "#757500"))))
  :config
  (setq lsp-highlight-symbol-at-point nil)
  (use-package company-lsp
    :config
    (setq company-lsp-enable-snippet t)
    (add-to-list 'company-backends 'company-lsp))
  (use-package lsp-ui
    :commands lsp-ui-mode
    :bind (:map lsp-ui-mode-map
           ("C-c C-s" . lsp-ui-sideline-toggle-symbols-info)
           ("M-." . lsp-ui-peek-find-definitions)
           ("M-?" . lsp-ui-peek-find-references)
           :map lsp-ui-peek-mode-map
           ("M-n" . lsp-ui-peek--select-next)
           ("M-p" . lsp-ui-peek--select-prev))
    :config
    (setq lsp-ui-sideline-delay 0.2))
  (require 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;; cquery
(use-package cquery
  :load-path "~/Projects/emacs-cquery"
  :commands lsp-cquery-enable
  :init
  (setq cquery-sem-highlight-method 'overlay)
  ;; (setq cquery-sem-highlight-method 'font-lock)
  ;; (setq cquery-sem-highlight-method nil)
  (setq-local cquery-extra-init-params
              '(:indexBlacklist '("GPATH" "GRTAGS" "GTAGS")
                                :cacheFormat "msgpack"))
  :config
  (setq xref-prompt-for-identifier (append xref-prompt-for-identifier '(xref-find-references))))

(defun in-docker-p ()
  "Returns a non-nil value if we are running in a docker container"
  (eq (call-process-shell-command "grep -q docker /proc/1/cgroup") 0))

(defun cquery-mode ()
  "Start all cquery-related modes"
  (interactive)
  (when (let ((ext (file-name-extension (or (buffer-file-name) ""))))
          (and (not (null ext))
               (or (string-equal ext "cpp")
                   (string-equal ext "cc")
                   (string-equal ext "hpp"))))

    (flycheck-mode)
    (lsp-cquery-enable)
    (yas-minor-mode)
    (helm-gtags-mode -1)
    (local-set-key (kbd "M-.") #'xref-find-definitions)))

(defun cquery-nix-shell ()
  "Find a cquery executable in a nix-shell associated with the
directory containig the current file if that file’s extension is
`cpp` or `hpp`. Use the location of that executable in the nix
store to load and configure the cquery lsp client."
  (when (let ((ext (file-name-extension (or (buffer-file-name) ""))))
          (and (not (null ext))
               (or (string-equal ext "cpp")
                   (string-equal ext "cc")
                   (string-equal ext "hpp"))))
    (if (in-docker-p)
        (progn
          (message "Using locally-built cquery in docker container")
          (setq-local cquery-executable "/home/acowley/src/cquery/docker-build/cquery"))
      (let ((nix-shell
           (concat (locate-dominating-file (or load-file-name buffer-file-name)
                                           "shell.nix")
                   "shell.nix")))
      (when nix-shell
        (let* ((exes
                (split-string
                 (string-trim
                  (shell-command-to-string
                   (concat "nix-shell " nix-shell
                           " --run 'which cquery; which clang-format'")))
                 "\n" t))
               (cquery-exe (car exes))
               (clang-format-exe (cadr exes))
               (cquery-root (file-name-directory
                             (directory-file-name
                              (file-name-directory cquery-exe)))))
          (message "cquery-root: %s" cquery-root)
          ;; (require 'cquery)
          (setq clang-format-executable clang-format-exe)
          (setq-local cquery-executable cquery-exe)))) )


    ;; General setup

    ;; (setq cquery-extra-args '("--log-all-to-stderr" "--log-file" "cquery.log"))
    (setq cquery-extra-args '("--log-all-to-stderr"))
    ;; (flycheck-mode)
    ;; (lsp-cquery-enable)
    ;; (yas-minor-mode)
    ;; (helm-gtags-mode -1)
    ;; (diminish 'company-mode)
    ;; (diminish 'flyspell-mode)
    ;; (local-set-key (kbd "M-.") #'xref-find-definitions)
    (cquery-mode)))

;;; C++

(use-package cmake-mode
  :mode "\\CMakeLists.txt\\’")

(use-package helm-gtags
  :defer t
  :diminish helm-gtags-mode
  :bind (:map helm-gtags-mode-map
         ("M-." . helm-gtags-find-tag)
         ("M-*" . helm-gtags-pop-stack))
  :config
  (setq helm-gtags-direct-helm-completing t
        helm-gtags-auto-update t
        helm-gtags-ignore-case t))

(use-package clang-format)

(defun my/c++-mode-hook ()
  (electric-indent-mode t)
  (electric-pair-mode t)
  (setq company-backends (delete 'company-clang (delete 'company-semantic company-backends)))
  (which-function-mode)
  (set-face-foreground 'which-func "LightSkyBlue")
  (yas-minor-mode-on)
  (helm-gtags-mode 1))

(add-hook 'c++-mode-hook #'my/c++-mode-hook)

;;; mixed-pitch
(use-package mixed-pitch
  :ensure t
  :diminish mixed-pitch-mode
  :config
  ;; If you want it in all text modes:
  (add-hook 'text-mode-hook #'mixed-pitch-mode)
  ;; Depending on your specific setup, you may want to adjust the height of
  ;; variable pitch fonts:
  (set-face-attribute 'variable-pitch nil :height 160)
  )

;;; python
(defun ac/python-hook ()
  (setq python-indent-offset 2))

(add-hook 'python-mode-hook #'ac/python-hook)

;;; File mode associtions

;; OpenCL code uses c-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))

; QML mode
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.machine\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-mode))

;;; imaxima
;; imaxima installation
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)


;;; git
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(use-package magit
  :defer t
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
  :config
  (add-hook 'twittering-mode-hook
            (lambda ()
              (variable-pitch-mode)
              (turn-on-visual-line-mode)
              (setq buffer-face-mode-face '(:family "Avenir Next"))
              (buffer-face-mode)
              (text-scale-adjust 1)))
  (add-hook 'twittering-edit-mode-hook 'flyspell-mode)

  (set-variable 'twittering-use-master-password t))

;;; corral
(use-package corral
  :defer nil
  :config
  (setq corral-preserve-point t)
  (global-set-key (kbd "M-9") #'corral-parentheses-backward)
  (global-set-key (kbd "M-0") #'corral-parentheses-forward)
  (global-set-key (kbd "M-\"") #'corral-double-quotes-backward)
  (global-set-key (kbd "M-{") #'corral-braces-backward)
  (global-set-key (kbd "M-}") #'corral-braces-forward)
  (global-set-key (kbd "M-[") #'corral-brackets-backward)
  (global-set-key (kbd "M-]") #'corral-brackets-forward))

;;; rust
(use-package rust-mode
  :defer t
  :config
  (use-package cargo)
  (use-package flycheck-rust
    :config
    (flycheck-rust-setup))
  (use-package racer
    :commands (racer-mode)
    :bind (:map rust-mode-map
           ("TAB" . company-indent-or-complete-common))
    :config
    (defun my/racer-hook ()
      (eldoc-mode)
      (company-mode)
      (setq company-tooltip-align-annotations t))
    (add-hook 'racer-mode-hook #'eldoc-mode))
  (defun my/rust-hook ()
    (flycheck-rust-setup)
    (flycheck-mode)
    (racer-mode))
  (add-hook 'rust-mode-hook #'my/rust-hook))

;;; purescript
(use-package purescript-mode
  :defer t
  :config
  ;(use-package flycheck-purescript)
  ;; (add-hook 'purescript-mode-hook #'flycheck-mode)
  (use-package psc-ide)
  (defun my/purescript-hook ()
    (psc-ide-mode)
    (company-mode t)
    (flycheck-mode t)
    (turn-on-purescript-indentation))
  (add-hook 'purescript-mode-hook #'my/purescript-hook))
;;; elisp
(use-package paredit
  :commands paredit-mode
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
  :config
  ;; From https://github.com/yoshiki/yaml-mode/issues/25#issuecomment-250440342
  (add-hook 'yaml-mode-hook
            (lambda ()
              (outline-minor-mode)
              (define-key yaml-mode-map (kbd "TAB") 'outline-toggle-children)
              ;(setq outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)")
              )))
;;; redprl
(use-package redprl
  :config
  (setq redprl-command "/nix/store/p0m23jnrpgypin961spa4c3gn0xmd3x1-redprl-2016-09-22/bin/redprl"
        flycheck-redprl-executable "/nix/store/p0m23jnrpgypin961spa4c3gn0xmd3x1-redprl-2016-09-22/bin/redprl"))

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
  :bind (("C-c d" . dict-lookup-search-pointer)))

;;; graphviz-dot-mode
(use-package graphviz-dot-mode :defer t)
;;; nix-buffer
(use-package nix-buffer
  :defer t
  :commands (nix-buffer))
;;; toml
(use-package toml-mode :defer t)
;;; markdown-mode
(use-package markdown-mode :defer t)
;;; glsl
(use-package glsl-mode
  :mode "\\.(vert|frag|geom)\\'")
;;; smartparens-mode
(use-package smartparens
  :defer t
  :config
  (require 'smartparens-config))

;;; logview
(use-package logview
  :defer t
  :config
  (setq logview-additional-level-mappings '(("ICP" . ((error       "ERROR")
                                                      (warning     "WARNING")
                                                      (information "INFO")
                                                      (debug       "DEBUG")
                                                      (trace       "TRACE"))))
        logview-additional-submodes '(("ICP" . ((format . "TIMESTAMP LEVEL ")
                                                (levels . "ICP")
                                                (timestamp . ("HH:mm:ss" "HH:mm:ss.SSS")))))))
;;; ag
(use-package ag :defer t)
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

;;; Private Configuration
;; Set up paths for org files, etc.
(when (file-exists-p "~/.emacsPrivate.el")
  (load "~/.emacsPrivate.el"))

;;; Mode-line cleanup
(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%2c")))))

(setq-default mode-line-format
      (cl-reduce #'cl-remove
                 (list 'mode-line-front-space
                       'mode-line-mule-info
                       'mode-line-client
                       'mode-line-remote
                       'mode-line-frame-identification)
                 :initial-value mode-line-format
                 :from-end t))

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(beacon-color "#ec4780")
 '(column-number-mode t)
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command outshine-self-insert-command)))
 '(company-ghc-autoscan t)
 '(company-ghc-show-info (quote oneline))
 '(custom-safe-themes
   (quote
    ("d5cdb20cc31dfd701ee4ac5fed09d0e1898ee828c6036c4ee00fdc1e50eb7558" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "412c25cf35856e191cc2d7394eed3d0ff0f3ee90bacd8db1da23227cdff74ca2" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "0f92b9f1d391caf540ac746bc251ea00a55f29e20a411460eb6d8e49892ddef9" "d94eec01b45c7dc72e324af86fd2858e97c92220c195b5dbae5f8fd926a09cec" "1a53efc62256480d5632c057d9e726b2e64714d871e23e43816735e1b85c144c" "0f98f9c2f1241c3b6227af48dc96e708ec023dd68363edb5d36dc7beaad64c23" "13270e81a07dac4aeb1efefb77b9e61919bb3d69da7253ade632856eed65b8a2" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "0a1a7f64f8785ffbf5b5fbe8bca1ee1d9e1fb5e505ad9a0f184499fe6747c1af" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c810219104d8ff9b37e608e02bbc83c81e5c30036f53cab9fe9a2163a2404057" "d46b5a32439b319eb390f29ae1810d327a2b4ccb348f2018b94ff22f410cb5c4" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "2d20b505e401964bb6675832da2b7e59175143290dc0f187c63ca6aa4af6c6c1" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "d22a6696fd09294c7b1601cb2575d8e5e7271064453d6fa77ab4e05e5e503cee" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(dired-dwim-target t)
 '(dired-recursive-deletes (quote always))
 '(doc-view-resolution 200)
 '(doc-view-scale-internally nil)
 '(electric-pair-mode t)
 '(electric-quote-mode t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-server-auto-reconnect nil)
 '(evil-emacs-state-cursor (quote ("#E57373" hbar)))
 '(evil-insert-state-cursor (quote ("#E57373" bar)))
 '(evil-normal-state-cursor (quote ("#FFEE58" box)))
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GPG_AGENT_INFO")))
 '(flycheck-ghc-args (quote ("-Wall")))
 '(flycheck-swift-sdk-path
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
 '(flycheck-swift-target "x86_64-macosx10.11")
 '(ghc-doc-browser-function (quote ghc-browse-url-safari))
 '(ghc-use-nix-shell (quote (quote t)))
 '(global-eldoc-mode t)
 '(haskell-indent-offset 2)
 '(helm-mu-gnu-sed-program "gnused")
 '(highlight-symbol-colors
   (quote
    ("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(hl-sexp-background-color "#efebe9")
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-use-overlays nil)
 '(org-default-notes-file "~/org/home.org")
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
 '(org-footnote-auto-label (quote plain))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-html-validation-link "")
 '(org-imenu-depth 3)
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("colorlinks=true" "hyperref" nil))))
 '(org-mobile-files (quote ("~/org/home.org")))
 '(org-preview-latex-default-process (quote imagemagick))
 '(org-reveal-root "reveal.js")
 '(org-src-preserve-indentation t)
 '(org-src-window-setup (quote other-window))
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_EXPORT LaTeX
?
#+END_EXPORT" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_EXPORT html
?
#+END_EXPORT" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")
     ("eq" "\\begin{equation*}
?
\\end{equation*}" "")
     ("Eq" "\\begin{equation}
?
\\end{equation}" "")
     ("al" "\\begin{align*}
?
\\end{align*}" "")
     ("n" "#+BEGIN_NOTES
?
#+END_NOTES" ""))))
 '(package-selected-packages
   (quote
    (ercn org-sticky-header org-table-sticky-header org-bullets org-ref org-noter znc yaml-mode use-package twittering-mode toml-mode spaceline smartparens shm redprl recentf-remove-sudo-tramp-prefix racer purescript-mode paredit osx-dictionary nix-mode nix-buffer multiple-cursors mixed-pitch magit lsp-ui logview intero hindent helm-tramp helm-swoop helm-projectile helm-gtags helm-dash helm-company graphviz-dot-mode god-mode flycheck-rust erc-terminal-notifier erc-hl-nicks docker-tramp dante corral company-lsp clang-format cargo buffer-move ag yasnippet visual-fill-column ox-tufte ox-clip outshine org-plus-contrib olivetti monokai-theme impatient-mode imenu-anywhere esup diminish dashboard darkokai-theme cmake-mode apropospriate-theme)))
 '(pop-up-windows nil)
 '(python-shell-interpreter "python3")
 '(racer-cmd "racer")
 '(safe-local-variable-values
   (quote
    ((eval if
           (memq window-system
                 (quote
                  (mac ns)))
           (cquery-nix-shell)
           (setq cquery-executable "/home/acowley/Projects/rcquery")
           (cquery-mode))
     (eval outshine-hook-function)
     (eval cquery-nix-shell)
     (org-time-stamp-format quote
                            ("<%Y-%m-%d>" . "<%Y-%m-%d>"))
     (org-html-htmlize-output-type . css)
     (org-html-postamble-format
      ("en" "<p class=\"date\">Published: %d</p>"))
     (org-html-postamble-format quote
                                (("en" "<p class=\"date\">Published: %d</p>")))
     (org-html-postamble . t)
     (org-export-with-creator)
     (org-export-with-email)
     (org-html-postamble)
     (org-export-babel-evaluate . t)
     (org-image-actual-width . 500)
     (org-confirm-babel-evaluate)
     (org-confirm-babel-evaluate lambda
                                 (lang body)
                                 (not
                                  (string= lang "emacs-lisp")))
     (eval org-overview))))
 '(show-paren-mode t)
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
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
 '(lsp-face-highlight-textual ((t (:background "#757500"))))
 '(mu4e-header-value-face ((t (:inherit font-lock-doc-face :foreground "Green"))))
 '(org-block ((t (:slant normal))))
 '(variable-pitch ((t (:family "Fira Sans Light")))))


;;; File Local Variables
;; Local Variables:
;; mode: emacs-lisp
;; eval: (outline-minor-mode)
;; eval: (outshine-hook-function)
;; eval: (org-overview)
;; End:
