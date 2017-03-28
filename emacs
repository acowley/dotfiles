(require 'package)

;;; Before everything else

;; This has to be very early in initialization.
(defvar outline-minor-mode-prefix "\M-#")

(add-to-list 'load-path "/Users/acowley/.nix-profile/share/emacs/site-lisp")

;;; Package setup

;; Make sure the packages I use are installed
(setq my-packages '(exec-path-from-shell
                    use-package
                    company-coq company-math
                    htmlize
                    auctex
                    powerline smart-mode-line smart-mode-line-powerline-theme
                    session
                    ag
                    nix-mode
                    glsl-mode yaml-mode vagrant-tramp cmake-mode
                    visual-fill-column
                    ;; Use the terminal-notifier program on OS X
                    erc-hl-nicks erc-terminal-notifier
                    tuareg flycheck-ocaml))

; If we run package-initialize, then add-to-list melpa, the
; package-install invocation will fail. We need the package-archives
; list setup before calling package-initialize.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; Fetch the list of available packages
(unless package-archive-contents (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (princ "Installing package ")
    (princ package)
    (newline)
    (package-install package)))

;;; General emacs configuration

;;;; Elisp Helpers
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

;;;; Miscellaneous Settings

;; Cause use-package to install packages automatically if not already
;; present
(setq use-package-always-ensure t)

;; Use the exec-path-from-shell package to set the PATH
(when (memq window-system '(mac ns))
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;; Move point to farthest possible position when scrolling the window
;; has reached the beginning or end of the buffer
(setq scroll-error-top-bottom t)

;; Support Cmd-up/down for top/bottom of buffer
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

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


;; Preserve history between sessions
(add-hook 'after-init-hook 'session-initialize)

;; Don't interfere with helm-show-kill-ring
(setq session-save-print-spec '(t nil 40000))

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
(when (fboundp 'server-mode) (funcall 'server-mode 1))

;; Revert buffers whose files have changed on disk
(global-auto-revert-mode t)

;; Disable the alarm bell on Quit (C-g)
(setq ring-bell-function 'ignore)

;; Turn auto-fill-mode on by default in text modes
; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Turn off electric-indent-mode everywhere
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(put 'downcase-region 'disabled nil)

;; (setq TeX-command-extra-options "-shell-escape")

(put 'dired-find-alternate-file 'disabled nil)

;(load-theme 'monokai t)
;(load-theme 'darktooth t)

(use-package recentf
  :init
  (setq
   ;; This is an attempt to prevent recentf (that keeps track of recent
   ;; files) from stat'ing remote files.
   recentf-keep '(file-remote-p file-readable-p)
   recentf-exclude
    `("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" ,(regexp-quote "/.emacs.d/elpa/") ,(regexp-quote "/var/folders/")))
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

;; Use erc-terminal-notifier with erc
(add-hook 'erc-mode-hook (lambda() (require 'erc-terminal-notifier)))

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

(setq browse-url-browser-function #'browse-url-safari)

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

;;;; variable-pitch-mode
(defun my/text-mode-hook ()
  (flyspell-mode)
  (turn-on-visual-line-mode)
  ;(variable-pitch-mode)
  (setq buffer-face-mode-face '(:family "Helvetica Neue" :weight thin))

  ;; (setq buffer-face-mode-face '(:family "Avenir"))
  ;; (setq buffer-face-mode-face '(:family "Montserrat"))
  (buffer-face-mode)
  (text-scale-adjust 2)
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
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
         ("-d" "en_US"))
        nil utf-8))
;(setq ispell-extra-args '("-a" "-i" "utf-8"))
(setq ispell-local-dictionary "en_US")
(setq ispell-personal-dictionary "~/.hunspell_en_US")

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
  (insert "\n"))

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


;;; Themes
(use-package darkokai-theme :defer t)
(use-package monokai-theme :defer t)
(use-package apropospriate-theme
  :load-path "~/Documents/Projects/apropospriate-theme"
  :config
  (load-theme 'apropospriate-dark t))

;;; Projectile
(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-global-mode t
        projectile-ignored-projects '("~/")
        projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".cabal-sandbox" ".cabbages" ".stack-work")
        projectile-project-root-files
        '("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "*.cabal"))
  (projectile-global-mode))

;;; Dashboard
(use-package dashboard
  :load-path "~/src/emacs-dashboard"
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


;;; Org-mode

;;;; General Org Configuration
(use-package org
  :defer 7
  :ensure org-plus-contrib
  :pin org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :org-mode-map
         ;; Don't fight the bindings that use
         ;; shift-arrow to move focus between windows.
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil))
  :config

  ;; Let markup strings be bordered by letter characters
  ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components)
  ;;         "[:alpha:]- \t\.,:!?;'\")}\\")
  ;; ;; Let emphasized strings be bordered by quotes
  ;; (setcar (nthcdr 2 org-emphasis-regexp-components) "\t\r\n, ")
  ;; (org-set-emph-re 'org-emphasis-regexp-components
  ;;                  org-emphasis-regexp-components)

  (setq org-src-fontify-natively t
        org-use-speed-commands t
        org-html-doctype "html5"
        org-directory "~/org"
        ;; For leuven-theme
        ;; Fontify the whole line for headings (with a background color).
        org-fontify-whole-heading-line t)

  (set-alist 'org-preview-latex-process-alist 'imagemagick (append '(:programs ("latex" "convert")) (alist-get 'imagemagick org-preview-latex-process-alist)))

;; (set-alist 'org-preview-latex-process-alist 'imagemagick '(:programs
;;               ("latex" "convert")
;;               :description "pdf > png" :message "you need to install the programs: latex, imagemagick and ghostscript." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
;;               (1.0 . 1.0)
;;               :latex-compiler
;;               ("pdflatex -interaction nonstopmode -output-directory %o %f")
;;               :image-converter
;;               ("convert -density %D -trim -antialias %f -quality 100 %b.png")))

  (setq org-image-actual-width 600)
  (setq org-latex-prefer-user-labels t)

  (use-package org-bullets)
  (use-package org-table-sticky-header)

  (defun my-org-hook ()
    (org-bullets-mode 1)
    (org-table-sticky-header-mode))
  (add-hook 'org-mode-hook #'my-org-hook)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t) (ditaa . t) (shell . t) (emacs-lisp . t) (octave . t)
     (C . t) (js . t) (ipython . t) (maxima . t) (latex . t) (dot . t)))

  ;; Syntax highlight dot source blocks
  (set-alist 'org-src-lang-modes "dot" 'graphviz-dot)

  ;; Org sets the block face to a hard-to-read gray color by default.
  ;; (set-face-attribute
  ;;  'org-block nil :foreground (face-attribute 'default :foreground))

  ;; Disable variable-pitch-mode in tables. We used to be
  ;; able to disable this in src blocks, but this no
  ;; longer works.
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images
            'append)

  (add-to-list 'org-agenda-files
               "~/Documents/Projects/roshask/roshask-notes.org"
               "~/Documents/Projects/Cosy/Cosy-notes.org")

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
  (defun adjoin-to-list-or-symbol (element list-or-symbol)
    (require 'cl)
    (adjoin element (if (not (listp list-or-symbol))
                        (list list-or-symbol)
                      list-or-symbol)))
  (mapc (lambda (face)
           (set-face-attribute face nil
                               :inherit (adjoin-to-list-or-symbol
                                          'fixed-pitch
                                          (face-attribute face :inherit))))
         ;(list 'org-code 'org-block 'org-table 'org-block-background)))
         (list 'org-code 'org-block 'org-table))

  ;; LaTeX export
  (require 'ox-latex)
  (setq org-latex-pdf-process '("latexmk -g -pdf -shell-escape %f -outdir=$(echo '%o' | sed 's|\\(.*\\)/$|\\1|g')"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-minted-langs '(haskell "haskell"))

  ;; mathescape permits math-mode in comments, while escapeinside
  ;; specifies brackets within which math-mode can be enabled in code
  ;; listings.
  (setq org-latex-minted-options
        '(("escapeinside" "||") ("mathescape=true")))
  ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))

  (require 'org-clock)
  (add-to-list 'org-clock-clocktable-language-setup '("en" "File"     "L"  "Timestamp"  "Task" "Time"  "ALL"   "Total time"   "File time" "Time Sheet at"))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda . " %i %-12:c%?-12t% s")
  ;;         (timeline . "  % s")
  ;;         (todo . " %i %b%-12:c")
  ;;         (tags . " %i %-12:c")
  ;;         (search . " %i %-12:c")))

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (use-package org-ref
    :defer nil
    :config
    (helm-delete-action-from-source "Add PDF to library" helm-source-bibtex)
    (setq
     reftex-default-bibliography '("~/Documents/MyPapers/mybib.bib")
     ;; see org-ref for use of these variables
     org-ref-bibliography-notes "~/Documents/MyPapers/bib-notes.org"
     org-ref-default-bibliography '("~/Documents/MyPapers/mybib.bib")
     org-ref-pdf-directory "~/Documents/MyPapers/references/"
     bibtex-completion-bibliography '("~/Documents/MyPapers/mybib.bib")))
  (require 'org-ref))

(eval-after-load "font-latex"
  '(mapc (lambda (face)
           (set-face-attribute face nil
                               :inherit (adjoin-to-list-or-symbol
                                          'fixed-pitch
                                          (face-attribute face :inherit))))
         (list 'font-latex-math-face 'font-latex-verbatim-face
               'font-lock-keyword-face)))

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
  :init
  (add-hook 'outline-minor-mode-hook (lambda ()
                                       (require 'outshine)
                                       (outshine-hook-function)))
  (add-hook 'prog-mode-hook 'outline-minor-mode))

;; Copy from an Org buffer to the system clipboard after converting
;; the Org content to rich text format.
(use-package ox-clip
  :defer t
  :commands (ox-clip-formatted-copy))

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

;;;; Encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "D50A574B")

;;;; IHaskell

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

;;;; Blog Publishing
(setq org-rss-use-entry-url-as-guid nil)
(defun my/blog-copy-index-to-rss (_)
  (shell-command "(cd ~/Documents/Projects/Blog/blog && cp index.xml rss.xml)"))
(defun my/blog-sync-assets (_)
  (shell-command "rsync -a ~/Documents/Projects/Blog/blog/assets/basedir/ ~/Documents/Projects/Blog/blog"))
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
;;;; org-mime

(use-package org-mime
  :load-path "~/src/org-mime"
  :ensure nil
  :commands (org-mime-org-buffer-htmlize))

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
;;; Helm
(use-package helm
  :defer 5
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
   helm-M-x-requires-pattern 3      ; Require at least one character
   helm-ff-file-name-history-use-recentf t
   helm-ff-skip-boring-files t

                                        ; helm-idle-delay 0.0
                                        ; helm-input-idle-delay 0.01

   ;; Use Spotlight on OS X to find files
   helm-locate-command
   "mdfind -onlyin $HOME -name %s %s | grep -E -v '/dist/|/Caches/'"
   helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-buffer-not-found
                               helm-source-locate))

  ;; ido offers a nicer UI for switching between open buffers
  ;; (add-hook 'helm-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'helm-completing-read-handlers-alist
  ;;                          '(switch-to-buffer . ido))))
  (helm-mode t)
  (use-package helm-company
    :defer t)
  (use-package helm-swoop
    :defer t
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
  (use-package helm-projectile
    :defer t))

(ido-mode -1)

;;; god-mode

(use-package god-mode
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)

  (defun ac/god-mode-toggle ()
    "Set the mode line to a white background when god-mode is
active; black when inactive."
    (if god-local-mode
        (progn
          (set-face-background 'sml/line-number "orange")
          (set-face-foreground 'sml/line-number "black")
          (set-face-attribute 'mode-line nil :box "orange")
          ;; (hl-line-mode 1)
          )

      (set-face-background 'sml/line-number "black")
      (set-face-foreground 'sml/line-number "white")
      (set-face-attribute 'mode-line nil :box nil)
      (unless (eq major-mode 'mu4e-headers-mode) (hl-line-mode -1))))

  (add-hook 'god-mode-enabled-hook #'ac/god-mode-toggle)
  (add-hook 'god-mode-disabled-hook #'ac/god-mode-toggle)
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

  ; mortal-mode from: https://github.com/chrisdone/god-mode/issues/77

  ; This mortal mode is designed to allow temporary departures from god mode
  ; The idea is that within god-mode, you can hit shift-i, type in a few characters
  ; and then hit enter to return to god-mode. To avoid clobbering the previous bindings,
  ; we wrap up this behavior in a minor-mode.
  (define-minor-mode mortal-mode
    "Allow temporary departure from god-mode."
    :lighter " mortal"
    :keymap '(([return] . (lambda ()
                            "Exit mortal-mode and resume god mode." (interactive)
                            (god-local-mode-resume)
                            (mortal-mode 0))))
    (when mortal-mode
      (god-local-mode-pause)))

  (define-key god-local-mode-map (kbd "i") 'mortal-mode)

  ;; On OS X, set "caps lock" to no action in system preferences, then
  ;; use the Seil app to rebind "caps lock" to f9.

  :bind (("<f9>" . god-mode-all)
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
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
;(require 'mu4e)

(use-package mu4e
  :ensure nil
  :defer t
  :commands (mu4e)
  :config
  (setq
   mu4e-maildir "~/.mail"
   mu4e-html2text-command  "/usr/local/bin/w3m -T text/html"
                                        ;mu4e-mu-binary "/usr/local/bin/mu"

   ;; allow for updating mail using 'U' in the main view:
                                        ; mu4e-get-mail-command "/usr/local/bin/mbsync -a"
   ;; mu4e-get-mail-command
   ;;   (concat
   ;;    (replace-regexp-in-string
   ;;     "\n\\'" "" (shell-command-to-string "readlink $(which mbsync)"))
   ;;    " -a")
   ;mu4e-get-mail-command "~/.nix-profile/bin/mbsync -a"
   mu4e-get-mail-command "~/.nix-profile/bin/mbsync gmail-inbox gmail-trash"

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
   message-kill-buffer-on-exit t)

  ;; Auto-complete contact email addresses
  ;; We don't want line breaks added to emails we compose
  (defun my/mu4e-compose-hook ()
    (company-mode)
    (turn-off-auto-fill)
    (variable-pitch-mode)
    (turn-on-visual-line-mode)
    (setq buffer-face-mode-face '(:family "Avenir Next"))
    (buffer-face-mode)
    ;; (text-scale-adjust 1)
    )

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
    (turn-on-visual-line-mode)
    (variable-pitch-mode)
    (setq buffer-face-mode-face '(:family "Avenir Next"))
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
                  (shell-command (concat "terminal-notifier -title \"mu4e\" -sender \"org.gnu.Emacs\" -message \"" msg "\""))))))

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

;;; smart-mode-line (powerline)

(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Replace ":Doc:Projects/Foo/blah.hs" with ":Foo:blah.hs"
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/\\([^/]*\\)/" ":\\1:") t)
;(sml/apply-theme 'smart-mode-line-powerline)
(sml/apply-theme 'dark)

;; Don't show common minor modes
(setq rm-blacklist (mapconcat 'identity '(" Fly" " company" " God" " Helm" " Outl" " ARev" " BufFace" " Wrap" "+1" "Projectile.*") "\\|"))
;(setq sml/mode-width "full")
;; (setq sml/mode-width 0)
;; (setq sml/shorten-modes nil)
(setq sml/name-width '(0 . 44))

;;; Multiple-cursors

;; multiple-cursors setup
(use-package multiple-cursors
  :defer nil
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (add-hook 'prog-mode-hook #'multiple-cursors-mode)
  :config
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
         ("M-?" . flycheck-display-error-at-point)))
;;; nix

(use-package nix-mode)
(defun find-nix-shell ()
  "Search for the first shell.nix file to be found in the same directory as the current file or all ancestor directories."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (while (not (or (file-exists-p (concat dir "shell.nix"))
                    (string-equal dir "/")))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if (string-equal dir "/")
        (error "Couldn't find a shell.nix")
      (concat dir "shell.nix"))))
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
  (electric-indent-local-mode -1)
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
    :bind (("M-n" . flycheck-next-error)
           ("M-p" . flycheck-previous-error)
           ("M-?" . flycheck-display-error-at-point)))
  (use-package hindent)
  (defun my-haskell-mode-hook ()
    (structured-haskell-mode)
    (add-hook 'before-save-hook #'whitespace-cleanup)
    (electric-indent-mode -1)
    (electric-pair-mode -1)
    (intero-mode t))
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
        (delete-backward-char (1- (point)))
        (insert (fill-list pragmas ", " "{-# LANGUAGE " " #-}"))))))

;; ; Prevent ghci from looking for a cabal projection definition when
;; ; loading a file
;; (setq inferior-haskell-find-project-root nil)

;; ; Let GLFW-b open windows from GHCi.
;; (setq haskell-process-type 'cabal-repl)
;; (setq haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fno-ghci-sandbox"))

;; (eval-after-load "haskell-mode"
;;   '(progn
;;     (define-key haskell-mode-map (kbd "C-x C-d") nil)
;;     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
;;     ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;     ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

;;     (define-key haskell-mode-map (kbd "C-c M-.") nil)
;;     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; (defun ac/haskell-mode-hook ()
;;   ;(require 'ghc)
;;   ;(setq ghc-debug 't)
;;   (electric-indent-local-mode -1)
;;   ;(ghc-init) ;;; ghc-mod
;;   (company-mode)
;;   ;; (add-to-list 'company-backends
;;   ;;              '(company-ghc :with company-dabbrev-code))

;;   ;(custom-set-variables '(haskell-tags-on-save t))
;;   (turn-on-haskell-indent))

;; (add-hook 'haskell-mode-hook #'ac/haskell-mode-hook)

(add-to-list 'load-path "~/.emacs.d/misc")
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
(autoload 'haskell-latex-mode "haskell-latex")

;;; C++

(use-package helm-gtags
  :defer t
  :bind (:map helm-gtags-mode-map
         ("M-." . helm-gtags-find-tag)
         ("M-*" . helm-gtags-pop-stack))
  :config
  (setq helm-gtags-direct-helm-completing t
        helm-gtags-auto-update t
        helm-gtags-ignore-case t))

(defun my/c++-mode-hook ()
  (electric-indent-mode t)
  (electric-pair-mode t)
  (setq company-backends (delete 'company-semantic company-backends))
  (helm-gtags-mode 1))

(add-hook 'c++-mode-hook #'my/c++-mode-hook)

(use-package irony
  :defer t
  :load-path "/Users/acowley/src/irony-mode"
  :commands irony-mode
  :config
  (use-package company-irony
    :defer t
    :config
    (add-to-list 'company-backends 'company-irony))

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    (company-irony-setup-begin-commands))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package flycheck-irony
    :defer t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    (add-hook 'irony-mode-hook #'flycheck-mode)))

;;; python
(defun ac/python-hook ()
  (setq python-indent-offset 2))

(add-hook 'python-mode-hook #'ac/python-hook)

;;; File mode associtions
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

; OpenCL code uses c-mode
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



;;; company-mode
(use-package company
  :defer t
  :config
  (setq company-idle-delay 0.1)
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company)
  (defun ac/company-text-mode ()
    ;; (add-to-list 'company-backends 'company-ispell)
    )
  (add-hook 'text-mode-hook #'ac/company-text-mode))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'prog-mode-hook 'company-mode)



;;; gpg
(defun pinentry-emacs (desc prompt ok error)
  "Interface for entering a password into gpg-agent."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
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
;; (eval-after-load "znc"
;;   '(let ((password (let ((auth (auth-source-search :host "rasznc.local")))
;;                      (cond
;;                       ((null auth) (error "Couldn't find rasznc authinfo"))
;;                       (t (funcall (plist-get (car auth) :secret)))))))
;;     (set-variable
;;      'znc-servers
;;      `(("raspberrypi.local" 1234 t ((rasznc "acowley/freenode" ,password)))))))

;;; twittering-mode
(add-hook 'twittering-mode-hook
          (lambda ()
            (variable-pitch-mode)
            (turn-on-visual-line-mode)
            (setq buffer-face-mode-face '(:family "Avenir Next"))
            (buffer-face-mode)
            (text-scale-adjust 1)))
(add-hook 'twittering-edit-mode-hook 'flyspell-mode)

(set-variable 'twittering-use-master-password t)

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
  (global-set-key (kbd "M-[") #'corral-brackets-backword)
  (global-set-key (kbd "M-]") #'corral-backents-forward))

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
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;;; PlatformIO

;; From github user @pashky
(add-hook 'projectile-mode-hook
          (lambda ()
            (projectile-register-project-type 'platformio '("platformio.ini") "platformio run" nil "platformio run -t upload")))

;;; redprl
(use-package redprl
  :config
  (setq redprl-command "/nix/store/p0m23jnrpgypin961spa4c3gn0xmd3x1-redprl-2016-09-22/bin/redprl"
        flycheck-redprl-executable "/nix/store/p0m23jnrpgypin961spa4c3gn0xmd3x1-redprl-2016-09-22/bin/redprl"))

;;; osx-dictionary
(use-package osx-dictionary
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (setq osx-dictionary-dictionary-choice '("Dictionary" "Thesaurus")))
;;; graphviz-dot-mode
(use-package graphviz-dot-mode :defer t)
;;; toml
(use-package toml-mode :defer t)
;;; markdown-mode
(use-package markdown-mode :defer t)
;;; Private Configuration
;; Set up paths for org files, etc.
(load "~/.emacsPrivate.el")

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/usr/local/opt/android-sdk")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command outshine-self-insert-command)))
 '(company-ghc-autoscan t)
 '(company-ghc-show-info (quote oneline))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "0f92b9f1d391caf540ac746bc251ea00a55f29e20a411460eb6d8e49892ddef9" "d94eec01b45c7dc72e324af86fd2858e97c92220c195b5dbae5f8fd926a09cec" "1a53efc62256480d5632c057d9e726b2e64714d871e23e43816735e1b85c144c" "0f98f9c2f1241c3b6227af48dc96e708ec023dd68363edb5d36dc7beaad64c23" "13270e81a07dac4aeb1efefb77b9e61919bb3d69da7253ade632856eed65b8a2" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "0a1a7f64f8785ffbf5b5fbe8bca1ee1d9e1fb5e505ad9a0f184499fe6747c1af" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c810219104d8ff9b37e608e02bbc83c81e5c30036f53cab9fe9a2163a2404057" "d46b5a32439b319eb390f29ae1810d327a2b4ccb348f2018b94ff22f410cb5c4" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "2d20b505e401964bb6675832da2b7e59175143290dc0f187c63ca6aa4af6c6c1" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "d22a6696fd09294c7b1601cb2575d8e5e7271064453d6fa77ab4e05e5e503cee" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(dired-dwim-target t)
 '(dired-recursive-deletes (quote always))
 '(doc-view-resolution 200)
 '(doc-view-scale-internally nil)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-server-auto-reconnect nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GPG_AGENT_INFO")))
 '(fci-rule-color "#49483E")
 '(flycheck-ghc-args (quote ("-Wall")))
 '(flycheck-swift-sdk-path
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
 '(flycheck-swift-target "x86_64-macosx10.11")
 '(ghc-doc-browser-function (quote ghc-browse-url-safari))
 '(ghc-use-nix-shell (quote (quote t)))
 '(global-visual-fill-column-mode t)
 '(haskell-indent-offset 2)
 '(helm-mu-gnu-sed-program "gnused")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
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
 '(outshine-preserve-delimiter-whitespace t)
 '(outshine-use-speed-commands t)
 '(pop-up-windows nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   (quote
    ((org-html-htmlize-output-type . css)
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
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(swift-repl-executable "xcrun swift -target x86_64-macosx10.11")
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#75715E" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#75715E" :slant italic))))
 '(font-lock-type-face ((t (:foreground "dodger blue" :slant normal :weight bold))))
 '(highlight ((t (:inverse-video nil))))
 '(mu4e-header-value-face ((t (:inherit font-lock-doc-face :foreground "Green"))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "light green" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :box (:line-width 1 :style released-button) :weight bold :height 1.3)))))


;;; File Local Variables
;; Local Variables:
;; mode: emacs-lisp
;; eval: (org-overview)
;; End:
