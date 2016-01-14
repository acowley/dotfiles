(require 'package)

;;; Before everything else

;; This has to be very early in initialization.
(defvar outline-minor-mode-prefix "\M-#")

;; Using a source checkout of ghc-mod
(add-to-list 'load-path "/Users/acowley/src/ghc-mod/elisp/")
(require 'ghc)

;;; Package setup

;; Make sure the packages I use are installed
(setq my-packages '(exec-path-from-shell 
                    ; ghc
                    haskell-mode
                    company company-ghc helm helm-ag
		    helm-company helm-swoop helm-dash
                    outorg
                    outshine
                    htmlize
                    impatient-mode
                    auctex
                    irony company-irony
                    powerline smart-mode-line smart-mode-line-powerline-theme
                    monokai-theme markdown-mode
                    session
                    projectile helm-projectile ag
                    nix-mode
		    ;git-commit-mode git-rebase-mode magit
		    magit
                    glsl-mode yaml-mode vagrant-tramp cmake-mode
                    buffer-move multiple-cursors
                    corral
                    visual-fill-column
                    ;; Use the terminal-notifier program on OS X
                    erc-hl-nicks erc-terminal-notifier 
                    jonprl-mode
                    god-mode
                    tuareg flycheck-ocaml))

; If we run package-initialize, then add-to-list melpa, the
; package-install invocation will fail. We need the package-archives
; list setup before calling package-initialize.
(setq package-archives '(;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
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

;; Use the exec-path-from-shell package to set the PATH
(when (memq window-system '(mac ns))
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

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

;; helm-swoop keybindings
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

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

(put 'dired-find-alternate-file 'disabled nil)

;(load-theme 'monokai t)
;(load-theme 'darktooth t)

;; This is an attempt to prevent recentf (that keeps track of recent
;; files) from stat'ing remote files.
(setq recentf-keep '(file-remote-p file-readable-p))

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

(setq visual-line-fringe-indicators '(left-curly-arrow nil))

;;;; variable-pitch-mode
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (turn-on-visual-line-mode)
            (variable-pitch-mode)
            (setq buffer-face-mode-face '(:family "Avenir Next"))
            (buffer-face-mode)
            (text-scale-adjust 1)))

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-visual-line-mode)
            (variable-pitch-mode)
            (setq buffer-face-mode-face '(:family "Avenir Next"))
            (buffer-face-mode)
            (text-scale-adjust 1)))


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
  (comment-dwim nil))

;;; impatient-mode

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
  (imp-set-user-filter #'markdown-html))
  

;;; Org-mode

(add-hook 'org-mode-hook
          (lambda () (progn
                       (setq org-src-fontify-natively t)
                       (setq org-use-speed-commands t)
                       (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((haskell . t) (ditaa . t) (sh . t) (emacs-lisp . t)
                          (C . t) (js . t) (ipython . t)))

                       ;; Disable variable-pitch-mode in tables
                       (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

                       ;; display/update images in the buffer after I evaluate
                       (add-hook 'org-babel-after-execute-hook
                                 'org-display-inline-images
                                 'append)

                       ;; Don't fight the bindings that use
                       ;; shift-arrow to move focus between windows.
                       (define-key org-mode-map (kbd "S-<left>") nil)
                       (define-key org-mode-map (kbd "S-<right>") nil)
                       (define-key org-mode-map (kbd "S-<up>") nil)
                       (define-key org-mode-map (kbd "S-<down>") nil))))

(setq org-directory "~/org")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

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

;; For leuven-theme
;; Fontify the whole line for headings (with a background color).
(setq org-fontify-whole-heading-line t)

;; Use fixed-width fonts where appropriate
;; From: https://yoo2080.wordpress.com/2013/05/30/monospace-font-in-tables-and-source-code-blocks-in-org-mode-proportional-font-in-other-parts/
(defun adjoin-to-list-or-symbol (element list-or-symbol)
  (require 'cl)
  (adjoin element (if (not (listp list-or-symbol))
                      (list list-or-symbol)
                      list-or-symbol)))

(eval-after-load "org"
  '(mapc (lambda (face)
           (set-face-attribute face nil
                               :inherit (adjoin-to-list-or-symbol
                                          'fixed-pitch
                                          (face-attribute face :inherit))))
         ;(list 'org-code 'org-block 'org-table 'org-block-background)))
         (list 'org-code 'org-block 'org-table)))

(eval-after-load "font-latex"
  '(mapc (lambda (face)
           (set-face-attribute face nil
                               :inherit (adjoin-to-list-or-symbol
                                          'fixed-pitch
                                          (face-attribute face :inherit))))
         (list 'font-latex-math-face 'font-latex-verbatim-face
               'font-lock-keyword-face)))

;;;; Outshine

;; (require 'outshine)
;; (require 'outorg)

; (add-to-list 'load-path "~/Documents/Projects/outorg/")
; (add-to-list 'load-path "~/Documents/Projects/outshine/")
(add-hook 'outline-minor-mode-hook (lambda ()
                                     (require 'outshine)
                                     (outshine-hook-function)))
(add-hook 'prog-mode-hook 'outline-minor-mode)

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
         "* TODO %?\n  %i\n")
        ("p" "Project Task" entry (file+headline (find-project-notes) "Tasks")
         "* TODO %?\n  %i\n  %a")))

;;;; Encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "D50A574B")

;;; Helm

(add-hook 'helm-mode-hook
          (lambda ()
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; use TAB for action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions
(setq
 helm-candidate-number-limit 100
 helm-quick-update t
 helm-M-x-requires-pattern 3 ; Require at least one character
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
                             helm-source-locate))))

;; ido offers a nicer UI for switching between open buffers
(add-hook 'helm-mode-hook
	  (lambda ()
	    (add-to-list 'helm-completing-read-handlers-alist
			 '(switch-to-buffer . ido))))
 
(helm-mode t)
(ido-mode -1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c i") 'helm-imenu)

(projectile-global-mode)
(setq projectile-enable-caching t)

(defun projectile-helm-dash ()
  "Set the helm-dash docsets path to the 'docsets' directory
under the current project's root directory."
  (interactive)
  (require 'helm-dash)
  (require 'projectile)
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-docsets-path (concat (projectile-project-root) "docsets"))
  (message (format "Loaded docsets for %s" (projectile-project-name))))


;;; god-mode

(require 'god-mode)

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

;; On OS X, set "caps lock" to no action in system preferences, then
;; use the Seil app to rebind "caps lock" to f9.
(global-set-key (kbd "<f9>") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)

;; Easier to use with god-mode
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-s") #'helm-swoop) ; instead of isearch-forward

;; I swap these becuase I use switch-to-buffer much more frequently
;; and prefer it to have the simpler binding.
(global-set-key (kbd "C-x b") #'list-buffers)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)

(defun ac/god-mode-toggle ()
  "Set the mode line to a white background when god-mode is
active; black when inactive."
  (if god-local-mode
      (progn
        (set-face-background 'mode-line "white")
        (set-face-background 'mode-line-inactive "white")
        (hl-line-mode 1))
      (set-face-background 'mode-line "black")
      (set-face-background 'mode-line-inactive "black")
      (unless (eq major-mode 'mu4e-headers-mode) (hl-line-mode -1))))

(add-hook 'god-mode-enabled-hook #'ac/god-mode-toggle)
(add-hook 'god-mode-disabled-hook #'ac/god-mode-toggle)

(defun ac/god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook #'ac/god-toggle-on-overwrite)

;;; Email
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq 
  mu4e-maildir "~/.mail"
  mu4e-html2text-command  "/usr/local/bin/w3m -T text/html"
  mu4e-mu-binary "/usr/local/bin/mu"

  ;; allow for updating mail using 'U' in the main view:
  ; mu4e-get-mail-command "/usr/local/bin/mbsync -a"
  mu4e-get-mail-command "~/.nix-profile/bin/mbsync -a"

  ;; gmail folder setup
  ;mu4e-drafts-folder "/gmail/drafts"
  mu4e-drafts-folder "/mu4e/drafts"
  mu4e-sent-folder   "/gmail/sent"
  mu4e-trash-folder  "/gmail/trash"

  mu4e-headers-skip-duplicates t
  mu4e-compose-dont-reply-to-self t)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/gmail/Inbox"   . ?i)
       ("/gmail/sent"    . ?s)
       ("/gmail/trash"   . ?t)
       ("/gmail/archive" . ?a)))

;; something about ourselves
(setq
   user-mail-address "acowley@gmail.com"
   user-full-name  "Anthony Cowley"
   mu4e-compose-signature-auto-include nil
   mu4e-compose-signature nil
   mu4e-change-filenames-when-moving t)

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Auto-complete contact email addresses
;; We don't want line breaks added to emails we compose
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (company-mode)
            (turn-off-auto-fill)
            (variable-pitch-mode)
            (turn-on-visual-line-mode)
            (setq buffer-face-mode-face '(:family "Avenir Next"))
            (buffer-face-mode)
            (text-scale-adjust 1)))

;; Add a view in browser action. Trigger with "aV"
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; NOTE: deleting a message in Gmail is accomplished by moving to the
;; trash folder. "Marking for deletion" actually archives the message.
(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types) (imagemagick-register-types))
;(setq mu4e-view-prefer-html t)
;(setq mu4e-html2text-command "html2text -utf8 -width 72")

;; Automatically update every 10 minutes and pop up a notification if
;; the index changed.
(defun take (n lst)
  (loop repeat n for x in lst collect x))

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

(setq mu4e-update-interval 600)
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
              my-mu4e-account-alist))

;;; smart-mode-line (powerline)

(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Replace ":Doc:Projects/Foo/blah.hs" with ":Foo:blah.hs"
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/\\([^/]*\\)/" ":\\1:") t)
(sml/apply-theme 'smart-mode-line-powerline)

;;; Multiple-cursors

;; multiple-cursors setup
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun ac/insert-numbers1 ()
  "Insert a number at each cursor counting up from 1."
  (interactive)
  (mc/insert-numbers 1))

;;; Buffer-move

;; buffer-move setup
(require 'buffer-move)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)

;;; haskell

;; Using a source checkout of ghc-mod
; (add-to-list 'load-path "~/src/ghc-mod/elisp/")

; Prevent ghci from looking for a cabal projection definition when
; loading a file
(setq inferior-haskell-find-project-root nil)

; Let GLFW-b open windows from GHCi.
(setq haskell-process-type 'cabal-repl)
(setq haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fno-ghci-sandbox"))

(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    ;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    ;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(defun ac/haskell-mode-hook ()
  (require 'ghc)
  (electric-indent-local-mode -1)
  (ghc-init) ;;; ghc-mod
  (company-mode)
  (add-to-list 'company-backends
               '(company-ghc :with company-dabbrev-code))
  ;(custom-set-variables '(haskell-tags-on-save t))
  (turn-on-haskell-indent))

(add-hook 'haskell-mode-hook #'ac/haskell-mode-hook)

(add-to-list 'load-path "~/.emacs.d/misc")
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
(autoload 'haskell-latex-mode "haskell-latex")

;;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (setq company-backends (delete 'company-semantic company-backends))))
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
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

;;; company-mode
(add-hook 'company-mode-hook
          (lambda ()
            ;(custom-set-variables '(company-idle-delay 0.0)) ; Always complete immediately
            (define-key company-mode-map (kbd "C-:") 'helm-company)
            (define-key company-active-map (kbd "C-:") 'helm-company)
            ))

(eval-after-load 'company-ghc (lambda ()
                                (company-ghc-turn-on-autoscan)
                                (setq company-ghc-show-info t)))

(defun ac/company-text-mode ()
  (add-to-list 'company-backends 'company-ispell))

(eval-after-load 'company
  (lambda ()
    (add-to-list 'company-backends 'company-irony)
    (add-hook 'text-mode-hook #'ac/company-text-mode)))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(add-hook 'prog-mode-hook 'company-mode)



;;; gpg
(defun pinentry-emacs (desc prompt ok error)
  "Interface for entering a password into gpg-agent."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
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
(setq corral-preserve-point t)
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
;;; JonPRL
(defun ac/jonprl-hook ()
  (setq jonprl-path "~/Documents/Projects/JonPRL/bin/jonprl"))
(add-hook 'jonprl-mode-hook #'ac/jonprl-hook)
;;; Private Configuration
;; Set up paths for org files, etc.
(load "~/.emacsPrivate.el")

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
    ("38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "0a1a7f64f8785ffbf5b5fbe8bca1ee1d9e1fb5e505ad9a0f184499fe6747c1af" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c810219104d8ff9b37e608e02bbc83c81e5c30036f53cab9fe9a2163a2404057" "d46b5a32439b319eb390f29ae1810d327a2b4ccb348f2018b94ff22f410cb5c4" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "2d20b505e401964bb6675832da2b7e59175143290dc0f187c63ca6aa4af6c6c1" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "d22a6696fd09294c7b1601cb2575d8e5e7271064453d6fa77ab4e05e5e503cee" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(dired-dwim-target t)
 '(doc-view-resolution 200)
 '(doc-view-scale-internally nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GPG_AGENT_INFO")))
 '(fci-rule-color "#49483E")
 '(ghc-doc-browser-function (quote ghc-browse-url-safari))
 '(global-visual-fill-column-mode t)
 '(haskell-indent-offset 2)
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
 '(magit-diff-use-overlays nil)
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-use-overlays nil)
 '(org-default-notes-file "~/org/home.org")
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
 '(org-image-actual-width nil)
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-src-preserve-indentation t)
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
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
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
\\end{align*}" ""))))
 '(outshine-preserve-delimiter-whitespace t)
 '(outshine-use-speed-commands t)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".cabal-sandbox" ".cabbages")))
 '(projectile-ignored-projects (quote ("~/")))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "*.cabal")))
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate lambda
                                 (lang body)
                                 (not
                                  (string= lang "emacs-lisp")))
     (eval org-overview))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
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
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant normal))))
 '(mu4e-header-value-face ((t (:inherit font-lock-doc-face :foreground "Green"))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "light green" :weight bold)))))


;;; File Local Variables
;; Local Variables:
;; mode: emacs-lisp
;; eval: (org-overview)
;; End:

