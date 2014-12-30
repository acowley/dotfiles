(require 'package)

;;; Before everything else

;; This has to be very early in initialization.
(defvar outline-minor-mode-prefix "\M-#")

;;; Package setup

;; Make sure the packages I use are installed
(setq my-packages '(exec-path-from-shell 
                    ghc haskell-mode
                    company company-ghc helm helm-ag
		    helm-company
                    outorg
                    outshine
                    htmlize
                    auctex
                    powerline smart-mode-line smart-mode-line-powerline-theme
                    monokai-theme markdown-mode
                    session
                    projectile helm-projectile ag
                    git-commit-mode git-rebase-mode magit
                    glsl-mode yaml-mode vagrant-tramp cmake-mode
                    zenburn-theme buffer-move multiple-cursors))

; If we run package-initialize, then add-to-list melpa, the
; package-install invocation will fail. We need the package-archives
; list setup before calling package-initialize.
(setq package-archives '(;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
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

;; Use the exec-path-from-shell package to set the PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Move point to farthest possible position when scrolling the window
;; has reached the beginning or end of the buffer
(setq scroll-error-top-bottom t)

;; Support Cmd-up/down for top/bottom of buffer
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Preserve history between sessions
(add-hook 'after-init-hook 'session-initialize)

; yank will replace the active region's contents
(delete-selection-mode 1)

(setq c-default-style "bsd"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq default-directory "~/")

(setq mac-option-modifier 'meta)

;; Keyboard shortcut for aligning a region on a regexp
(global-set-key (kbd "C-x a r") 'align-regexp)

(when (fboundp 'server-mode) (funcall 'server-mode 1))

;; Revert buffers whose files have changed on disk
(global-auto-revert-mode t)

;; Disable the alarm bell on Quit (C-g)
(setq ring-bell-function 'ignore)

;; Turn auto-fill-mode on by default in text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(put 'downcase-region 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

(load-theme 'monokai t)

;; This is an attempt to prevent recentf (that keeps track of recent
;; files) from stat'ing remote files.
(setq recentf-keep '(file-remote-p file-readable-p))

;; Another options is
;; (require 'recentf)
;; (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
;; (recentf-mode 1)

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
(setq ispell-extra-args '("-a" "-i" "utf-8"))
(setq ispell-local-dictionary "en_US")
(setq ispell-personal-dictionary "~/.hunspell_en_US")

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;;;; Miscellaneous Helper Functions

;; Use with `impatient-mode' by running `M-x imp-set-user-filter' in a
;; markdown buffer, and supplying `markdown-html' as the argument.
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;;; Org-mode

(add-hook 'org-mode-hook
          (lambda () (progn
                       (setq org-src-fontify-natively t)
                       (setq org-use-speed-commands t)
                       (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((haskell . t) (ditaa . t) (sh . t))))))

(setq org-directory "~/org")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; outshine mode extensions to the outline minor mode
;; Doesn't work with haskell-mode + ghc-mod

;; (require 'outshine)
;; (require 'outorg)

;; (add-to-list 'load-path "~/Documents/Projects/outorg/")
;; (add-to-list 'load-path "~/Documents/Projects/outshine/")
(add-hook 'outline-minor-mode-hook (lambda ()
                                     (require 'outshine)
                                     (outshine-hook-function)))
(add-hook 'prog-mode-hook 'outline-minor-mode)

;;; Helm

(add-hook 'helm-mode-hook
          (lambda ()
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; use TAB for action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions
(setq
 helm-candidate-number-limit 25
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

(projectile-global-mode)

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

;;; Buffer-move

;; buffer-move setup
(require 'buffer-move)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)

;;; haskell

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

(add-hook 'haskell-mode-hook 
          (lambda ()
            (electric-indent-local-mode -1)
            (ghc-init) ;;; ghc-mod
            (company-mode)
            (custom-set-variables '(haskell-tags-on-save t))
            (turn-on-haskell-indent)))

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
(autoload 'haskell-latex-mode "haskell-latex")

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

;;; company-mode
(add-hook 'company-mode-hook
          (lambda ()
            (add-to-list 'company-backends
                         '(company-ghc :with company-dabbrev-code))
           
            ;(custom-set-variables '(company-idle-delay 0.0)) ; Always complete immediately
            (define-key company-mode-map (kbd "C-:") 'helm-company)
            (define-key company-active-map (kbd "C-:") 'helm-company)
            ))

(eval-after-load 'company-ghc (lambda ()
                                (company-ghc-turn-on-autoscan)
                                (setq company-ghc-show-info t)))

;;; Private Configuration
;; Set up paths for org files, etc.
(load "~/.emacsPrivate.el")

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-begin-commands t)
 '(company-ghc-autoscan t)
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(haskell-tags-on-save t)
 '(magit-use-overlays nil)
 '(org-default-notes-file "~/org/home.org")
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
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
 '(safe-local-variable-values
   (quote
    ((eval org-overview))))
 '(session-use-package t nil (session))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#75715E" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#75715E" :slant italic))))
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant normal))))
 '(haskell-constructor-face ((t (:inherit font-lock-type-face :slant normal)))))


;;; File Local Variables
;; Local Variables:
;; mode: emacs-lisp
;; eval: (org-overview)
;; End:
