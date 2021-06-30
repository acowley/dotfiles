(setq gc-cons-threshold 100000000; ie 100mb, default is 800kb
      gc-cons-percentage 0.6)
(setq frame-inhibit-implied-resize t)
(tool-bar-mode -1)
(setq package-enable-at-startup nil)

;; From https://github.com/geza-herman/emacs/tree/fast-emacs

;; remove searching for .gz files, as all my packages are uncompressed
;; this halves how many files emacs tries to open (if you have a lot of
;; packages, emacs executes a lot of failing file opens)
(setq jka-compr-load-suffixes nil)
(jka-compr-update)

;; also set file-name-handler-alist to nil during 'require'
;; this is needed because of deferred package loading
;; (advice-add 'require :around (lambda (orig-fun &rest args)
;;   (let ((file-name-handler-alist))
;;     (apply orig-fun args))))

;; For gccemacs
(setq native-comp-deferred-compilation nil)
