;;; ob-qalc.el --- org-babel functions for qalc evaluation -*- lexical-binding: t; -*-

;; Org-babel support for `qalc' (Qalculate! CLI), including variables
;; that persist across blocks and across Emacs sessions via a defs
;; file per :session name.

;;; Code:

(require 'ob)

(defvar org-babel-default-header-args:qalc
  '((:session . "default")
    (:results . "output"))
  "Default header args for qalc source blocks.")

(defcustom ob-qalc-session-directory
  (expand-file-name "qalc-sessions" user-emacs-directory)
  "Directory holding per-session qalc variable definition files."
  :type 'directory
  :group 'org-babel)

(defun ob-qalc--defs-file (session)
  "Return the definitions file path for SESSION, creating its directory."
  (unless (file-directory-p ob-qalc-session-directory)
    (make-directory ob-qalc-session-directory t))
  (let ((file (expand-file-name (concat session ".qalc") ob-qalc-session-directory)))
    (unless (file-exists-p file)
      (write-region "" nil file))
    file))

(defun ob-qalc--assignment-name (line)
  "If LINE is a qalc assignment (NAME = EXPR), return NAME, else nil."
  (when (string-match "\\`[ \t]*\\([[:alpha:]_][[:alnum:]_]*\\)[ \t]*=[^=]" line)
    (match-string 1 line)))

(defun ob-qalc--update-defs (defs-file name line)
  "Insert or replace the definition of NAME with LINE in DEFS-FILE."
  (let* ((existing (if (file-exists-p defs-file)
                        (with-temp-buffer
                          (insert-file-contents defs-file)
                          (split-string (buffer-string) "\n" t))
                      nil))
         (kept (seq-remove (lambda (l) (equal (ob-qalc--assignment-name l) name))
                            existing)))
    (with-temp-file defs-file
      (dolist (l kept) (insert l "\n"))
      (insert line "\n"))))

(defun ob-qalc--vars-to-lines (vars)
  "Turn org-babel VARS alist into qalc assignment lines."
  (mapcar (lambda (pair) (format "%s = %s" (car pair) (cdr pair))) vars))

(defun ob-qalc--run-line (defs-file line)
  "Evaluate LINE with qalc using DEFS-FILE, returning trimmed output."
  (string-trim
   (with-output-to-string
     (call-process "qalc" nil (list standard-output nil) nil "-f" defs-file "-t" line))))

(defun org-babel-execute:qalc (body params)
  "Execute a block of qalc code with org-babel."
  (let* ((session (cdr (assq :session params)))
         (session (if (or (null session) (equal session "none")) "default" session))
         (defs-file (ob-qalc--defs-file session))
         (var-lines (ob-qalc--vars-to-lines (org-babel--get-vars params)))
         (body-lines (seq-filter (lambda (l) (not (string-blank-p l)))
                                  (split-string (org-babel-expand-body:generic body params) "\n")))
         result)
    ;; Persist any :var bindings before running the body.
    (dolist (line var-lines)
      (ob-qalc--update-defs defs-file (ob-qalc--assignment-name line) line))
    (dolist (line body-lines)
      (setq result (ob-qalc--run-line defs-file line))
      (when-let* ((name (ob-qalc--assignment-name line)))
        (ob-qalc--update-defs defs-file name line)))
    result))

(defun org-babel-expand-body:qalc (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-body:generic body params))

(defun ob-qalc--session-names ()
  "Return the list of existing qalc session names."
  (if (file-directory-p ob-qalc-session-directory)
      (mapcar #'file-name-sans-extension
              (directory-files ob-qalc-session-directory nil "\\.qalc\\'"))
    nil))

(defun qalc-reset-session (session)
  "Delete the persisted variables for SESSION, starting it fresh."
  (interactive (list (completing-read "Reset qalc session: " (ob-qalc--session-names) nil t)))
  (let ((file (expand-file-name (concat session ".qalc") ob-qalc-session-directory)))
    (if (file-exists-p file)
        (progn
          (delete-file file)
          (message "Reset qalc session %s" session))
      (message "No such qalc session: %s" session))))

(provide 'ob-qalc)
;;; ob-qalc.el ends here
