;;; omp-emacs-tools.el --- Emacs tools bridge for omp extensions -*- lexical-binding: t -*-

;; This file provides the `cet-eval-to-file` bridge and the tool functions
;; from claude-mcp-tools.el, stripped of the Claude Code IDE dependency.
;; Load it once, then call tools via emacsclient --eval.

;; --- eval-to-file bridge ------------------------------------------------

(defun cet-eval-to-file (out-file form)
  "Eval FORM and write its string result to OUT-FILE.
Errors are caught and written as text so the caller always gets output."
  (let ((result (condition-case err (eval form t)
                  (error (format "Error: %s" (error-message-string err))))))
    (with-temp-file out-file
      (insert (if (stringp result) result (format "%S" result))))
    nil))

;; --- image viewer -------------------------------------------------------

(defvar omp-image-list nil "List of image filepaths currently being viewed.")
(defvar omp-image-index 0 "Current index in `omp-image-list'.")
(defvar omp-image-buffer-name "*omp-images*" "Buffer name for displaying images.")

(defun omp-image--display-current ()
  "Display the current image from the list without stealing focus."
  (when (and omp-image-list
             (>= omp-image-index 0)
             (< omp-image-index (length omp-image-list)))
    (let* ((filepath (nth omp-image-index omp-image-list))
           (buf (get-buffer omp-image-buffer-name)))
      (unless buf
        (setq buf (get-buffer-create omp-image-buffer-name))
        (with-current-buffer buf
          (setq buffer-save-without-query t)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq buffer-file-name filepath)
          (insert-file-contents filepath)
          (image-mode)
          (setq header-line-format
                (format " Image %d/%d: %s"
                        (1+ omp-image-index)
                        (length omp-image-list)
                        (file-name-nondirectory filepath)))
          (set-visited-file-modtime)
          (set-buffer-modified-p nil)
          (setq-local auto-revert-verbose nil)
          (auto-revert-mode 1))
        (setq-local revert-without-query '(".*"))
        (setq-local buffer-stale-function (lambda (&rest _) nil))
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (setq-local auto-revert-verbose nil)
        (auto-revert-mode 1))
      (unless (get-buffer-window buf)
        (display-buffer buf '(nil (inhibit-same-window . t))))
      (format "Displaying image %d/%d: %s"
              (1+ omp-image-index)
              (length omp-image-list)
              filepath))))

(defun omp-open-image (filepath)
  "Open a single image, replacing the current list."
  (let ((full-path (expand-file-name filepath)))
    (if (file-exists-p full-path)
        (progn
          (setq omp-image-list (list full-path))
          (setq omp-image-index 0)
          (omp-image--display-current))
      (format "Error: File not found: %s" full-path))))

(defun omp-open-images (filepaths)
  "Open multiple images for cycling through."
  (let ((full-paths
         (mapcar (lambda (p) (expand-file-name p)) filepaths))
        (missing '()))
    (dolist (p full-paths)
      (unless (file-exists-p p)
        (push p missing)))
    (if missing
        (format "Error: No valid files found. Missing: %s" (string-join (nreverse missing) ", "))
      (setq omp-image-list full-paths)
      (setq omp-image-index 0)
      (omp-image--display-current))))

;; --- pulse (flash) region ------------------------------------------------

(require 'pulse)

(defun omp-pulse-region (filepath start-line end-line)
  "Open FILEPATH and briefly pulse lines START-LINE to END-LINE."
  (let ((full-path (expand-file-name filepath)))
    (if (not (file-exists-p full-path))
        (format "Error: File not found: %s" full-path)
      (let ((buf (find-file-noselect full-path)))
        (unless (get-buffer-window buf)
          (display-buffer buf '(nil (inhibit-same-window . t))))
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start-pos (point)))
            (forward-line (- end-line start-line -1))
            (let ((end-pos (point)))
              (pulse-momentary-highlight-region start-pos end-pos)
              (when-let* ((win (get-buffer-window buf)))
                (set-window-point win start-pos)
                (with-selected-window win
                  (recenter)))
              (format "Highlighted %s lines %d-%d"
                      (file-name-nondirectory full-path)
                      start-line end-line))))))))

;; --- org-mode tools -----------------------------------------------------

(defun omp-org--call-with-file (filepath fn)
  "Call FN in buffer containing FILEPATH's contents."
  (let* ((full-path (expand-file-name filepath))
         (existing-buf (find-buffer-visiting full-path)))
    (if existing-buf
        (with-current-buffer existing-buf
          (save-excursion
            (funcall fn)))
      (with-temp-buffer
        (insert-file-contents full-path)
        (org-mode)
        (funcall fn)))))

(defun omp-org-list-headings (filepath &optional max-depth)
  "List all headings in FILEPATH up to MAX-DEPTH."
  (let ((full-path (expand-file-name filepath)))
    (if (not (file-exists-p full-path))
        (format "Error: File not found: %s" full-path)
      (omp-org--call-with-file full-path
        (lambda ()
          (let ((headings '())
                (max-depth (or max-depth 99)))
            (goto-char (point-min))
            (org-map-entries
             (lambda ()
               (let ((level (org-current-level))
                     (title (org-get-heading t t t t))
                     (line (line-number-at-pos)))
                 (when (<= level max-depth)
                   (push (format "%d:%s%s"
                                 line
                                 (make-string (* 2 (1- level)) ?\s)
                                 title)
                         headings)))))
            (string-join (nreverse headings) "\n")))))))

(defun omp-org-get-subtree (filepath heading-pattern)
  "Get entire subtree under heading matching HEADING-PATTERN."
  (let ((full-path (expand-file-name filepath)))
    (if (not (file-exists-p full-path))
        (format "Error: File not found: %s" full-path)
      (omp-org--call-with-file full-path
        (lambda ()
          (goto-char (point-min))
          (let ((found nil)
                (result nil))
            (org-map-entries
             (lambda ()
               (when (and (not found)
                          (string-match-p heading-pattern
                                          (org-get-heading t t t t)))
                 (setq found t)
                 (setq result
                       (buffer-substring-no-properties
                        (point)
                        (org-end-of-subtree t t))))))
            (or result (format "No heading matching '%s' found" heading-pattern))))))))

(defun omp-org-get-heading-at-line (filepath line-number)
  "Get the subtree at LINE-NUMBER in FILEPATH."
  (let ((full-path (expand-file-name filepath)))
    (if (not (file-exists-p full-path))
        (format "Error: File not found: %s" full-path)
      (omp-org--call-with-file full-path
        (lambda ()
          (goto-char (point-min))
          (forward-line (1- line-number))
          (if (org-at-heading-p)
              (buffer-substring-no-properties
               (point)
               (org-end-of-subtree t t))
            (format "No heading at line %d" line-number)))))))

(defun omp-org-search-headings (filepath pattern)
  "Search for headings matching PATTERN in FILEPATH."
  (let ((full-path (expand-file-name filepath)))
    (if (not (file-exists-p full-path))
        (format "Error: File not found: %s" full-path)
      (omp-org--call-with-file full-path
        (lambda ()
          (goto-char (point-min))
          (let ((matches '()))
            (org-map-entries
             (lambda ()
               (let ((heading (org-get-heading t t t t)))
                 (when (string-match-p pattern heading)
                   (push (format "%d: %s"
                                 (line-number-at-pos)
                                 heading)
                         matches)))))
            (if matches
                (string-join (nreverse matches) "\n")
              (format "No headings matching '%s'" pattern))))))))

(provide 'omp-emacs-tools)
;;; omp-emacs-tools.el ends here
