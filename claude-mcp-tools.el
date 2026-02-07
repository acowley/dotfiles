;;; claude-mcp-tools.el --- MCP tools for Claude Code -*- lexical-binding: t -*-

;;; Image Viewer for Claude Code IDE

;; State for tracking images
(defvar my-claude-image-list nil
  "List of image filepaths currently being viewed.")

(defvar my-claude-image-index 0
  "Current index in `my-claude-image-list'.")

(defvar my-claude-image-buffer-name "*claude-images*"
  "Buffer name for displaying images.")

(defun my-claude-image--display-current ()
  "Display the current image from the list without stealing focus."
  (when (and my-claude-image-list
             (>= my-claude-image-index 0)
             (< my-claude-image-index (length my-claude-image-list)))
    (let* ((filepath (nth my-claude-image-index my-claude-image-list))
           (buf (get-buffer my-claude-image-buffer-name)))
      (unless buf
        (setq buf (get-buffer-create my-claude-image-buffer-name))
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
                        (1+ my-claude-image-index)
                        (length my-claude-image-list)
                        (file-name-nondirectory filepath)))
          ;; These must come AFTER image-mode and other setup
          (set-visited-file-modtime)
          (set-buffer-modified-p nil)
          ;; Revert silently without prompting
          (setq-local auto-revert-verbose nil)
          (setq-local revert-buffer-in-progress-p nil)
          (auto-revert-mode 1))
        ;; Critical: set up revert to not prompt
        (setq-local revert-without-query '(".*"))
        (setq-local buffer-stale-function (lambda (&rest _) nil))
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        ;; Use auto-revert without prompting
        (setq-local auto-revert-verbose nil)
        (auto-revert-mode 1))
      (unless (get-buffer-window buf)
        (display-buffer buf '(nil (inhibit-same-window . t))))
      (format "Displaying image %d/%d: %s"
              (1+ my-claude-image-index)
              (length my-claude-image-list)
              filepath))))

(defun my-claude-open-image (filepath)
  "Open a single image, replacing the current list."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (file-exists-p full-path)
          (progn
            (setq my-claude-image-list (list full-path))
            (setq my-claude-image-index 0)
            (my-claude-image--display-current))
        (format "Error: File not found: %s" full-path)))))

(defun my-claude-open-images (filepaths)
  "Open multiple images for cycling through.
FILEPATHS should be a list of paths."
  (claude-code-ide-mcp-server-with-session-context nil
    (let* ((paths (if (stringp filepaths)
                      (json-parse-string filepaths :array-type 'list)
                    filepaths))
           (full-paths (mapcar (lambda (p) (expand-file-name p default-directory)) paths))
           (existing (seq-filter #'file-exists-p full-paths))
           (missing (seq-remove #'file-exists-p full-paths)))
      (if existing
          (progn
            (setq my-claude-image-list existing)
            (setq my-claude-image-index 0)
            (let ((result (my-claude-image--display-current)))
              (if missing
                  (format "%s\nWarning: %d file(s) not found: %s"
                          result (length missing) (string-join missing ", "))
                result)))
        (format "Error: No valid files found. Missing: %s" (string-join missing ", "))))))

(defun my-claude-next-image ()
  "Show the next image in the list."
  (interactive)
  (if (null my-claude-image-list)
      "No images loaded"
    (setq my-claude-image-index
          (mod (1+ my-claude-image-index) (length my-claude-image-list)))
    (my-claude-image--display-current)))

(defun my-claude-previous-image ()
  "Show the previous image in the list."
  (interactive)
  (if (null my-claude-image-list)
      "No images loaded"
    (setq my-claude-image-index
          (mod (1- my-claude-image-index) (length my-claude-image-list)))
    (my-claude-image--display-current)))

(defun my-claude-goto-image (index)
  "Jump to a specific image by INDEX (1-based)."
  (if (null my-claude-image-list)
      "No images loaded"
    (let ((idx (1- index)))             ; Convert to 0-based
      (if (and (>= idx 0) (< idx (length my-claude-image-list)))
          (progn
            (setq my-claude-image-index idx)
            (my-claude-image--display-current))
        (format "Error: Index %d out of range (1-%d)"
                index (length my-claude-image-list))))))

(defun my-claude-list-images ()
  "Return a list of currently loaded images."
  (if (null my-claude-image-list)
      "No images loaded"
    (format "Loaded %d images (currently showing #%d):\n%s"
            (length my-claude-image-list)
            (1+ my-claude-image-index)
            (string-join
             (cl-loop for path in my-claude-image-list
                      for i from 1
                      collect (format "  %d. %s%s"
                                      i
                                      (file-name-nondirectory path)
                                      (if (= i (1+ my-claude-image-index)) " <-- current" "")))
             "\n"))))

;; Register image tools with Claude Code
(claude-code-ide-make-tool
 :function #'my-claude-open-image
 :name "open_image"
 :description "Open a single image file in Emacs. Replaces any previously loaded images."
 :args '((:name "filepath"
                :type string
                :description "Path to the image file (absolute or relative to project)")))

(claude-code-ide-make-tool
 :function #'my-claude-open-images
 :name "open_images"
 :description "Open multiple images for cycling through. Pass a JSON array of paths."
 :args '((:name "filepaths"
                :type string
                :description "JSON array of image paths, e.g. [\"img1.png\", \"img2.png\"]")))

(claude-code-ide-make-tool
 :function #'my-claude-next-image
 :name "next_image"
 :description "Show the next image in the loaded image list."
 :args '())

(claude-code-ide-make-tool
 :function #'my-claude-previous-image
 :name "previous_image"
 :description "Show the previous image in the loaded image list."
 :args '())

(claude-code-ide-make-tool
 :function #'my-claude-goto-image
 :name "goto_image"
 :description "Jump to a specific image by its number (1-based index)."
 :args '((:name "index"
                :type integer
                :description "Image number to display (1-based)")))

(claude-code-ide-make-tool
 :function #'my-claude-list-images
 :name "list_images"
 :description "List all currently loaded images and which one is being displayed."
 :args '())

;; Keybindings for manual cycling (optional)
(defvar my-claude-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'my-claude-next-image)
    (define-key map (kbd "p") #'my-claude-previous-image)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for claude image viewer.")

;; Apply keymap to the image buffer
(add-hook 'image-mode-hook
          (lambda ()
            (when (string= (buffer-name) my-claude-image-buffer-name)
              (use-local-map (make-composed-keymap
                              my-claude-image-mode-map
                              image-mode-map)))))
;;; MCP Tool to load and highlight a range from a file
(require 'pulse)

(defun my-claude-pulse-region (filepath start-line end-line)
  "Open FILEPATH and briefly pulse lines START-LINE to END-LINE."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (let ((buf (find-file-noselect full-path)))
          ;; Display without selecting
          (unless (get-buffer-window buf)
            (display-buffer buf '(nil (inhibit-same-window . t))))
          ;; Do the pulsing in the buffer
          (with-current-buffer buf
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((start-pos (point)))
              (forward-line (- end-line start-line -1))
              (let ((end-pos (point)))
                (pulse-momentary-highlight-region start-pos end-pos)
                ;; Scroll the window to show the region without selecting it
                (when-let ((win (get-buffer-window buf)))
                  (set-window-point win start-pos)
                  (with-selected-window win
                    (recenter)))
                (format "Highlighted %s lines %d-%d"
                        (file-name-nondirectory full-path)
                        start-line end-line)))))))))

(claude-code-ide-make-tool
 :function #'my-claude-pulse-region
 :name "flash_code"
 :description "Open a file and briefly flash/pulse/highlight a region to draw attention to it."
 :args '((:name "filepath"
                :type string
                :description "Path to the file")
         (:name "start_line"
                :type integer
                :description "Starting line number (1-based)")
         (:name "end_line"
                :type integer
                :description "Ending line number (1-based)")))

;;; Org-mode MCP Tools
(defun my-claude-org--call-with-file (filepath fn)
  "Call FN in buffer containing FILEPATH's contents.
Uses existing buffer if file is open, otherwise reads from disk."
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

(defun my-claude-org-list-headings (filepath &optional max-depth)
  "List all headings in FILEPATH up to MAX-DEPTH."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
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
              (string-join (nreverse headings) "\n"))))))))

(defun my-claude-org-get-heading-content (filepath heading-pattern)
  "Get content under heading matching HEADING-PATTERN in FILEPATH."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
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
                   (setq result (org-get-entry)))))
              (or result (format "No heading matching '%s' found" heading-pattern)))))))))

(defun my-claude-org-get-subtree (filepath heading-pattern)
  "Get entire subtree under heading matching HEADING-PATTERN."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
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
              (or result (format "No heading matching '%s' found" heading-pattern)))))))))

(defun my-claude-org-get-properties (filepath heading-pattern)
  "Get properties of heading matching HEADING-PATTERN."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
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
                   (setq result (org-entry-properties)))))
              (if result
                  (mapconcat (lambda (p) (format "%s: %s" (car p) (cdr p)))
                             result "\n")
                (format "No heading matching '%s' found" heading-pattern)))))))))

(defun my-claude-org-search-headings (filepath pattern)
  "Search for headings matching PATTERN in FILEPATH."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
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
                (format "No headings matching '%s'" pattern)))))))))

(defun my-claude-org-get-heading-at-line (filepath line-number)
  "Get the subtree at LINE-NUMBER in FILEPATH."
  (claude-code-ide-mcp-server-with-session-context nil
    (let ((full-path (expand-file-name filepath default-directory)))
      (if (not (file-exists-p full-path))
          (format "Error: File not found: %s" full-path)
        (my-claude-org--call-with-file full-path
          (lambda ()
            (goto-char (point-min))
            (forward-line (1- line-number))
            (if (org-at-heading-p)
                (buffer-substring-no-properties
                 (point)
                 (org-end-of-subtree t t))
              (format "No heading at line %d" line-number))))))))

;;;; Register the tools

(claude-code-ide-make-tool
 :function #'my-claude-org-list-headings
 :name "org_list_headings"
 :description "List all headings in an org file with line numbers. Returns indented outline showing document structure. Use this first to understand the structure of an org file."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "max_depth"
          :type integer
          :description "Maximum heading depth to include (optional)"
          :optional t)))

(claude-code-ide-make-tool
 :function #'my-claude-org-get-heading-content
 :name "org_get_heading_content"
 :description "Get the content (body text) under a specific heading, not including subheadings. Use when you need just the text of one section."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "heading_pattern"
          :type string
          :description "Regex pattern to match the heading title")))

(claude-code-ide-make-tool
 :function #'my-claude-org-get-subtree
 :name "org_get_subtree"
 :description "Get an entire subtree (heading + all content + all subheadings) under a matching heading. Use when you need a section and all its children."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "heading_pattern"
          :type string
          :description "Regex pattern to match the heading title")))

(claude-code-ide-make-tool
 :function #'my-claude-org-get-properties
 :name "org_get_properties"
 :description "Get all properties of a heading (from its property drawer)."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "heading_pattern"
          :type string
          :description "Regex pattern to match the heading title")))

(claude-code-ide-make-tool
 :function #'my-claude-org-search-headings
 :name "org_search_headings"
 :description "Search for headings matching a pattern. Returns matching headings with line numbers."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "pattern"
          :type string
          :description "Regex pattern to search for in heading titles")))

(claude-code-ide-make-tool
 :function #'my-claude-org-get-heading-at-line
 :name "org_get_heading_at_line"
 :description "Get the subtree starting at a specific line number. Use after org_list_headings to fetch a section by its line number."
 :args '((:name "filepath"
          :type string
          :description "Path to the org file")
         (:name "line_number"
          :type integer
          :description "Line number where the heading starts")))

(provide 'claude-mcp-tools)
;;; claude-mcp-tools.el ends here
