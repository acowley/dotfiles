;;; cursor-context.el --- Read-only selected-window context for OpenCode -*- lexical-binding: t -*-

(defun opencode-emacs-cursor-context ()
  "Describe the selected window's buffer and point without changing editor state.
When the selected buffer is an agent-shell buffer, report the most recently
visited non-agent-shell buffer instead."
  (let* ((window (selected-window))
         (selected-buffer (window-buffer window))
         (agent-shell-buffer-p
          (lambda (buffer)
            (with-current-buffer buffer
              (or (derived-mode-p 'agent-shell-mode)
                  (equal (buffer-name) "OpenCode Agent")))))
         (buffer
          (if (funcall agent-shell-buffer-p selected-buffer)
              (catch 'recent-buffer
                (dolist (candidate (buffer-list))
                  (unless (or (eq candidate selected-buffer)
                              (minibufferp candidate)
                              (funcall agent-shell-buffer-p candidate))
                    (throw 'recent-buffer candidate)))
                selected-buffer)
            selected-buffer))
         (position
          (if (eq buffer selected-buffer)
              (window-point window)
            (with-current-buffer buffer (point)))))
    (with-current-buffer buffer
      (save-excursion
        ;; A window can retain a point distinct from the buffer's point.  Use
        ;; the selected window's point normally, and the saved buffer point for
        ;; the most recently visited buffer when agent-shell is selected.
        (goto-char position)
        (let* ((file buffer-file-name)
               (name (buffer-name))
               ;; The non-nil ABSOLUTE argument counts from the real buffer start
               ;; even when this buffer is narrowed.
               (line (line-number-at-pos (point) t))
               (column (1+ (current-column)))
               (heading
                (when (derived-mode-p 'org-mode)
                  (let ((position (point)))
                    (condition-case nil
                        (save-restriction
                          (widen)
                          (save-excursion
                            (goto-char position)
                            (org-back-to-heading t)
                            (org-get-heading t t t t)))
                      (error nil))))))
          (concat
           (format "Buffer: %s\nFile: %s\nLine: %d\nColumn: %d"
                   name (or file "(none)") line column)
           (if heading (format "\nOrg heading: %s" heading) "")))))))

(provide 'opencode-emacs-cursor-context)
;;; cursor-context.el ends here
