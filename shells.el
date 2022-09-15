;; -*- lexical-binding: t; -*-

(require 'project)

(defvar shells-frame nil)
(defvar shells-shells-buffer "*shells*")

;; Shells mode

(defvar shells-mode-map (make-sparse-keymap))
(bind-key "q" #'delete-frame 'shells-mode-map)

;;;###autoload
(define-minor-mode shells-mode "Shells mode."
  :keymap shells-mode-map)

(defun shells-make-frame ()
  (unless (frame-live-p shells-frame)
    (let ((frame (cl-remove-if-not
                  (lambda (frame)
                    (let ((windows (window-list frame)))
                      (and (= 1 (length windows))
                           (with-current-buffer (window-buffer (car windows))
                             (and (eq major-mode 'ibuffer-mode)
                                  (string= (buffer-name) shells-shells-buffer))))))
                  (frame-list))))
      (if frame
          (setq frame (car frame))
        (let* ((default-frame-alist
                 `((width . 120)
                   (height . 40)
                   (left-fringe . 1)
                   (right-fringe . 1)
                   (vertical-scroll-bars . nil)
                   (foreground-color . "#2A2A2A"))))
          (setq frame (make-frame-command))))
      (setq shells-frame frame))))

;; Utilities

(defun shells-project-name ()
  (if-let ((project-current (project-current)))
      (file-name-nondirectory
       (directory-file-name
        (car (project-roots project-current))))))

(defun shells-resolve-buffer ()
  (if-let ((project (shells-project-name)))
      (let* ((remote-p (file-remote-p default-directory 'host))
             (buf-name (format "*shell*/%s%s" project (if remote-p (format "@%s" remote-p) "")))
             (buffers (cl-remove-if-not
                       (lambda (buffer)
                         (and
                          ;; By the time resolving buffer name,
                          ;; `shell-mode' has not been applied.
                          ;; (eq (buffer-local-value 'major-mode buffer) 'shell-mode)
                          (string= (buffer-name buffer) buf-name)
                          (equal (file-remote-p
                                  (buffer-local-value 'default-directory buffer)
                                  'host)
                                 remote-p)))
                       (buffer-list))))
        (if buffers
            (car buffers)
          (generate-new-buffer buf-name)))
    (if-let ((buffers (cl-remove-if-not
                       (lambda (buffer)
                         (and (eq (buffer-local-value 'major-mode buffer) 'shell-mode)
                              (string= (buffer-local-value 'default-directory buffer)
                                       default-directory)))
                       (buffer-list))))
        (if buffers
            (car buffers)
          (generate-new-buffer "*shell*")))))

;; Commands

;;;###autoload
(defun shells-ibuffer-shells ()
  (interactive)
  (let ((default-directory "~"))
    (ibuffer nil shells-shells-buffer '((used-mode . shell-mode)))
    (with-current-buffer shells-shells-buffer
      (shells-mode))))

;;;###autoload
(defun shells-dwim (arg)
  (interactive "P")
  (if arg
      (shell (get-buffer-create (generate-new-buffer "*shell*")))
    (let ((buffer (shells-resolve-buffer)))
      (if (eq buffer (current-buffer))
          (call-interactively #'bury-buffer)
        (if buffer
            (shell buffer)
          (shell (generate-new-buffer "*shell*")))))))

;;;###autoload
(defun shells-switch-buffers ()
  (interactive)
  (shells-make-frame)
  (if (frame-focus-state shells-frame)
      (cond ((eq (buffer-name) shells-shells-buffer)
             (select-frame-set-input-focus (next-frame)))
            ((eq major-mode 'shell-mode)
             (shells-ibuffer-shells))
            (t (select-frame-set-input-focus (next-frame))))
    (progn (select-frame-set-input-focus shells-frame)
           (unless (eq (buffer-name) shells-shells-buffer)
             (shells-ibuffer-shells)))))

(provide 'shells)
