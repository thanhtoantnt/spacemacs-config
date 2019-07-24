(require 'notifications)

(defun yank-current-word-isearch ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(defun yank-word-minibuffer ()
  "Get word at point in original buffer and insert it to minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (skip-syntax-backward "w_")
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

(defun vc-status ()
  "Show version control status (git, hg) of the current project."
  (interactive)
  (defun find-vc-tool (dir)
    (cond ((string-match-p (regexp-quote "..") dir) nil)
          ((file-exists-p (expand-file-name ".git/config" dir)) 'Git)
          ((file-exists-p (expand-file-name ".hg/hgrc" dir)) 'Hg)
          (t (find-vc-tool (expand-file-name ".." dir)))))
  (let* ((path (if buffer-file-name buffer-file-name default-directory))
         (vc-tool (find-vc-tool path)))
    (cond ((eq vc-tool 'Hg)
           (call-interactively 'monky-status))
          ((eq vc-tool 'Git)
           (call-interactively 'magit-status))
          (t (message "Error: unknown version control tool")))))

(defun hook-change-major-mode ()
  ;; change some weird keys
  (keyboard-translate ?\C-\[ ?\H-\[)
  (keyboard-translate ?\C-i ?\H-i)
  (keyboard-translate ?\C-m ?\H-m)
  (define-key input-decode-map (kbd "C-M-m") (kbd "H-M-m"))
  (define-key input-decode-map (kbd "C-M-[") (kbd "H-M-["))
  (define-key input-decode-map (kbd "C-S-I") (kbd "H-I"))
  (define-key input-decode-map (kbd "C-S-M") (kbd "H-M")))

(defun hook-prog-mode ()
  "Hook to run in 'prog-mode'."
  (column-marker-3 -1)
  (whitespace-mode -1)
  (flyspell-mode -1)
  (linum-mode 1)
  (auto-fill-mode 1)
  (electric-pair-mode -1)
  (flycheck-mode -1))

(defun disable-ocp-indent ()
  (interactive)
  (setq indent-line-function 'indent-relative))
(defun enable-ocp-indent ()
  (interactive)
  (setq indent-line-function 'ocp-indent-line))

;; configuration
(defun toan/config-packages()
  (defun check-sub-string (sub-string super-string)
    (and super-string
         sub-string
         (string-match-p (regexp-quote sub-string) super-string)))

  ;; hook prog mod
  (add-hook 'LaTeX-mode-hook 'hook-prog-mode)
  (add-hook 'TeX-mode-hook 'hook-prog-mode)
  (add-hook 'tex-mode-hook 'hook-prog-mode)
  (add-hook 'prog-mode-hook 'hook-prog-mode)

  ;; anzu
  (defadvice anzu-query-replace (around wrap-query-replace activate)
    (save-excursion
      (goto-char (anzu--thing-begin t))
      ad-do-it
      (goto-char (point-min))
      ad-do-it))

  ;; js-comment-tab
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)

  (add-hook 'change-major-mode-hook 'hook-change-major-mode)

  ;; mode-line setting
  (setq powerline-default-separator 'bar)

  ;; company
  (setq company-idle-delay 300)

  ;; langtool
  ;; (setq langtool-language-tool-jar "~/.emacs.d/private/toan/languagetool.jar")
  ;; (require 'langtool)

  ;; ocp-indent
  (add-to-list 'load-path "/home/thanhtoantnt/.opam/4.07.0/share/emacs/site-lisp")
  (require 'ocp-indent)

  ;; songbird-mode
  (add-to-list 'auto-mode-alist '("\\.sb\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.ss\\'" . songbird))
  (add-to-list 'auto-mode-alist '("\\.slk\\'" . songbird))

    ;; advice the message function
  (defvar notify-command nil)
  (defun notify-output (type command output)
    (when command
      (notifications-notify
       :title (format "%s" command)
       :body (format "%s%s" (if (equal type 'error) "ERROR OCCURS!!!\n" "")
                     output)))
    (when notify-command
      (setq notify-command nil)))
  ;; for notification
  (defun notify-message (orig-fun &rest args)
    (let ((output (apply orig-fun args)))
      (cond
       ;; LaTeX
       ((check-sub-string "LaTeX errors" output)
        (notify-output 'error "LaTeX" output))
       ((or (check-sub-string "You should run LaTeX again" output)
            (check-sub-string "LaTeX: there were unresolved" output)
            (check-sub-string "LaTeX: successfully formatted" output))
        (notify-output 'success "LaTeX" output))
       ((check-sub-string "BibTeX finished" output)
        (notify-output 'success "BibTeX" output))
       ;; magit
       ((check-sub-string "Running git" output)
        (setq notify-command output))
       ((check-sub-string "Git finished" output)
        (notify-output 'success notify-command output))
       ((check-sub-string "Hit $ to see buffer magit-process" output)
        (notify-output 'error notify-command output))
       ;; Hg
       ((check-sub-string "Running hg" output)
        (setq notify-command output))
       ((check-sub-string "Hg finished" output)
        (notify-output 'success notify-command output))
       ((check-sub-string "Hg exited abnormally" output)
        (notify-output 'error notify-command output))
       ;; Compilation
       ((check-sub-string "Compiling: make" output)
        (setq notify-command output))
       ((check-sub-string "Compilation finished" output)
        (notify-output 'success notify-command output))
       ((check-sub-string "Compilation exited abnormally" output)
        (notify-output 'error notify-command output))
       ;;
       )))
  (advice-add 'message :around #'notify-message)
  
  (defun advise-compilation (orig-fun &rest args)
    (message "Compiling: %s" (car args))
    (apply orig-fun args))
  (advice-add 'compilation-start :around #'advise-compilation)

  )

;; configure shortcut key
(defun toan/config-keys()
  (global-set-key (kbd "C-c C-a") 'helm-do-ag)
  (global-set-key (kbd "TAB") 'indent-for-tab-command)
  (global-set-key (kbd "C-M-/") 'helm-company)             ;; auto completion
  (global-set-key (kbd "C-x g") 'vc-status)
  (global-set-key (kbd "C-x m") 'helm-mini)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "C-q") 'goto-last-change)
  (global-set-key (kbd "C-o") 'helm-semantic-or-imenu)

  (global-set-key (kbd "M-+") 'delete-other-windows)
  (global-set-key (kbd "M--") 'delete-window)
  (global-set-key (kbd "M-_") 'split-window-below)
  (global-set-key (kbd "M-|") 'split-window-right)

  (global-set-key [?\H-m] 'helm-mini)
  (global-set-key (kbd "H-M-m") 'projectile-find-file)
  (global-set-key [?\H-i] 'swiper)
  (global-set-key [?\H-I] 'swiper)

  (define-key isearch-mode-map (kbd "C-.") 'yank-current-word-isearch)
  (define-key swiper-map (kbd "C-.") 'yank-word-minibuffer)
  (define-key minibuffer-local-map (kbd "C-.") 'yank-word-minibuffer)

  ;; windmove
  (global-set-key (kbd "S-<left>") 'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down)
  (global-set-key (kbd "M-[") 'windmove-left)
  (global-set-key (kbd "M-]") 'windmove-right)
  (global-set-key (kbd "M-=") 'windmove-up)
  (global-set-key (kbd "M-'") 'windmove-down)
  (global-set-key (kbd "H-[") 'windmove-up)
  (global-set-key (kbd "C-]") 'windmove-down)

  (global-set-key (kbd "M-%") 'anzu-query-replace)

  ;; buffermove
  (global-set-key (kbd "H-M-[") 'previous-buffer)
  (global-set-key (kbd "C-M-]") 'next-buffer)
  (global-set-key (kbd "C-M-{") 'winner-undo)
  (global-set-key (kbd "C-M-}") 'winner-redo)

  ;; html
  (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") nil)
  (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") nil)
  (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") nil)
  (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") nil)
  (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") nil)
  (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") nil)

  ;; emacs-state
  (define-key evil-hybrid-state-map (kbd "TAB") nil)
  (define-key evil-emacs-state-map (kbd "TAB") nil)
  ;; undo tree
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  (define-key undo-tree-map (kbd "C-/") nil)

  )
