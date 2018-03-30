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

(defun hook-prog-mode ()
  "Hook to run in 'prog-mode'."
  (column-marker-3 -1)
  (whitespace-mode -1)
  (flyspell-mode -1)
  (linum-mode 1)
  (flycheck-mode -1))

;; configuration
(defun toan/config-packages()

  ;; hook prog mod
  (add-hook 'LaTeX-mode-hook 'hook-prog-mode)
  (add-hook 'TeX-mode-hook 'hook-prog-mode)
  (add-hook 'tex-mode-hook 'hook-prog-mode)
  (add-hook 'prog-mode-hook 'hook-prog-mode)

  ;; mode-line setting
  (setq powerline-default-separator 'bar)

  ;; company
  (setq company-idle-delay 300)
  )

;; configure shortcut key
(defun toan/config-keys()
  (global-set-key (kbd "TAB") 'indent-for-tab-command)
  (global-set-key (kbd "C-M-/") 'helm-company)             ;; auto completion

  (global-set-key (kbd "C-x g") 'vc-status)

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

  )
