(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq org-support-shift-select nil)

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode -1) ; disable fringe
(menu-bar-mode -1)   ; disable the menu bar

(auto-save-visited-mode -1) ; no autosave in file
(auto-save-mode 1)          ; autosave enabled (in backup file)

(column-number-mode) ; enable column indexing in modeline

(set-face-attribute 'default nil :height 145) ; set text size

;; set backup folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; override some modes which derive from the above
(dolist (mode '(org-mode-hook
		messages-buffer-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers-width-start 1) ; set width equal to max(lines)

;; BINDINGS
(global-set-key (kbd "s-r") 'split-window-right)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-'") 'next-window-any-frame)
(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-/") 'next-window-any-frame)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; initialize package sources
(require 'package)

;; where to search for packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package ivy
  :diminish
  ;; :bind (("C-s" . swiper)
  ;;        :map ivy-minibuffer-map
  ;;        ("TAB" . ivy-alt-done)	
  ;;        ("C-l" . ivy-alt-done)
  ;;        ("C-j" . ivy-next-line)
  ;;        ("C-k" . ivy-previous-line)
  ;;        :map ivy-switch-buffer-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-l" . ivy-done)
  ;;        ("C-d" . ivy-switch-buffer-kill)
  ;;        :map ivy-reverse-i-search-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(setq ivy-use-virtual-buffers t) ; don't know how this works

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; don't start searches with ^

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(load-theme 'doom-gruvbox t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Work")
    (setq projectile-project-search-path '("~/Work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package minimap)

;; MULTIPLE CURSORS
(use-package multiple-cursors)
(setq mc/always-run-for-all t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package org) ; for updating org

;; add OS-specific customizations
(cond
 ; macos
 ((string= "darwin" system-type)
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (toggle-frame-fullscreen)
    (global-unset-key (kbd "s-c"))
    (global-unset-key (kbd "s-x"))
    (global-unset-key (kbd "s-v"))
    (global-unset-key (kbd "s-z"))))
 ; linux
 ((string= "gnu/linux" system-type)
  (progn ()))
 ; windows
 ((string= "windows-nt" system-type)
  (progn ())))

;; (use-package pdf-tools) ; doesn't work (make it work using its github)

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; configure eshell
(defun configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . configure-eshell))

;; dired

(use-package dired-single)

(define-key dired-mode-map (kbd "<left>") 'dired-single-up-directory)
(define-key dired-mode-map (kbd "<right>") 'dired-single-buffer)
;; (use-package dired
;;   :ensure nil
;;   :commands (dired dired-jump)
;;   :bind (("C-x C-j" . dired-jump))
;;   :config ((define-key dired-mode-map (kbd "<left>") 'dired-single-up-directory)
;; 	   (define-key dired-mode-map (kbd "<right>") 'dired-single-buffer)))

;; (use-package phi-search)

;; (global-set-key (kbd "C-s") 'phi-search)
;; (global-set-key (kbd "C-r") 'phi-search-backward)

(use-package iedit)
