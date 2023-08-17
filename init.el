(setq-default inhibit-startup-message t          ; clean *scratch* buffer
              ring-bell-function 'ignore         ; no bell sounds
              org-support-shift-select nil       ; no shift-select in org mode
              scroll-conservatively 101          ; avoid recentering when scrolling far
              scroll-margin 5                    ; add margin when scrolling verticall
              recenter-positions '(5 bottom)     ; set re-centering positions
              line-spacing 0.15                  ; set default distance between lines
              use-short-answers t                ; yes/no -> y/n (confirmation prompts)
              tab-always-indent 'complete)       ; ?

(scroll-bar-mode -1)        ; disable visible scrollbar
(tool-bar-mode -1)          ; disable the toolbar
(tooltip-mode -1)           ; disable tooltips
(set-fringe-mode -1)        ; disable fringe
(menu-bar-mode -1)          ; disable the menu bar
(auto-save-visited-mode -1) ; no autosave in file
(auto-save-mode 1)          ; autosave enabled (in backup file)
(column-number-mode 1)      ; enable column indexing in modeline
(delete-selection-mode 1)   ; replace selection with typed text
(desktop-save-mode 1)       ; save current state (?)
(tab-bar-mode -1)           ; hide tab bar

(set-face-attribute 'default nil :height 145) ; set text size

;; set backup folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda ()
           (setq display-line-numbers-width-start t)
           ;; (setq display-line-numbers-width 3)
           (setq display-line-numbers-grow-only t) ; fix for help bug
           (display-line-numbers-mode 1))))

;; override some modes which derive from the above
(dolist (mode '(org-mode-hook messages-buffer-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


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

;; event/command history for buffers
(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
     ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done) 
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "M-s M-s") 'swiper-thing-at-point)
(global-set-key (kbd "M-s s") 'swiper-all)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(setq ivy-use-virtual-buffers t) ; don't know how this works

(use-package counsel
  :bind (("M-x" . counsel-M-x)
     ;; ("C-x b" . counsel-ibuffer)
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
;; M-x nerd-icons-install-fonts (optional)
(use-package all-the-icons)

(load-theme 'doom-gruvbox t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Work")
    (setq projectile-project-search-path '("~/Work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; icons in minibuffer
(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package minimap
  :config
  (setq minimap-window-location 'right))

;; multiple cursors
(use-package multiple-cursors)
(setq mc/always-run-for-all t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; reassign org commands in order to use M and M-S globally
;; S (shift) becomes s (super)
(use-package org
  :bind (("C-M-s-<left>" . org-decrease-number-at-point)
     ("C-M-s-<right>" . org-increase-number-at-point)
     ("C-s-<down>" . org-shiftcontroldown)
     ("C-s-<left>" . org-shiftcontrolleft)
     ("C-s-<return>" . org-insert-todo-heading-respect-content)
     ("C-s-<right>" . org-shiftcontrolright)
     ("C-s-<up>" . org-shiftcontrolup)
     ("C-<return>" . org-insert-heading-respect-content)
     ("M-s-<down>" . org-shiftmetadown)
     ("M-s-<left>" . org-shiftmetaleft)
     ("M-s-<return>" . org-insert-todo-heading)
     ("M-s-<right>" . org-shiftmetaright)
     ("M-s-<up>" . org-shiftmetaup)
     ("s-<down>" . org-metadown)
     ("s-<left>" . org-metaleft)
     ("s-<right>" . org-metaright)
     ("s-<up>" . org-metaup)))


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

;; only one dired instance
(use-package dired-single)
;; navigate with left and right arrows
(define-key dired-mode-map (kbd "<left>") 'dired-single-up-directory)
(define-key dired-mode-map (kbd "<right>") 'dired-single-buffer)

;; highlight and modify symbol
(use-package iedit)

;; better syntax highlighting 
(use-package tree-sitter)
(use-package tree-sitter-langs)

;; customize space/newline/tab whitespace
(setq whitespace-display-mappings
      '((space-mark 32 [32] [46])     ; space (custom)
        (space-mark 160 [164] [95])   ; non-breaking space (original)
        (newline-mark 10 [172 10])    ; newline (custom)
        (tab-mark 9 [124 9]))) ; tab (custom) [187 9] [92 9]

;; what whitespaces to show
(setq whitespace-style
      '(face tabs spaces newline indentation space-mark tab-mark newline-mark missing-newline-at-eof))

;; make tabs and newlines smaller
;; (custom-set-faces
;;  '(whitespace-newline ((t (:height 0.75))))
;;  '(whitespace-tab ((t (:height 0.75))))) 

(global-set-key (kbd "C-x w") 'whitespace-mode)

;; lsp
(use-package eglot)

;; indentation guides
(use-package highlight-indent-guides
  :ensure t
  :hook ((python-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'nil)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-dots)
)

;; tab indent version (old)
;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (setq indent-tabs-mode t)
;;     (setq python-indent-offset 4)
;;     (setq tab-width 4)
;;     (setq tabify-regexp "^\t* [ \t]+") ; tabify only leading spaces
;;     (setq forward-sexp-function nil) ; fix forward-sexp for python
;;     (tabify (point-min) (point-max))
;;     (save-buffer)
;;     (toggle-truncate-lines 1)
;;     (whitespace-mode)
;;     (tree-sitter-hl-mode 1)
;;     (company-mode 1)))


;; space indent version`(new)
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (setq forward-sexp-function nil) ; fix forward-sexp for python
    (untabify (point-min) (point-max))
    (toggle-truncate-lines 1)
    (whitespace-mode)
    (tree-sitter-hl-mode 1)
    (company-mode 1)))

;; treemacs
(use-package treemacs-all-the-icons)
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-load-theme "all-the-icons")
  (setq treemacs-text-scale -1.5)
  (setq treemacs-width 28))

(global-set-key (kbd "C-s-/") 'treemacs)

;; invalid svg fix for treemacs
(setq image-types (cons 'svg image-types))

;; code completion
(use-package company)
(setq company-selection-wrap-around t)
(setq company-idle-delay 0.15)
(setq company-require-match 'never)
(setq company-minimum-prefix-length 1) ;; 3

;; tab bar
(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-new-tab-to 'rightmost
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-button-relief 30
      tab-bar-show nil)


(fset 'select-line
      (kmacro-lambda-form [?\C-a ?\C-  ?\C-e] 0 "%d"))

(fset 'copy-line
      (kmacro-lambda-form [?\C-a ?\C-  ?\C-e ?\M-w] 0 "%d"))

(fset 'select-line-indent
      (kmacro-lambda-form [?\M-m ?\C-  ?\C-e] 0 "%d"))

(fset 'copy-line-indent
      (kmacro-lambda-form [?\M-m ?\C-  ?\C-e ?\M-w] 0 "%d"))

(fset 'select-word
   (kmacro-lambda-form [C-M-left ?\C-  C-M-right] 0 "%d"))

(fset 'copy-word
   (kmacro-lambda-form [C-M-left ?\C-  C-M-right ?\M-w] 0 "%d"))

(fset 'kill-buffer-other-window
   (kmacro-lambda-form [?\C-x ?o ?\s-k ?\C-x ?o] 0 "%d"))


(global-set-key (kbd "C-c l") 'select-line)
(global-set-key (kbd "C-c c l") 'copy-line)
(global-set-key (kbd "C-c C-l") 'select-line-indent)
(global-set-key (kbd "C-c c C-l") 'copy-line-indent)
(global-set-key (kbd "C-c w") 'select-word)
(global-set-key (kbd "C-c c w") 'copy-word)
(global-set-key (kbd "C-c C-k") 'kill-buffer-other-window)

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(use-package json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; copy/paste for macos
(global-set-key (kbd "M-c") 'undefined)
(global-set-key (kbd "M-v") 'undefined)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)

;; Global-local bindings (keep this at the end of the file)
;; use this to override any major mode defined keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-S-<left>") 'previous-buffer)
    (define-key map (kbd "M-S-<right>") 'next-buffer)
    (define-key map (kbd "M-S-<down>") 'tab-previous)
    (define-key map (kbd "M-S-<up>") 'tab-next)
    (define-key map (kbd "M-<left>") 'windmove-left)
    (define-key map (kbd "M-<right>") 'windmove-right)
    (define-key map (kbd "M-<up>") 'windmove-up)
    (define-key map (kbd "M-<down>") 'windmove-down)
    (define-key map (kbd "M-/") 'next-window-any-frame)
    (define-key map (kbd "s-k") 'kill-current-buffer)
    (define-key map (kbd "C-c k") 'kill-current-buffer)
    (define-key map (kbd "C-M-j") 'counsel-switch-buffer)
    (define-key map (kbd "<escape>") 'keyboard-escape-quit) map)
  "my-keys-minor-mode keymap")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override any major modes."
  :init-value t
  :lighter "my-keys")

(my-keys-minor-mode 1) ; activate it

(use-package goto-last-change)
(require 'goto-last-change)
(global-set-key (kbd "C-x /") 'goto-last-change)

