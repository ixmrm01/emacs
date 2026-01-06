;; ----------------
;; Package setup
;; ----------------
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------
;; Helpful behavior
;; ----------------
(setq-default
  inhibit-startup-message t

  create-lockfiles nil

  backup-directory-alist '(("." . "~/.emacs.d/backups"))

  auto-save-default nil

  indent-tabs-mode nil

  show-paren-delay 0

  scroll-conservatively 100)

(show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(column-number-mode t)

(global-auto-revert-mode t)

(save-place-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

;; ----------------
;; Zenburn theme
;; ----------------
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; ----------------
;; Set initial frame size and position
;; ----------------
(defun my/set-initial-frame ()
  (let* ((base-factor 0.70)
	 (a-width (* (display-pixel-width) base-factor))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
	 (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

(setq frame-resize-pixelwise t)

(my/set-initial-frame)

;; ----------------
;; C / C++ mode
;; ----------------
(use-package cc-mode
  :ensure t
  :mode ("\\.h\\'" . c++-mode))

;; ----------------
;; Snippets
;; ----------------
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; ----------------
;; tab indent or complete
;; ----------------
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; ----------------
;; Completion
;; ----------------
(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 2.0)
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

;; ----------------
;; Eglot (LSP)
;; ----------------
(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"))))
