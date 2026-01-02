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

;; Zenburn theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Rust major mode
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))
;;  :init
;;  (setq rust-mode-treesitter-derive t))

;; C / C++ mode
(use-package cc-mode
  :ensure t)

;; Tree Sitter
;;(use-package tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Completion frontend
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

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

;; Snippets
(use-package yasnippet
  :ensure t
  :hook (afer-init . yas-global-mode))
;;  :config
;;  (yas-reload-all)
;;  (add-hook 'prog-mode-hook 'yas-minor-mode)
;;  (add-hook 'text-mode-hook 'yas-minor-mode))

;; Eglot configuration
(use-package eglot
  :ensure t
;;  :hook (rust-mode . eglot-ensure)
  :hook ((rust-mode c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer"))
               '((c++-mode c-mode) . ("clangd"))))
