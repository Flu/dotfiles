(require 'package)

;; Start Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable company mode globally 
(add-hook 'after-init-hook 'global-company-mode)

;; This prevents an error when sourcing package lists from gnu
(setq package-check-signature nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages nil)
 '(term-buffer-maximum-size 1024))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ----------- base ---------------
(require 'doom-modeline)
(setq doom-modeline-buffer-file-name-style 'truncate-except-project)
(doom-modeline-mode)

(setq catppuccin-flavor 'mocha) ; or 'latte, 'macchiato, or 'mocha
(load-theme 'catppuccin t)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(setq column-number-mode t)

(scroll-bar-mode -1)
(add-hook 'prog-mode-hook #'subword-mode)

;; Stop creating backup and autosave files.
(setq make-backup-files nil
      auto-save-default nil)

;; Accept 'y' and 'n' rather than 'yes' and 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; I typically want to use UTF-8.
(prefer-coding-system 'utf-8)

;; Enable highlighting of current line.
(global-hl-line-mode 1)

;; Improved handling of clipboard in GNU/Linux and otherwise.
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; ---------- irony-mode ----------
;; Enable irony-mode automatically for C/C++/ObjC
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; Use irony's completion functions
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ---------- company-mode ----------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Add irony backend to company
(require 'company-irony)
(add-to-list 'company-backends 'company-irony)

;; ---------- optional extras ----------
;; Better C headers completion
(require 'company-irony-c-headers)
(add-to-list 'company-backends 'company-irony-c-headers)

;; Use irony's Eldoc support for function signatures
(add-hook 'irony-mode-hook #'irony-eldoc)

;; For terminals, truncate the maximum lines it can show to make it faster
(defun less-lines-term-mode-hook ()
  "Custom `term-mode' behaviours."
  (setq term-buffer-maximum-size 1024))

(add-hook 'term-mode-hook 'less-lines-term-mode-hook)

;; ------------ keybindings ------------
;; You might want a convenient completion trigger
(define-key irony-mode-map (kbd "C-c C-d") 'irony-get-type)

; Compatibility shim: some packages still use pos-bol
(unless (fboundp 'pos-bol)
  (defun pos-bol (&optional n)
    "Return the position of the beginning of line N.
With no arg, N defaults to current line."
    (save-excursion
      (forward-line (or n 0))
      (point))))

;; ------------ flycheck --------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ---------- clang-format ------------
(require 'clang-format)
(add-hook 'c-mode-common-hook #'clang-format+-mode)
(defun my/clang-format-set-style (style)
  (interactive "Style: ")
  (setq clang-format-style style)
  (message "clang-format style set to %s" style))

(global-set-key (kbd "C-c c") 'my/clang-format-set-style)
(global-set-key (kbd "C-c f") 'clang-format-region)

; Use tabs or spaces depending on project
(defun my/linux-kernel-style ()
  (interactive)
  (setq indent-tabs-mode t
        c-basic-offset 8
        tab-width 8))

;; ---------- line numbers -----------
(global-display-line-numbers-mode)

;; --------- ultra-scroll ------------
(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; ------------- chronos --------------
(require 'chronos)

;; ------- kernel coding style --------
(setq c-default-style "linux")

;; --------- haskell mode -------------
(require 'haskell-mode)
(require 'lsp-mode)
(require 'lsp-haskell)
(require 'company)
(require 'flycheck)

(setq lsp-haskell-server-path "haskell-language-server-wrapper")

(add-hook 'haskell-mode-hook #'flycheck-mode)
(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'lsp)

(add-hook 'haskell-literate-mode-hook #'lsp)
(setq lsp-diagnostics-provider :flycheck)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

;; Optional: pretty symbols
(setq haskell-font-lock-symbols t)

;; Optional: use TAB for completion if you like manual triggers
;; (define-key haskell-mode-map (kbd "TAB") #'company-complete)

;; ---------- dired-git ----------------
(add-hook 'dired-mode-hook 'dired-git-mode)

;; -------------- magit ---------------
(require 'magit)

;; ------------ vertico ---------------
(require 'vertico)
(vertico-mode 1)

;; ------------- latex ----------------
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)
(latex-preview-pane-enable)

;; --------- Python mode with LSP ---------
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'company-mode)

;; Optional: Configure Python LSP server
(with-eval-after-load 'lsp-mode
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil  ; disable if too noisy
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled t
        lsp-pylsp-plugins-pylint-enabled nil))  ; enable if you want pylint

;; Optional: Better Python indentation detection
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;; Optional: Use pyright instead
(setq lsp-pyright-python-executable-cmd "python3")

;; ------------- pdftools ----------------
(pdf-tools-install)

;; ----------- eletric-mode --------------
(electric-pair-mode)

;; --------- OCaml mode with LSP ---------
(require 'tuareg)
(add-hook 'tuareg-mode-hook #'lsp)
(add-hook 'tuareg-mode-hook #'flycheck-mode)
(add-hook 'tuareg-mode-hook #'company-mode)

;; Also for interface files
(add-hook 'tuareg-mode-hook #'lsp)

;; ------------- move-text ---------------
(require 'move-text)
(global-set-key (kbd "M-<up> ") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(provide '.emacs)
;;; .emacs ends here
