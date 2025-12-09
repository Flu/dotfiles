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
 '(package-selected-packages
   '(0blayout 0x0 0xc 2048-game all-the-icons-completion
	      all-the-icons-dired all-the-icons-nerd-fonts
	      catppuccin-theme chronos clang-format+ company-c-headers
	      company-ghci company-irony company-irony-c-headers
	      dired-git flycheck flycheck-clang-analyzer haskell-mode
	      flycheck-rust rust-mode cargo-mode
	      highlight-indent-guides highlight-indentation irony
	      irony-eldoc lsp-haskell lsp-mode nix-mode nlinum peep-dired
	      ultra-scroll magit vertico doom-modeline latex-extra latex-math-preview latex-preview-pane smudge))
 '(term-buffer-maximum-size 1024))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ensure all selected packages are installed
(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (ignore-errors
      (package-install pkg))))

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
  (interactive "sStyle: ")
  (setq clang-format-style style)
  (message "clang-format style set to %s" style))

(global-set-key (kbd "C-c s") 'my/clang-format-set-style)
(global-set-key (kbd "C-c f") 'clang-format-region)

; Use tabs or spaces depending on project
(defun my/linux-kernel-style ()
  (interactive)
  (setq indent-tabs-mode t
        c-basic-offset 8
        tab-width 8))

(defun my/personal-style ()
  (interactive)
  (setq indent-tabs-mode nil
        c-basic-offset 4
        tab-width 4))
(global-set-key (kbd "C-c a d r i a n") 'zone-sl)
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
;; --- Haskell setup ---
(require 'haskell-mode)
(require 'lsp-mode)
(require 'lsp-haskell)
(require 'company)
(require 'flycheck)

;; Ensure ghcup binaries are on PATH
(let ((ghcup-path (expand-file-name "~/.ghcup/bin")))
  (setenv "PATH" (concat ghcup-path ":" (getenv "PATH")))
  (add-to-list 'exec-path ghcup-path))

(setq lsp-haskell-server-path "/home/flu/.ghcup/hls/2.10.0.0/lib/haskell-language-server-2.10.0.0/bin/haskell-language-server-wrapper")
(add-hook 'haskell-mode-hook #'flycheck-mode)
(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'lsp)        ;; enable LSP + HLS
(add-hook 'haskell-literate-mode-hook #'lsp)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

;; Optional: pretty symbols
(setq haskell-font-lock-symbols t)

;; Optional: use TAB for completion if you like manual triggers
(define-key haskell-mode-map (kbd "TAB") #'company-complete)

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

;; ------------ smudge ---------------
(setq smudge-oauth2-client-secret "12963ce328034f28aeb3b1f16b716306")
(setq smudge-oauth2-client-id "1688938caefc4f45b2cdeac4015db5f0")
(setq smudge-transport 'connect)

(provide '.emacs)
;;; .emacs ends here
