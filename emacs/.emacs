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
	      highlight-indent-guides highlight-indentation irony
	      irony-eldoc lsp-haskell lsp-mode md4rd nlinum peep-dired
	      ultra-scroll))
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
(setq catppuccin-flavor 'mocha) ; or 'latte, 'macchiato, or 'mocha
(load-theme 'catppuccin t)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(setq column-number-mode t)

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

;; ------------- reddit ---------------
(setq md4rd-subs-active '(emacs askscience assholedesign archlinux C_Programming ChoosingBeggars community conlangs Cplusplus cpp csharp dataisbeautiful dating_advice embedded enschede Esperanto europe history homelab iamverysmart interestingasfuck InternetIsBeautiful IsItBullshit itsaunixsystem javascript languagelearning latin learndutch linux linuxmasterrace linuxquestions lisp LispMemes MaliciousCompliance mildlycarcinogenic mildlyinteresting movies opensource Physics ProgrammerHumor programming Romania rust science sex ShitAmericansSay theydidthemath))

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

(provide '.emacs)
;;; .emacs ends here
