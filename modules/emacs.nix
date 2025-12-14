{ config, pkgs, ... }:
{
  home-manager.users.flu = {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        # Themes & UI
        catppuccin-theme
        doom-modeline
        all-the-icons-completion
        all-the-icons-dired
        all-the-icons-nerd-fonts
        
        # Completion & Navigation
        company
        vertico
        magit
        
        # Language support
        flycheck
        lsp-mode
        lsp-haskell
        lsp-pyright
        haskell-mode
        nix-mode
        rust-mode
        cargo-mode
        flycheck-rust
        
        # C/C++ support
        irony
        irony-eldoc
        company-irony
        company-irony-c-headers
        company-c-headers
        clang-format
        flycheck-clang-analyzer
        
        # Haskell extras
        company-ghci

        # Ocaml extras
        tuareg
        
        # Utilities
        chronos
        dired-git
        peep-dired
        highlight-indent-guides
        highlight-indentation
        ultra-scroll
        org
        
        # LaTeX
        latex-extra
        latex-math-preview
        latex-preview-pane
        pdf-tools
        
        # Music & Entertainment
        xkcd
        spotify
        smudge
        
        # Fun stuff
        gameoflife
      ];
    };
    
    home.file.".emacs".source = ./../emacs/.emacs;
  };
}
