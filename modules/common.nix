{ config, pkgs, lib, ... }:
let
  libunwindLinked = pkgs.symlinkJoin {
    name = "libunwind-links";
    paths = [ pkgs.libunwind ];
    postBuild = ''
      mkdir -p $out/lib
      ln -s ${pkgs.libunwind}/lib/libunwind.so $out/lib/libunwind.so.1
    '';
  };
in {
  imports = [];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Enable networking and set hostname
  networking.hostName = "david";
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  time.timeZone = "Europe/Amsterdam";
  i18n.defaultLocale = "ro_RO.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ro_RO.UTF-8";
    LC_IDENTIFICATION = "ro_RO.UTF-8";
    LC_MEASUREMENT = "ro_RO.UTF-8";
    LC_MONETARY = "ro_RO.UTF-8";
    LC_NAME = "ro_RO.UTF-8";
    LC_NUMERIC = "ro_RO.UTF-8";
    LC_PAPER = "ro_RO.UTF-8";
    LC_TELEPHONE = "ro_RO.UTF-8";
    LC_TIME = "ro_RO.UTF-8";
  };

  # Configure keymap
  services.xserver.xkb = {
    layout = "ro";
    variant = "";
  };

  xdg.portal = {
    enable = true;
  };
  
  # Enable CUPS to print documents.
  services.printing.enable = true;

  nixpkgs.config.allowUnfree = true;
  nix.settings.download-buffer-size = 512 * 1024 * 1024;
  
  users.users.flu = {
    isNormalUser = true;
    description = "Adrian Fluturel";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Other services
  hardware.bluetooth.enable = true;
  services.blueman.enable = true; # optional but very helpful GUI
  services.openssh.enable = true;

  # KDE connect
  programs.kdeconnect.enable = true;

  # Fix for using Principia mod in KSP
  environment.variables = {
    LD_LIBRARY_PATH = "/nix/store/mrsbbybinn24fpkz1zp401dyb16pwpx6-libcxx-21.1.2/lib:/nix/store/g40raprphg2a4m5zb7823rdgxyv6685n-libunwind-links/lib:$LD_LIBRARY_PATH";
  };
  
  environment.systemPackages = with pkgs; [
    rnnoise-plugin
    fastfetch
    wget
    curl
    pciutils
    git
    btop
    vlc
    alacritty
    (python313.withPackages (ps: with ps; [
      python-lsp-server
      pylsp-mypy
      python-lsp-black
      pyls-isort
      numpy
      pandas
      pip
      pyelftools
      scikit-learn
      matplotlib
      requests
      ipykernel
      pytesseract
      pillow
      thefuzz
      z3-solver
      textual
      osmnx
      (callPackage ./krpc.nix { })
    ]))
    tesseract
    pyright
    irony-server
    nmap
    gcc
    clang
    gnumake
    tree
    ascii
    texlive.combined.scheme-full
    vscode-fhs
    jetbrains.idea
    discord
    whatsapp-electron
    spotify
    qbittorrent
    gnupg
    gmp
    gimp
    pkg-config
    libreoffice
    syncplay
    steam
    dotnet-runtime_10
    ckan
    ollama-vulkan
    proton-vpn

    # For use with Principia mod for KSP
    libcxx libcxxrt libunwindLinked
  ];

  programs.steam.enable = true;

  services.ollama = {
    enable = true;
    package = pkgs.ollama-vulkan;
    
    environmentVariables = {
      # Replace "1" with the ID you found in vulkaninfo if it's different
      GGML_VK_VISIBLE_DEVICES = "1"; 
      # This ensures Vulkan is actually being requested as the runner
      OLLAMA_VULKAN = "1";
    };
  };
  
  home-manager.users.flu.home.sessionVariables = {
    PKG_CONFIG_PATH = "${pkgs.gmp.dev}/lib/pkgconfig";
    LD_LIBRARY_PATH = "";
  };

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    fira-code
    fira-code-symbols
    nerd-fonts.symbols-only
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
  ];

  system.stateVersion = "25.05";
}
