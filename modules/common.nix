{ config, pkgs, ... }:
{
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

  # Enable CUPS to print documents.
  services.printing.enable = true;

  nixpkgs.config.allowUnfree = true;

  # Static ip address on eth1
  # Temporary
  networking = {
    interfaces.eth1 = {
      ipv4.addresses = [{
        address = "192.168.10.3";
        prefixLength = 24;
      }];
    };
  };

  users.users.flu = {
    isNormalUser = true;
    description = "Adrian Fluturel";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    fastfetch
    wget
    curl
    git
    vlc
    alacritty
    python313
    irony-server
    nmap
    gcc
    clang
    tree
  ];

  system.stateVersion = "25.05";
}
