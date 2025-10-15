{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/common.nix
    ./modules/zsh.nix
    ./modules/emacs.nix
  ];

  services = {
    xserver.enable = false;
    #displayManager.sddm.enable = true;
    #displayManager.sddm.wayland.enable = true;
  };

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # Use greetd as a secure, lightweight display manager
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --cmd Hyprland";
        user = "greeter";
      };
    };
  };

  programs.waybar.enable = true;

  environment.systemPackages = with pkgs; [
    hyprpaper
    alacritty
    kitty
    kdePackages.dolphin
    waybar
    rofi
    grim slurp wl-clipboard
    brightnessctl
    playerctl
    pavucontrol
  ];

  # Home Manager for user-level config files
  home-manager.users.flu = {
    home.stateVersion = "25.05";

    # Hyprland config
    home.file.".config/hypr/hyprland.conf".source = ./hyprland/hyprland.conf;

    # Hyprpaper
    home.file.".config/hypr/hyprpaper.conf".source = ./hyprland/hyprpaper.conf;

    # Waybar configs
    home.file.".config/waybar/config".source = ./hyprland/waybar/config;
    home.file.".config/wayba/style.css".source = ./hyprland/waybar/style.css;

    # Rofi config
    home.file.".config/rofi/config.rasi".source = ./hyprland/rofi/config.rasi;
  };

  # Enable polkit agent (needed for GUI apps like pavucontrol)
  security.polkit.enable = true;
  services.dbus.enable = true;

  # PipeWire sound
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  security.rtkit.enable = true;
}
