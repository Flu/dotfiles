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
    displayManager.greetd.enable = true;
  };

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  programs.waybar.enable = true;

  environment.systemPackages = with pkgs; [
    kitty
    waybar
    rofi
    grim
    slurp
    wl-clipboard
  ];
}
