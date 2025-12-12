{ config, lib, pkgs, ... }:

{
  # Enable asusd service
  services.asusd = {
    enable = true;
    enableUserService = true;
  };

  environment.systemPackages = with pkgs; [
    asusctl
  ];
}
