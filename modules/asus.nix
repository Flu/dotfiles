{ config, lib, pkgs, ... }:

{
  # Enable asusd service
  services.asusd = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
    asusctl
  ];
}
