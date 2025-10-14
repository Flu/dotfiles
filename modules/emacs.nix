{ config, pkgs, ... }:
{
  home-manager.users.flu = {
    programs.emacs.enable = true;

    home.file.".emacs".source = ./../emacs/.emacs;
  };
}
