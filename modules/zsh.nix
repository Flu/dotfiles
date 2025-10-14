{ config, pkgs, ... }:
{
  programs.zsh.enable = true;
  users.users.flu.shell = pkgs.zsh;

  home-manager.users.flu = {
    home.stateVersion = "25.05";

    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autocd = true;
      autosuggestion.enable = true;
      history.size = 10000;

      shellAliases = {
        ls = "ls -l";
        update = "sudo nixos-rebuild switch --flake /etc/nixos#$(hostname)";
      };

      initContent = ''
        fastfetch
        '';

      oh-my-zsh = {
        enable = true;
        custom = "$HOME/.oh-my-zsh/custom/";
        theme = "lambda_adrian";
        plugins = [ "git" ];
      };
    };

    home.file."/home/flu/.oh-my-zsh/custom/themes/lambda_adrian.zsh-theme".source =
        ./../zsh/lambda_adrian.zsh-theme;
  };
}
