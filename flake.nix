{
  description = "orangeFlu's systems";

  inputs = {
    # Unstable branch - get the latest and the newest
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
  let
    system = "x86_64-linux";
  in {
    nixosConfigurations = {
      # KDE variant
      laptop-kde = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./kde-configuration.nix
          home-manager.nixosModules.home-manager
        ];
      };

      # Hyprland variant
      laptop-hyprland = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./hyprland-configuration.nix
          home-manager.nixosModules.home-manager
        ];
      };
    };
  };
}

